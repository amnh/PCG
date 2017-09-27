(* A program to evaluate the change in the relative cost between a pair of trees
* before and after DO. *)

type mode = [ `Normal | `Exact | `Iterative ]

let initial_mode : mode ref = ref `Normal
let final_mode : mode ref = ref `Exact

let parallel = ref false

let build_method : [`Random | `Wagner ] ref = ref `Random

let gap_opening = ref 0

let substitution = ref 1

let indel = ref 1

let assign_tree_build_method str =
    match str with
    | "rnd" -> build_method := `Random
    | "wgn" -> build_method := `Wagner
    | str -> 
            failwith 
            "Illegal build method. Valid methods are rnd (random) and wgn (wagner)."

let assign_cost_calculation_mode item mode =
    item := 
        match mode with
        | "e" -> `Exact
        | "n" -> `Normal
        | "i" -> `Iterative
        | _ -> 
                failwith
                "Illegal tree cost modes. Valid modes are i (iterative), \
                n (normal), e (exhaustive)."

let repetitions = ref 100

let parse_list = [
    ("-initial-mode", (Arg.String (assign_cost_calculation_mode initial_mode)),
    "The cost calculation mode of the first tree cost estimation.i for \
    iterative, e for exhaustive, n for normal DO. The default is exhaustive.");
    ("-final-mode", (Arg.String (assign_cost_calculation_mode final_mode)),
    "The cost calculation mode of the final tree cost estimation. i for \
    iterative, e for exhaustive, n for normal DO. The default is exhaustive.");
    ("-tree-build", (Arg.String assign_tree_build_method), 
    "Select the method to build trees. rnd or wgn");
    ("-repetitions", (Arg.Int (fun x -> 
        if x >= 0 then repetitions := x)), 
    "The number of repetitions for the experiment. Default is 100.");
    ("-indel", Arg.Int (fun x -> indel := x), 
    "The cost of an indel. Default: 1");
    ("-subst", Arg.Int (fun x -> substitution := x),
    "The cost of a substitution. Default: 1");
    ("-gap_opening", Arg.Int (fun x -> gap_opening := x),
    "The cost of opening a gap. Default: 1")
]

let filenames : string list ref = ref []

let anon_fun str =
    filenames := !filenames @ [str]

let usage = "costFlip [OPTIONS] filename [filename ...]"

let () = Arg.parse parse_list anon_fun usage

module Nodes = AllDirNode.AllDirF
module Edges = Edge.LazyEdge
module TreeOps = AllDirChar.F
module CharOps = AllDirChar.CharScripting

module Parsimony = 
    Scripting.Make 
    (Nodes)
    (Edges) 
    (TreeOps)
    (CharOps)

let seed = 
IFDEF USEPARALLEL THEN
    (truncate (Unix.time ())) + (Mpi.comm_rank Mpi.comm_world)
ELSE
    truncate (Unix.time ())
END



let create_scripts () = 
    let initial_cost_step = (!initial_mode :> Methods.script) in
    let final_cost_step = (!final_mode :> Methods.script) in
    let build_step : Methods.script = 
        match !build_method with
        | `Random -> (`Build_Random (1, `First, [], `UnionBased None))
        | `Wagner -> 
                `Build (1, (`Wagner_Rnd (1, `First, [], `UnionBased None)), [])
    in
    (([initial_cost_step; build_step], [final_cost_step]) :
        (Methods.script list * Methods.script list))

let () = Random.init seed

IFDEF USEPARALLEL THEN
let repetitions = 
    let processes = Mpi.comm_size Mpi.comm_world in
    let res = (!repetitions) / processes in
    let rest = !repetitions - (res * processes) in
    res + (if rest < Mpi.comm_rank Mpi.comm_world then 1 else 0)
ELSE
let repetitions = !repetitions
END

let results =
    let tcm : Methods.script list = 
        match !gap_opening with
        | 0 -> 
                [`Create_Transformation_Cost_Matrix 
                (!substitution, !indel, `All)]
        | x -> 
                [`Create_Transformation_Cost_Matrix 
                (!substitution, !indel, `All); 
                `Assign_Affine_Gap_Cost (x, `All)]
    in
    let run = 
        let files = List.map (fun x -> `Remote x) !filenames in
        Parsimony.run ((`AutoDetect files) :: tcm) in
    let get_cost run =
        let tree = Sexpr.first run.Scripting.trees in
        Ptree.get_cost `Adjusted tree 
    in
    let begin_script, end_script = create_scripts () in
    Array.init repetitions (fun _ ->
        let run = Parsimony.run ~start:run begin_script in
        let starting_cost = get_cost run in
        let run = Parsimony.run ~start:run end_script in
        starting_cost, get_cost run) 
IFDEF USEPARALLEL THEN
let () =
        Mpi.barrier Mpi.comm_world;
        let results = Mpi.gather results 0 Mpi.comm_world in
        if 0 = Mpi.comm_rank Mpi.comm_world then
            Array.iter (Array.iter (fun (a, b) ->
                Printf.printf "%f\t%f\n" a b)) results
        else ()
ELSE
let () =
        Array.iter (fun (a, b) ->
            Printf.printf "%f\t%f\n" a b) results
END
