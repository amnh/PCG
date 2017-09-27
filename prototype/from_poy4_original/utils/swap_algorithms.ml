(* A program to benchmark different algorithms of POY for various datasets.
*
* The algorithms to be evaluated include:
    * swap (_maxtrees:25000, spr)
    * swap (_maxtrees:25000, tbr)
    * swap (_maxtrees:25000, all, tbr)
    * swap (_maxtrees:25000, all, spr)
    * swap (_maxtrees:25000, sectorial:2)
    * swap (_maxtrees:25000, sectorial:3)
    * swap (_maxtrees:25000, sectorial:5)
    * swap (_maxtrees:25000, bfs:2)
    * swap (_maxtrees:25000, bfs:3)
    * swap (_maxtrees:25000, bfs:5)
    *
    * swap (annealing:(2%, 12))
    * swap (annealing:(2%, 25))
    * swap (annealing:(2%, 50))
    * swap (annealing:(2%, 100))
    * swap (annealing:(2%, 125))
    * swap (annealing:(2%, 250))
    * swap (annealing:(2%, 500))
    * swap (annealing:(2%, 1000))
    *
    * swap (annealing:(5%, 12))
    * swap (annealing:(5%, 25))
    * swap (annealing:(5%, 50))
    * swap (annealing:(5%, 100))
    * swap (annealing:(5%, 125))
    * swap (annealing:(5%, 250))
    * swap (annealing:(5%, 500))
    * swap (annealing:(5%, 1000))
    * 
    * swap (annealing:(10%, 12))
    * swap (annealing:(10%, 25))
    * swap (annealing:(10%, 50))
    * swap (annealing:(10%, 100))
    * swap (annealing:(10%, 125))
    * swap (annealing:(10%, 250))
    * swap (annealing:(10%, 500))
    * swap (annealing:(10%, 1000))
    * *) 


let () = 
    FileStream.do_broadcast := false


let run_command_get_tree_cost commands =
    Phylo.parsed_run (PoyCommand.of_parsed false commands);
    match Phylo.Runtime.trees () with
    | [tree] -> 
            let tree = 
                Phylo.PhyloTree.to_string false tree
                (Phylo.Runtime.data ())
            in
            (match tree with
            | [tree] -> Some (Phylo.Runtime.min_cost (), tree)
            | _ -> assert false)
    | [] -> assert false
    | _ -> assert false

let parse_line line = 
    let line = Str.split (Str.regexp ":") line in
    match line with
    | file :: _ :: _ ->
            let tree = List.hd (List.rev line) in
            file, tree
    | _ -> assert false

let process_tree tree = 
    (Phylo.PhyloTree.of_string tree 
    (Phylo.Runtime.data ()) 
    (Phylo.Runtime.nodes ()))

let process_file substitution gap_extension gap_opening line = 
    (* First prepare the data *)
    let file, tree = parse_line line in
    NPOY 
        wipe ()
        read ([file])
        transform (tcm:([substitution], [gap_extension]),
        gap_opening:[gap_opening]);
    let tree = process_tree tree in
    Phylo.Runtime.set_trees tree;
    let cost = 
        match Phylo.Runtime.min_cost () with
        | Some x -> x
        | None -> assert false
    in
    let five = cost *. 0.05
    and ten = cost *. 0.10
    and two = cost *. 0.02 in
    let res =
        let runit commd = 
            Phylo.Runtime.set_trees tree;
            run_command_get_tree_cost commd
        in
        Array.map runit [|
        CPOY swap (_maxtrees:25000, spr);
        CPOY swap (_maxtrees:25000, tbr);
        CPOY swap (_maxtrees:25000, all, tbr);
        CPOY swap (_maxtrees:25000, all, spr);
        CPOY swap (_maxtrees:25000, sectorial:2);
        CPOY swap (_maxtrees:25000, sectorial:3);
        CPOY swap (_maxtrees:25000, sectorial:5);
        CPOY swap (_maxtrees:25000, bfs:2);
        CPOY swap (_maxtrees:25000, bfs:3);
        CPOY swap (_maxtrees:25000, bfs:5);
        CPOY swap (_maxtrees:25000, randomized, spr);
        CPOY swap (_maxtrees:25000, randomized, tbr);
        CPOY swap (_maxtrees:25000, randomized, all, tbr);
        CPOY swap (_maxtrees:25000, randomized, all, spr);
        CPOY swap (_maxtrees:25000, randomized, sectorial:2);
        CPOY swap (_maxtrees:25000, randomized, sectorial:3);
        CPOY swap (_maxtrees:25000, randomized, sectorial:5);
        CPOY swap (_maxtrees:25000, randomized, bfs:2);
        CPOY swap (_maxtrees:25000, randomized, bfs:3);
        CPOY swap (_maxtrees:25000, randomized, bfs:5);|]
    in
    let nres = 
        Array.map (function None -> "NA" | Some (x, _) -> 
            match x with None -> "NA" | Some c -> string_of_float c) res 
    in
    let tree = snd (Array.fold_left (fun (best, tree) x ->
        match x with 
        | None -> (best, tree)
        | Some (Some cost, ntree) -> 
                if cost < best then (cost, ntree)
                else (best, tree)
        | Some (None, _) -> (best, tree)) (max_float, "") res)
    in
    file, nres, tree

let (-->) a b = b a 

let file_list = ref ""
let substitution = ref 1
let gap_opening = ref 0
let gap_extension = ref 1

let parse_list = [
    ("-gap-opening", (Arg.Set_int gap_opening), "Gap opening");
    ("-indel", (Arg.Set_int gap_extension), "Indel");
    ("-substitution", (Arg.Set_int substitution), "Substitution");
    ("-file-list", (Arg.Set_string file_list), "Fasta file pattern");
]

let anon_fun x = failwith "No annonymous arguments"

let file_to_list file =
    let res = ref [] in
    let ch = open_in file in
    try
        while true do
            res := (input_line ch) :: !res;
        done;
        assert false
    with
    | End_of_file -> 
            close_in ch;
            List.rev (!res)

let () = 
    Arg.parse_argv Phylo.args parse_list anon_fun "build_benchmark [OPTIONS]";
    let master = 0 in
    let files =
        !file_list
        --> file_to_list
        --> Array.of_list
        --> Array_ops.split (Mpi.comm_size Mpi.comm_world) 
    in
    let files = 
        Mpi.comm_world 
        --> Mpi.scatter files master
        --> Array.map 
            (process_file !substitution !gap_extension !gap_opening) 
    in
    let files = Mpi.gather files master Mpi.comm_world in
    if master = Mpi.comm_rank Mpi.comm_world then
        (* We print the results *)
        let printer x = Printf.printf "%s:" x in
        Array.iter (Array.iter (fun (file, res, tree) ->
                printer file; 
                printer (string_of_int !substitution);
                printer (string_of_int !gap_extension);
                printer (string_of_int !gap_opening);
                Array.iter printer res; 
                Printf.printf "%s\n" tree)) files
    else ();
    Mpi.finalize ()
