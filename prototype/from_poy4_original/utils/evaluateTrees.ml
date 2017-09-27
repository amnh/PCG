let split size arr = 
    let len = Array.length arr in
    let remainder = len - ((len / size) * size) in
    let fraction = len / size in
    Array.init size (fun pos ->
        if remainder = 0 then
            Array.sub arr (pos * fraction) fraction
        else if remainder > pos then
            Array.sub arr ((pos * fraction) + pos) 
            (fraction + 1)
        else 
            Array.sub arr ((pos * fraction) + remainder)
            fraction)

let trees = ref ""
let fasta = ref ""
let files = ref 0
let initial_file = ref 1
let substitution = ref 1
let indel = ref 2
let fasta = ref ""
let gap_opening = ref 0
let exact = ref false
let approximate = ref false

let () = 
    let options = [
        ("-trees", Arg.Set_string trees, "The pattern of the tree files. # \
        will be replaced with numbers from 1 to the value of argument n.");
        ("-fasta", Arg.Set_string fasta, "The pattern of the fasta files. # \
        will be replaced with numbers from 1 to the value of argument n.");
        ("-substitution", Arg.Set_int substitution, "Substitution cost (1)");
        ("-indel", Arg.Set_int indel, "Indel cost (1)");
        ("-gap_opening", Arg.Set_int gap_opening, "Gap opening (0)");
        ("-files", Arg.Set_int files, "Number of files to process. This \
        processes from 1 to n.");
        ("-initial", Arg.Set_int initial_file, "Initial file number");
        ("-exact", Arg.Unit (fun () -> exact := true), "Use iterative exact");
        ("-approx", Arg.Unit (fun () -> approximate := true), 
        "Use iterative approximate")
    ]
    in
    Arg.parse options (fun _ -> ()) "evaluateTrees [OPTIONS]"

let pattern = Str.regexp "#"

let process_trees () =
    let tree = !trees in
    let trees = Array.init !files (fun x -> x + !initial_file) in
    let master = 0 in
    let procs = Mpi.comm_size Mpi.comm_world in
    let trees = split procs trees in
    let my_trees = Mpi.scatter trees master Mpi.comm_world in
    let my_trees = 
        Array.map (fun file_integer ->
            let file_integer = string_of_int file_integer in
            let fasta = Str.global_replace pattern file_integer !fasta
            and tree = Str.global_replace pattern file_integer tree in
            NPOY 
                wipe ()
                read ([fasta])
                transform (tcm:([!substitution], [!indel]),
                    gap_opening:[!gap_opening])
                    read ([tree]);
            if !exact then
                (NPOY set (iterative:exact))
            else if !approximate then begin
                NPOY set (iterative:approximate);
            end;
            let cost =  
                match Phylo.Runtime.min_cost () with
                | None -> 0.0
                | Some cost -> cost 
            in
            cost) my_trees
    in
    Mpi.barrier Mpi.comm_world;
    let trees = Mpi.gather my_trees master Mpi.comm_world in
    if master = Mpi.comm_rank Mpi.comm_world then
        let cnt = ref 0 in
        for i = 0 to (Array.length trees) - 1 do
            for j = 0 to (Array.length trees.(i)) - 1 do
                incr cnt;
                let file = string_of_int !cnt in
                let cost = trees.(i).(j) in
                print_string
                (string_of_float cost ^ "\t" ^ 
                file ^ "\n")
            done;
        done
    else ()

let () =
    if !fasta <> "" && !trees <> "" then process_trees ();
    Mpi.finalize ()
