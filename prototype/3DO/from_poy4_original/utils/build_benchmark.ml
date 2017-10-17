(* A program to benchmark different algorithms of POY for various datasets.
*
* The algorithms to be evaluated include:
    *
    * Default build (1)
    * build (all, 1)
    * build (_mst, 1)
    * build (lookahead:2, 1)
    * build (lookahead:3, 1)
    * build (lookahead:4, 1)
    * build (all, lookahead:2, 1)
    * build (all, lookahead:4, 1)
    * build (all, lookahead:10, 1)
    * build (_mst, lookahead:2, 1)
    * build (_mst, lookahead:4, 1)
    * build (_mst, lookahead:10, 1)
    * build (all, _mst, lookahead:2, 1)
    * build (all, _mst, lookahead:4, 1)
    * build (all, _mst, lookahead:10, 1) *)

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

let process_file substitution gap_extension gap_opening file = 
    (* First prepare the data *)
    NPOY 
        wipe ()
        read ([file])
        transform (tcm:([substitution], [gap_extension]),
        gap_opening:[gap_opening]);
    let res =
        Array.map run_command_get_tree_cost
        [|(CPOY build (1) select (best:1));
        (CPOY build (all, 1) select (best:1));
        (CPOY build (_mst, 1) select (best:1));
        (CPOY build (lookahead:2, 1) select (best:1));
        (CPOY build (lookahead:3, 1) select (best:1));
        (CPOY build (lookahead:4, 1) select (best:1));
        (CPOY build (all, lookahead:2, 1) select (best:1));
        (CPOY build (all, lookahead:4, 1) select (best:1));
        (CPOY build (all, lookahead:10, 1) select (best:1));
        (CPOY build (_mst, lookahead:2, 1) select (best:1));
        (CPOY build (_mst, lookahead:4, 1) select (best:1));
        (CPOY build (_mst, lookahead:10, 1) select (best:1));
        (CPOY build (all, _mst, lookahead:2, 1) select (best:1));
        (CPOY build (all, _mst, lookahead:4, 1) select (best:1));
        (CPOY build (all, _mst, lookahead:10, 1) select (best:1))|]
    in
    let nres = Array.map (function None -> "NA" | Some (x, _) -> 
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

let file_pattern = ref ""
let substitution = ref 1
let gap_opening = ref 0
let gap_extension = ref 1

let parse_list = [
    ("-gap-opening", (Arg.Set_int gap_opening), "Gap opening");
    ("-indel", (Arg.Set_int gap_extension), "Indel");
    ("-substitution", (Arg.Set_int substitution), "Substitution");
    ("-file-pattern", (Arg.Set_string file_pattern), "Fasta file pattern");
]

let anon_fun x = failwith "No annonymous arguments"

let filter lst = 
    let pattern = Str.regexp !file_pattern in
    let rec aux res lst = 
        match lst with
        | [] -> List.rev res
        | h :: t -> 
                if Str.string_match pattern h 0 then
                    aux (h :: res) t
                else aux res t
    in
    aux [] lst

let () = 
    Arg.parse_argv Phylo.args parse_list anon_fun "build_benchmark [OPTIONS]";
    let master = 0 in
    let files =
        "./" 
        --> Sys.readdir 
        --> Array.to_list
        --> filter
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
