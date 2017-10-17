(* A program to benchmark different algorithms of POY for various datasets. *)

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

let process_file len substitution gap_extension gap_opening item file = 
    (* First prepare the data *)
    NPOY 
        wipe ()
        read ([file])
        transform(tcm:([substitution], [gap_extension]),
            gap_opening:[gap_opening]);
    let res =
        Array.map run_command_get_tree_cost
        [|(CPOY search (max_time:0:0:3) select (best:1))|]
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

let master = 0

let () = 
    Arg.parse_argv Phylo.args parse_list anon_fun "build_benchmark [OPTIONS]";
    if !file_pattern = "" then ()
    else
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
            --> (fun files ->
                let len = Array.length files in
                Array.mapi
                (process_file len !substitution !gap_extension !gap_opening)
                files)
        in
        let files = Mpi.gather files master Mpi.comm_world in
        if master = Mpi.comm_rank Mpi.comm_world then 
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
