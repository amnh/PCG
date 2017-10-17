open Genlex

type 'a tree = 
    | Leaf of 'a 
    | Node of ('a tree * 'a tree * 'a)

let lexer = Genlex.make_lexer ["("; ")"; ":"; ","]

let rec parse_tree = parser
    | [< 'Kwd "("; t1 = internal_tree; 'Kwd ","; t2 =  internal_tree; 'Kwd ")";
'Ident root >] ->
            Node (t1, t2, (root, 0.0)) 
and internal_tree = parser
    [< leaf = node >] -> Leaf leaf
    | [< 'Kwd "("; t1 = internal_tree; 'Kwd ","; t2 =  internal_tree; 
    'Kwd ")"; info = node >] -> Node (t1, t2, info) 
and node = parser 
    [< 'Ident taxon; 'Kwd ":"; 'Float length >] -> (taxon, length)

let tree_cost distance tree =
    let rec aux tree =
        match tree with
        | Leaf node -> 0, node
        | Node (a, b, node) -> 
                let costa, a = aux a
                and costb, b = aux b in
                costa + costb + (distance node a) + (distance node b), node
    in
    fst (aux tree)

let distance fasta substitution gap_opening gap_extension =
    let cm = 
        Scripting.DNA.CM.of_sub_indel_affine substitution 
        gap_extension gap_opening
    in
    let fasta = Scripting.DNA.Fasta.of_file false fasta in
    fun (a, _) (b, _) ->
        let seqa = List.assoc a fasta
        and seqb = List.assoc b fasta in
        let _, _, cost, _ = Scripting.DNA.Align.algn seqa seqb cm in
        cost


let substitution_cost = ref 1
let indel_cost = ref 1
let gap_opening_cost = ref 0
let fasta = ref ""
let tree = ref ""
let files = ref 0

let () =
    let parse_list = [
        ("-substitution", Arg.Set_int substitution_cost, "Substitution [1]");
        ("-gap_opening", Arg.Set_int gap_opening_cost, "Gap opening [0]");
        ("-indel", Arg.Set_int indel_cost, "Indel [1]");
        ("-n", Arg.Set_int files, "The numbers of files to be processed (from 1 to n)");
        ("-fasta", Arg.Set_string fasta, "Fasta file pattern (# is replaced with a number)");
        ("-tree", Arg.Set_string tree, "Tree containing file pattern (see fasta)."); ]
    in
    Arg.parse parse_list (fun _ -> ()) "solutionTree [OPTIONS]"

let () =
    let pattern = Str.regexp "#" in
    if !tree = "" || !fasta = "" then ()
    else
        let run_number n =
            let n = string_of_int n in
            let tree = Str.global_replace pattern n !tree
            and fasta = Str.global_replace pattern n !fasta in
            let tree = 
                let channel = open_in tree in
                let stream = Stream.of_channel channel in
                let stream = lexer stream in
                parse_tree stream
            in
            let distance = 
                distance fasta !substitution_cost !gap_opening_cost
                !indel_cost
            in
            tree_cost distance tree
        in
        let arr = Array.init !files (fun x -> x + 1) in
        let arr = Array_ops.split (Mpi.comm_size Mpi.comm_world) arr in
        let arr = Mpi.scatter arr 0 Mpi.comm_world in
        let arr = Array.map run_number arr in
        let arr = Mpi.gather arr 0 Mpi.comm_world in
        let cnt = ref 0 in
        Array.iter (Array.iter (fun cost ->
                incr cnt;
                Printf.printf "%d\t%d\n" !cnt cost)) arr;
        Mpi.finalize ()
