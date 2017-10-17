type label = String of (string * int) | Integer of int

let get_code = function String (_, mine) | Integer mine -> mine

let assign_labels tree =
    let cnt = ref (-1) in
    let rec assign_labels tree =
        match tree with
        | Parser.Tree.Leaf x -> 
                incr cnt;
                Parser.Tree.Leaf (String (x, !cnt))
        | Parser.Tree.Node (chld, _) ->
                let chld = List.map assign_labels chld in
                incr cnt;
                Parser.Tree.Node (chld, Integer !cnt)
    in
    assign_labels tree

let merge_paths left right code =
    let left_pass_back = Sexpr.map (Sexpr.union code) left
    and right_pass_back = Sexpr.map (Sexpr.union code) right in
    let all_connected = Sexpr.all_to_all Sexpr.union left right in
    let all_to_pass_back = Sexpr.union left_pass_back right_pass_back in
    all_connected, all_to_pass_back

let produce_paths tree =
    let rec generate_all_paths tree =
        match tree with
        | Parser.Tree.Leaf x ->
                let mine = get_code x in 
                let res = Sexpr.singleton (Sexpr.singleton mine) in
                `Empty, res
        | Parser.Tree.Node ([ch1; ch2], x) ->
                let mine = 
                    let mine = get_code x in
                    Sexpr.singleton mine
                in
                let ch1_ac, ch1_apb = generate_all_paths ch1 in
                let ch2_ac, ch2_apb = generate_all_paths ch2 in
                let all_connected = Sexpr.union ch1_ac ch2_ac in
                let new_all_connected, all_to_pass_back = 
                    merge_paths ch1_apb ch2_apb mine
                in
                Sexpr.union all_connected new_all_connected, all_to_pass_back
        | Parser.Tree.Node _ -> assert false
    in
    match tree with
    | Parser.Tree.Leaf _ -> []
    | Parser.Tree.Node ([ch1; ch2], _) ->
            let ch1_ac, ch1_apb = generate_all_paths ch1
            and ch2_ac, ch2_apb = generate_all_paths ch2 in
            let union = Sexpr.all_to_all Sexpr.union ch1_apb ch2_apb in
            Sexpr.to_list (Sexpr.union ch1_ac (Sexpr.union union ch2_ac))
    | _ -> assert false

let list_of_leaves tree = 
    let rec list_of_leaves acc tree =
        match tree with
        | Parser.Tree.Leaf x -> x :: acc
        | Parser.Tree.Node (lst, _) ->
                List.fold_left list_of_leaves acc lst 
    in
    List.map (function String x -> x | Integer _ -> assert false)
    (list_of_leaves [] tree)

let make_constraint leaves distance set =
    let leaves =
        Sexpr.to_list (Sexpr.filter (fun x -> All_sets.Integers.mem x leaves)
        set)
    in
    match leaves with
    | [x; y] ->
            let di = distance x y in
            (di, set, `Pair (x, y))
    | _ -> assert false

let produce_LP_instance treefile synonyms pairs triplets = 
    match Parser.Tree.of_file (`Local treefile) with
    | [[tree]] ->
            let costs, leaves, tree, rtree, leaf_dict =  (* Compute all the costs *)
                let rtree = 
                    let exists = 
                        let tmp =
                            List.fold_left (fun acc ((a, b), _) ->
                                All_sets.Strings.add b
                                (All_sets.Strings.add a acc))
                            All_sets.Strings.empty pairs
                        in
                        match triplets with
                        | None -> tmp
                        | Some triplets ->
                                List.fold_left (fun acc ((a, b, c), _) ->
                                    All_sets.Strings.add c
                                    (All_sets.Strings.add b
                                    (All_sets.Strings.add a acc)))
                                tmp triplets
                    in
                    let cnt = ref (-1) in
                    let res2 =
                    Parser.Tree.cleanup (fun name ->
                        let res = (name = "") || 
                            not (All_sets.Strings.mem name exists) in
                        if res then res
                        else 
                            let () = incr cnt in
                            res
                        ) tree
                    in
                    match res2 with
                    | None -> assert false
                    | Some res2 -> res2
                in
                let tree as rtree = assign_labels rtree in
                let leaves, tree = list_of_leaves tree, tree in
                let tbl = Hashtbl.create 1667 in
                let cleanup ((a, b), d) = 
                    Hashtbl.add tbl (List.assoc a leaves, List.assoc b leaves) d
                in
                List.iter cleanup pairs;
                tbl, 
                List.fold_left (fun acc x -> All_sets.Integers.add (snd x) acc)
                All_sets.Integers.empty leaves, tree, rtree, leaves
            in
            (* We compute the number of variables that will be involved *)
            let total_variables = 
                ((All_sets.Integers.cardinal leaves) * 2) - 3
            in
            (* We create the minimization problem, we want to reduce the total
            * sum of the variables, each assigned with a factor of one *)
            let minimization_problem = Array.make total_variables 1. in
            (* Now we compute the constraints, right now, those constraints are
            * only the all pairs distances *)
            let constraints = 
                match pairs with
                | [] -> []
                | _ ->
                        let paths = produce_paths tree in
                        List.map (make_constraint leaves 
                        (fun a b -> Hashtbl.find costs (a, b))) paths
            in
            let constraints = 
                match triplets with
                | None -> constraints
                | Some triplets ->
                    let index = Hashtbl.create 1667 in
                    List.iter (fun (_, set, item) -> Hashtbl.add index item set)
                    constraints;
                    let leafs = Array.of_list (All_sets.Integers.elements leaves) in
                    Array_ops.randomize leafs;
                    let create_triplet ((a, b, c) as tr, distance) =
                        let assoc x = List.assoc x leaf_dict in
                        let a = assoc a
                        and b = assoc b
                        and c = assoc c in
                        let find x y = 
                            try Hashtbl.find index (`Pair (x, y)) with
                            Not_found -> Hashtbl.find index (`Pair (y, x))
                        in
                        let set = Sexpr.union (find a b) (find b c) in
                        (distance, set, `Triple tr)
                    in
                    let rec make_triplets triplets acc =
                        match triplets with
                        | hd :: tl ->
                                let new_constraint = create_triplet hd in
                                make_triplets tl (new_constraint :: acc)
                        | [] -> acc
                    in
                    make_triplets triplets constraints
            in
            let a, b = 
                List.fold_left (fun (stacc, cacc) (c, sexp, _) ->
                    let cacc = (float_of_int c, float_of_int max_int) :: cacc in
                    let arr = Array.make total_variables 0. in
                    Sexpr.leaf_iter (fun code ->
                        if code = total_variables then ()
                        else arr.(code) <- 1.) sexp;
                    (arr :: stacc), cacc) ([], []) constraints
            in
            minimization_problem, Array.of_list a, Array.of_list b,
            Array.make total_variables (0., float_of_int max_int), rtree
    | _ -> assert false


let distances_file processor file =
    let res = ref [] 
    and ch = open_in file in
    try while true do
        res := (processor ch) :: !res
    done; assert false with End_of_file -> !res

let triplets_file find_synonym b = distances_file (fun ch -> 
    Scanf.sscanf (input_line ch) "%s\t%s\t%s\t%d" (fun a b c d -> 
        ((find_synonym a, find_synonym b, find_synonym c), d))) b

let duplets_file find_synonym b = distances_file (fun ch -> 
    Scanf.sscanf (input_line ch) "%s\t%s\t%d" (fun a b d -> ((find_synonym a,
    find_synonym b), d))) b

let rec print_tree tree =
    match tree with
    | Parser.Tree.Leaf str -> print_string str; print_string " "
    | Parser.Tree.Node (chld, str) ->
            print_string "(";
            List.iter print_tree chld;
            print_string ")";
            print_string str;
            print_string " "

let process_problem treefile synfile (pairs, triplets) = 
    let synonyms = 
        match synfile with
        | None -> Hashtbl.create 2
        | Some synfile ->
                let ch = open_in synfile in
                let res = Parser.Dictionary.of_channel ch in
                close_in ch;
                res
    in
    let syn x = try Hashtbl.find synonyms x with Not_found -> x in
    let triplets = 
        match triplets with
        | None -> None
        | Some triplets -> Some (triplets_file syn triplets) in
    let pairs = duplets_file syn pairs in
    let a, b, c, d, tree = 
        produce_LP_instance treefile synonyms pairs triplets 
    in
    let lp = Glpk.make_problem Glpk.Minimize a b c d in
    Glpk.set_message_level lp 0;
    Glpk.use_presolver lp true;
    Glpk.simplex lp;
    let res = Glpk.get_obj_val lp in
    let variables = Glpk.get_col_primals lp in
    (*
    let res = Array.fold_left (fun acc x -> acc +. (ceil x)) 0. variables in
    let tree = Parser.Tree.map (fun x ->
        let name, x =
            match x with
            | String x -> x
            | Integer x -> "", x
        in
        try name ^ ":" ^ string_of_float (ceil variables.(x)) with
        | _ -> name) tree in
    print_tree tree;
    *)
    ceil res

let rec make_pairs lst =
    match lst with
    | f :: s :: tl -> (f, Some s) :: make_pairs tl
    | [] -> []
    | _ -> failwith "Input files must come in pairs"

let tree = ref None
let use_triplets = ref false
let synonyms = ref None
let input_files = ref []

let () =
    let options = 
        [("-tree", (Arg.String (fun x -> tree := Some x)), "The tree file");
        ("-triplets", (Arg.Unit (fun () -> use_triplets := true)), 
        "Use triplets file");
        ("-synonyms", (Arg.String (fun x -> synonyms := Some x)), 
        "The synonyms file")] 
    in
    let usage = "./lower_bound [OPTION] input_file [input_file *]" in
    Arg.parse options (fun x -> input_files := x :: !input_files) usage

let () = 
    match !tree with
    | None -> ()
    | Some tree ->
            let res =
                let input_files = List.rev !input_files in
                let tl =
                    if !use_triplets then make_pairs input_files
                    else List.map (fun x -> (x, None)) input_files
                in
                List.fold_left (fun acc dist ->
                    acc +. (process_problem tree !synonyms dist)) 
                0. tl
            in
            print_float res;
            print_newline ()
