(* A program to generate newick trees, not random, but balanced binary trees, of
* a certain length *)
let size = ref 8
let branch_length_variation = ref 0.
let branch_length = ref 0.1
let is_random = ref false
let () = Random.self_init ()

let asgn r verifier msg x = 
    if verifier x then r := x 
    else failwith msg

let generate_length () = 
    !branch_length +.
    (if !branch_length_variation = 0. then 0.0
    else 
        (Random.float (2.0 *. !branch_length_variation)) -.
        !branch_length_variation)

let dbg = false

let split_array len arr = 
    match arr with
    | [||] | [|_|] -> assert false
    | [|a; b|] -> [|a|], [|b|]
    | arr ->
            let pos = 
                if !is_random then
                    1 + (Random.int (len - 2))
                else len / 2
            in
            if dbg then Printf.printf "%d - %d\n%!" len pos;
            Array.sub arr 0 pos, Array.sub arr pos (len - pos)

let output_ancestors = ref false

let ancestor = 
    let cnt = ref 0 in
    fun () -> 
        incr cnt;
        "A" ^ string_of_int !cnt

let rec generator is_root arr =
    match arr with
    | [||] -> assert false
    | [|a|] -> 
            Printf.sprintf "%s:%f" a (generate_length ())
    | _ -> 
            let len = Array.length arr 
            and len1 = generate_length () in
            let f, s = split_array len arr in
            if is_root then
                if !output_ancestors then
                    Printf.sprintf "(%s, %s)%s" (generator false f) 
                    (generator false s) (ancestor ())
                else
                    Printf.sprintf "(%s, %s)" (generator false f) 
                    (generator false s) 
            else
                if !output_ancestors then
                    Printf.sprintf "(%s, %s)%s:%f" (generator false f) 
                    (generator false s) (ancestor ()) len1
                else
                    Printf.sprintf "(%s, %s):%f" (generator false f) 
                    (generator false s) len1

let create_array size = 
    Array.init size (fun x -> "T" ^ string_of_int x) 

let () =
    let params = [
        ("-random", (Arg.Unit (fun () -> is_random := true)),
        "Do not generate a balanced tree but a true random tree. By default the program produces a balanced tree.");
        ("-size", 
        (Arg.Int (asgn size (fun x -> x > 1) "-size must be greater than 1")), 
        "The number of terminals in the tree");
        ("-bl", (Arg.Float (asgn branch_length (fun x -> x >= 0.0) 
        "-bl must be greater than or equal to 0.0")), 
        "The length of the branches of the tree");
        ("-ancestors", (Arg.Unit (fun () -> output_ancestors := true)),
        "Assign lables to the ancestral vertices");
        ("-blv", (Arg.Float (asgn branch_length_variation (fun x -> x >= 0.0)
        "The -blv must be greater than or equal to 0.0")),
        "The variation of the branch length") ]
    in
    let usage = "randomTree [OPTIONS]\nA program to generate random trees." in
    Arg.parse params (fun _ -> ()) usage;
    print_endline (generator true (create_array !size))

