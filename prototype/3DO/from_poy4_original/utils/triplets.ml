(* A program to compute a set of triplets distances (sequences). *)
let arg = Mpi.init Sys.argv
let () = Random.self_init ()

let make_triplets arr =
    let res = ref [] in
    let len = Array.length arr in
    for i = 0 to len - 3 do
        res := (arr.(i), arr.(i + 1), arr.(i + 2)) :: !res;
    done;
    !res

let all_triplets arr = 
    let res = ref [] in
    let len = Array.length arr in
    for i = 0 to len - 3 do
        for j = i + 1 to len - 2 do
            for k = j + 1 to len - 1 do
                res := (arr.(i), arr.(j), arr.(k)) :: !res;
            done;
        done;
    done;
    !res

let compute_triplet toseq tcm tcm3 ((a, b, c) as triple) =
    let a_s = toseq a
    and b_s = toseq b 
    and c_s = toseq c in
    (triple, 
    (snd (Sequence.Align.align_3_powell_inter a_s b_s c_s tcm tcm3)))

let rank = Mpi.comm_rank Mpi.comm_world
let worldsize = Mpi.comm_size Mpi.comm_world

let tip_to_tip arr =
    let len = Array.length arr in
    let half = len / 2 in
    let f = Array.sub arr 0 half
    and s = Array.sub arr half (len - half) in
    let s = Array.of_list (List.rev (Array.to_list s)) in
    let res = ref [] in
    for i = 0 to (Array.length f) - 1 do
        res := f.(i) :: s.(i) :: !res;
    done;
    if Array.length s > Array.length f then res := s.(half) :: !res;
    List.rev !res

let terminals_arr seqs treefile =
    if treefile = "" then
        let terminals_arr = Array.of_list (List.map fst seqs) in
        Array_ops.randomize terminals_arr;
        terminals_arr
    else
        match Parser.Tree.of_file (`Local treefile) with
        | [[tree]] ->
                let queue = Queue.create () in
                Queue.push tree queue;
                let res = ref [] in
                while not (Queue.is_empty queue) do
                    match Queue.pop queue with
                    | Parser.Tree.Leaf str -> res := str :: !res
                    | Parser.Tree.Node (chld, _) ->
                            List.iter (fun x -> Queue.push x queue) chld
                done;
                let res = List.rev !res in
                let other = tip_to_tip (Array.of_list res) in
                Array.of_list (res @ other)
        | _ -> failwith "We can only accept one tree per input file, no forest"


let compute_triplets treefile file sub indel affine =
    let tcm = Scripting.DNA.CM.of_sub_indel_affine sub indel affine in
    let seqs = 
        let seqs = 
            if rank > 0 then []
            else
                let seqs =  Scripting.DNA.Fasta.of_file false file in
                List.map (fun (a, b) -> a, Scripting.DNA.Seq.to_string
                (Sequence.select_one b tcm)) seqs
        in
        Mpi.broadcast seqs 0 Mpi.comm_world 
    in
    let seqs = List.map (fun (a, b) -> a, Scripting.DNA.Seq.of_string b) seqs in
    let index = Hashtbl.create 1667 in
    List.iter (fun (taxon, seq) -> Hashtbl.add index taxon seq) seqs;
    let tcm3 = Cost_matrix.Three_D.of_two_dim tcm in
    let compute_triplet = compute_triplet (Hashtbl.find index) in
    let terminals_arr = terminals_arr seqs treefile in
    let triplets = make_triplets terminals_arr in
    let triplets = Array.of_list triplets in
    let compute_my_part () =
        let res = ref [] in
        let mylen = ((Array.length triplets) / worldsize) * worldsize in
        for i = 0 to mylen - 1 do
            if 0 = (i - rank) mod worldsize then
                res := (compute_triplet tcm tcm3 triplets.(i)) :: !res;
        done;
        Array.of_list !res
    in
    let triplet = compute_my_part () in
    let triplets = Mpi.gather triplet 0 Mpi.comm_world in
    if 0 = rank then
        Array.iter (Array.iter (fun ((a, b, c), dist) ->
            Printf.printf "%s\t%s\t%s\t%d\n%!" a b c dist))
        triplets
    else ()

let gap_opening = ref 0
let indel = ref 1
let substitution = ref 1
let input_file = ref ""
let tree = ref ""

let parse_list = [
    ("-gap-opening", (Arg.Set_int gap_opening), "Gap opening");
    ("-indel", (Arg.Set_int indel), "Indel");
    ("-substitution", (Arg.Set_int substitution), "Substitution");
    ("-tree", (Arg.Set_string tree), "The guide tree");
]

let anon_fun x = input_file := x

let () = Arg.parse_argv arg  parse_list anon_fun "triplets [OPTIONS] fasta_file.fas"

let () = 
    match !input_file with
    | "" -> exit 1
    | _ -> 
            let () = compute_triplets !input_file !substitution !indel
            !gap_opening in
            Mpi.finalize ()
