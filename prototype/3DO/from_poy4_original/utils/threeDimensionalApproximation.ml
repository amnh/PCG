(* The first question we want to answer is: what is tthe distribution of all
* those medians? *)


let generate_list_array seq = 
    Array.init (Sequence.length seq) (fun x ->
        Cost_matrix.Two_D.list_of_bits (Sequence.get seq x) 33) 

let generate_random_sequences f number seq =
    let array = generate_list_array seq in
    let array = Array.map Array.of_list array in
    for i = 1 to number do
        let arr = 
            Array.map (fun x ->
                x.(Random.int (Array.length x)))
            array
        in
        let seq = Sequence.create (Sequence.capacity seq) in
        let _ =
            for i = (Array.length arr) - 1 downto 0 do
                Sequence.prepend seq arr.(i);
            done
        in
        f seq;
    done;
    ()

let report_cost cm dist c seq =
    let _, _, d, _ = Scripting.DNA.Align.algn c seq cm in
    Printf.printf "%d\t%d\n" dist d


let evaluate_distance samples cm a b c =
    let (_, _, dist, med) = Scripting.DNA.Align.algn a b cm in
    let _, _, best, _ = Scripting.DNA.Align.algn med c cm in
    Printf.printf "Best cost is %d\n" best;
    generate_random_sequences (report_cost cm dist c) samples med 

let ( --> ) a b = b a

let distance a b c =
    if a = b && b = c then 0
    else if a = b then 1
    else if b = c then 1 
    else if a = c then 1
    else 2


let matrix = ref [|[|[||]|]|]

let la = ref 0
let lb = ref 0 
let lc = ref 0 
let high_num = max_int / 2

let check_size a b c =
    if a > !la || b > !lb || c > !lc then begin
        la := a;
        lb := b;
        lc := c;
        matrix :=
            Array.init 2 (fun _ ->
                Array.init (!lb + 1) (fun _ ->
                    Array.make (!lc + 1) high_num));
        (!matrix).(0).(0).(0) <- 0;
    end else ()

let primitive_three_dimensional_alignment a b c =
    let la = Sequence.length a
    and lb = Sequence.length b
    and lc = Sequence.length c in
    check_size la lb lc;
    let ga p = Sequence.get a (p - 1)
    and gb p = Sequence.get b (p - 1)
    and gc p = Sequence.get c (p - 1) in
    let arr = !matrix in
    let gap = 16 in
    for j = 0 to lb do
        for k = 0 to lc do
            arr.(0).(j).(k) <- high_num;
            arr.(1).(j).(k) <- high_num;
        done;
    done;
    arr.(0).(0).(0) <- 0;
    for i = 1 to la do
        for j = 1 to lb do
            for k = 1 to lc do
                let i' = i mod 2 in
                let i'' = (i + 1) mod 2 in
                arr.(i').(j).(k) <-
                    ((arr.(i'').(j - 1).(k - 1) +
                    distance (ga i) (gb j) (gc k)) -->
                        min (arr.(i'').(j - 1).(k) +
                        (distance (ga i) (gb j) gap)) 
                        --> min (arr.(i'').(j).(k - 1) +
                        (distance (ga i) gap (gc k)))
                        --> min (arr.(i').(j - 1).(k - 1) +
                        (distance gap (gb j) (gc k)))
                        --> min (arr.(i').(j).(k - 1) + 
                        (distance gap gap (gc k)))
                        --> min (arr.(i').(j - 1).(k) +
                        (distance gap (gb j) gap))
                        --> min (arr.(i'').(j).(k) +
                        (distance (ga i) gap gap)));
            done;
        done;
        arr.(0).(0).(0) <- high_num;
    done;
    arr.(la mod 2).(lb).(lc)

let evaluate_best subst indel gap_opening cm a b c =
    let algn a b = 
        let _, _, a, b = Scripting.DNA.Align.algn a b cm in
        a, b
    in
    let distab, medab = algn a b
    and distbc, medbc = algn b c
    and distac, medac = algn a c in
    let distabc, _ = algn medab c
    and distbca, _ = algn medbc a
    and distacb, _ = algn medac b in
    let comp_three comp a b c = comp a (comp b c) in
    let _, _, _, upper = 
        Sequence.Align.align_3_powell a b c subst gap_opening indel
    in
    let lower = 
        comp_three min (distab + distabc) (distbc + distbca) (distac + distacb)
    in
    let approx = ((float_of_int lower) /. (float_of_int upper)) in
    if approx > 1.2 then
        Printf.printf "Bad case with %d and %d:\n%s\n%s\n%s\n" lower upper (Scripting.DNA.Seq.to_string
        a) (Scripting.DNA.Seq.to_string b) (Scripting.DNA.Seq.to_string c);
    Printf.printf "%d\t%d\t%d\t%d\t%d\t%f\n%!" distab distbc distac lower upper ((float_of_int lower) /. (float_of_int upper))


let print_samples = ref false

let random_selection subst indel gap_opening iterations subsamples seqs =
    let cm = 
        if gap_opening = 0 then Scripting.DNA.CM.of_sub_indel subst indel 
        else Scripting.DNA.CM.of_sub_indel_affine subst indel gap_opening
    in
    let arr = Array.of_list seqs in
    for i = (iterations - 1) downto 0 do
            Array_ops.randomize arr;
            if !print_samples then begin
                let _, _, d1, _ = Scripting.DNA.Align.algn arr.(0) arr.(1) cm in
                let _, _, d2, _ = Scripting.DNA.Align.algn arr.(0) arr.(2) cm in
                let _, _, d3, _ = Scripting.DNA.Align.algn arr.(1) arr.(2) cm in
                Printf.printf "All Pairs Distance: %d\t%d\t%d\n" d1 d2 d3;
                evaluate_distance subsamples cm arr.(0) arr.(1) arr.(2);
            end else
                evaluate_best subst indel gap_opening cm arr.(0) arr.(1) arr.(2);
    done;
    ()

let assign r v = r := v

let indel = ref 1
let subst = ref 1
let gap_opening = ref 0
let samples = ref 100
let input = ref []
let subsamples = ref 10

let () = 
    (* Upon initialization, we parse the command line *)
    Random.self_init ();
    let parse_list = [
        ("-indel", Arg.Int (assign indel), "Indel cost, 1 by default");
        ("-subst", Arg.Int (assign subst), "Substitution cost, 1 by default");
        ("-open", Arg.Int (assign gap_opening), "Gap opening cost, 0 by default");
        ("-samples", Arg.Int (assign samples), 
        "Number of samples to be performed, 100 by default");
        ("-subsamples", Arg.Int (assign subsamples), 
        "Number of subsamples to be performed (from the median), 10 by default");
        ("-printsamples", Arg.Unit (fun () -> print_samples := true), 
        "Print the distance to the samples of the medians stored in a pairwise \
        alignment");
    ] in
    let annon_fun (str : string) = 
        input := str :: !input
    in
    Arg.parse parse_list annon_fun "test_do [OPTIONS]* filename [filename]*" 

let () =
    let seqs = 
        let s = List.rev_map Scripting.DNA.Generic.molecular !input in
        let s = List.flatten s in
        (List.rev_map (fun (_, s) -> s) s) 
    in
    random_selection !subst !indel !gap_opening !samples !subsamples seqs
        (*
let split_in_all_possible_sequences seq = 
    let len = Sequence.length seq in
    let matrix = [Sequence.create len] in
    let process_position lst i =
        match Cost_matrix.Two_D.list_of_bits (Sequence.get seq i) 33 with
        | [h] ->
                List.iter (fun seq -> Sequence.prepend seq h) lst;
                lst
        | lst ->
                let lst_len = List.length lst in

        *)
