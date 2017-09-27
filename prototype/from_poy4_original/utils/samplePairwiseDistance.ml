let () = Random.self_init ()

exception Center of Sequence.s

let do_pairs cm seqs =
    Array_ops.randomize seqs;
    let len = Array.length seqs in
    let center = seqs.(0) in
    for i = 1 to len - 1 do
        let _, _, dist, _ = Scripting.DNA.Align.algn seqs.(i) center cm in
        Printf.printf "%d\n%!" dist;
    done;
    ()

let assign r v = r := v
let input = ref []
let samples = ref 100
let gap_opening = ref 0
let subst = ref 1
let indel = ref 1 

let () =
    let parser_list = [
        ("-indel", Arg.Int (assign indel), "Indel cost, 1 by default");
        ("-subst", Arg.Int (assign subst), "Substitution cost, 1 by default");
        ("-open", Arg.Int (assign gap_opening), "Gap opening cost, 0 by default");
        ("-samples", Arg.Int (assign samples), 
        "Number of samples to be performed, 100 by default");
    ]
    in
    let annon_fun (str : string) = 
        input := str :: !input
    in
    Arg.parse parser_list annon_fun 
    "samplePairwiseDistance [OPTIONS]* filename [filename]*" 

let () =
    let seqs = 
        let s = List.rev_map Scripting.DNA.Generic.molecular !input in
        let s = List.flatten s in
        Array.of_list (List.rev_map (fun (_, s) -> s) s) 
    in
    let cm = 
        if !gap_opening = 0 then
            Scripting.DNA.CM.of_sub_indel !subst !indel 
        else Scripting.DNA.CM.of_sub_indel_affine !subst !indel !gap_opening
    in
    do_pairs cm seqs 
