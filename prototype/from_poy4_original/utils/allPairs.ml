 (* A program to produce the list with all the pairwise alignment costs *)
let gap_opening = ref 0
let indel = ref 1
let substitution = ref 1
let input_file = ref ""

let parse_list = [
    ("-gap-opening", (Arg.Set_int gap_opening), "Gap opening");
    ("-indel", (Arg.Set_int indel), "Indel");
    ("-substitution", (Arg.Set_int substitution), "Substitution");
]

let anon_fun x = input_file := x

let () = 
    (* Parse the arguments *)
    Arg.parse parse_list anon_fun "allPairs [OPTIONS] fasta_file.fas";
    (* Parse the FASTA file *)
    let file = 
        if !input_file = "" then
            Scripting.DNA.Fasta.of_channel false stdin
        else
            Scripting.DNA.Fasta.of_file false !input_file 
    in
    let cm = 
        Scripting.DNA.CM.of_sub_indel_affine !substitution !indel
        !gap_opening
    in
    let all_algnments = Scripting.DNA.Align.algn_all file cm in
    List.iter (List.iter (fun ((a, _), (b, _), c, _) ->
        Printf.printf "%s\t%s\t%d\n%!" a b c)) all_algnments
