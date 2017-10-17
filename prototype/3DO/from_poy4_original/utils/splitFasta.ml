(** A program to split a FASTA file that is already separated using pounds or
* pipes. (POY format) *)
let split file = 
    try
        let seqs = Scripting.DNA.Fasta.multi_of_file file in
        let bas = Filename.chop_extension file in
        let cnt = ref (-1) in 
        List.iter (fun x ->
            incr cnt;
            Scripting.DNA.Fasta.to_file (bas ^ string_of_int !cnt ^ ".fas") x)
        seqs
    with 
    | err -> 
            Printf.fprintf stderr "Error in file %s\n%!" file

let input = ref []

let () =
    Arg.parse [] (fun x -> input := x :: !input) "splitFasta file*"

let () =
    List.iter split !input
