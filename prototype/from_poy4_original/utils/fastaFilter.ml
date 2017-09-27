(* A program to filter the contents of a fasta file *)
let pattern = ref ""
let fasta = ref ""

let parse_list = [
    ("-pattern", (Arg.Set_string pattern), "The pattern to be searched");
    ("-fasta", (Arg.Set_string fasta), "The fasta file to be filtered");
]

let () = 
    (* Parse the arguments *)
    Arg.parse parse_list (fun _ -> ()) "fastaFilter [OPTIONS] fasta_file.fas";
    let file = 
        if !fasta = "" then
            Scripting.DNA.Fasta.of_channel true stdin
        else Scripting.DNA.Fasta.of_file true !fasta
    in
    let pattern = Str.regexp !pattern in
    let file = List.filter (fun (name, _) -> 
        Str.string_match pattern name 0) file 
    in
    Scripting.DNA.Fasta.to_channel stdout file
