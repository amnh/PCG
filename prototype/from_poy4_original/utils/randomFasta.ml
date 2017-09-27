(* A program to generate random sequences in a fasta file *)
let () = Random.self_init ()

let int_to_dna int =
    let four = Big_int.big_int_of_int 4 in
    let rec print_base current_integer =
        let c, r = Big_int.quomod_big_int current_integer four in
        if Big_int.eq_big_int c Big_int.zero_big_int && 
        Big_int.eq_big_int r Big_int.zero_big_int then ()
        else
            let _ =
                match Big_int.int_of_big_int r with
                | 0 -> output_string stdout "A"
                | 1 -> output_string stdout "C"
                | 2 -> output_string stdout "G"
                | 3 -> output_string stdout "T"
                | _ -> assert false
            in
            print_base c
    in
    print_base int

let random_sequence taxon_name x =
    Printf.printf ">%s\n" taxon_name;
    let b = Buffer.create x in
    for i = x - 1 downto 0 do 
        match Random.int 10 with
        | 0 -> Buffer.add_string b "0"
        | 1 -> Buffer.add_string b "1"
        | 2 -> Buffer.add_string b "2"
        | 3 -> Buffer.add_string b "3"
        | 4 -> Buffer.add_string b "4"
        | 5 -> Buffer.add_string b "5"
        | 6 -> Buffer.add_string b "6"
        | 7 -> Buffer.add_string b "7"
        | 8 -> Buffer.add_string b "8"
        | 9 -> Buffer.add_string b "9"
        | _ -> assert false
    done;
    let int = Big_int.big_int_of_string (Buffer.contents b) in
    int_to_dna int;
    print_newline ();
    print_newline ()

let next_name pref cntr =
    incr cntr;
    pref ^ string_of_int !cntr

let assign x y = x := y

let seqs = ref 100
let length = ref 100
let pref = ref "taxon_"
let len_var = ref 0
let source = ref ""

let () =
    let parse_list = [
        ("-sequences", Arg.Int (assign seqs), 
        "The number of sequences to be generated. 100 by default");
        ("-length", Arg.Int (assign length), 
        "The minimum length of each sequence to be generated, 100 by default");
        ("-length-variation", Arg.Int (assign len_var),
        "The maximum length variation between sequences (picked uniformly at \
        random). The default is 0.");
        ("-of-file", Arg.String (assign source), 
        "Collect a random sample from the input file");
        ("-prefix", Arg.String (assign pref),
        "The prefix of the taxa to be generated") ]
    in
    Arg.parse parse_list (fun _ -> ()) "random [OPTIONS]*"

let () =
    let cnt = ref 0 in
    match !source with
    | "" ->
            for i = !seqs - 1 downto 0 do
                let length = 
                    if !len_var < 1 then !length
                    else !length + (Random.int !len_var)
                in
                random_sequence (next_name !pref cnt) length;
            done;
            ()
    | source -> 
            let src = 
                let src = Scripting.DNA.Generic.molecular source in
                let src = Array.of_list src in
                Array_ops.randomize src;
                src
            in
            for i = !seqs - 1 downto 0 do
                let (name, seq) = src.(i) in
                let seq = 
                    let seq = Scripting.DNA.Seq.to_string seq in
                    String.sub seq 0 ((String.length seq) - 1)
                in
                Printf.printf ">%s\n%s\n\n" name seq;
            done;
            ()
