let files = Array.to_list Sys.argv
let cnt = ref 1 

let output_compressed ch = 
    let lst = ref [] in
    let table = Lz.initial_table () in
    let output_binary_int ch int =
        let fst = (int land 0xFF00) lsr 8
        and snd = (int land 0xFF) in
        output_byte ch fst;
        output_byte ch snd
    in
    let flush () =
        List.iter (output_binary_int ch) (List.rev !lst);
        lst := [];
        flush ch;
    in
    let out string =
        lst := Lz.compress table string !lst
    in
    out,
    fun () -> 
        flush ();
        match !(table.Lz.state) with
        | None -> ()
        | Some x -> output_binary_int ch x

let output ch infile =
    Printf.printf "Working on file %d\n%!" !cnt;
    incr cnt;
    let t =
        let inch = open_in_bin infile in
        let t = Lz.detect_type inch in
        close_in inch;
        t
    in
    let out, flush = output_compressed ch in
    let inch = open_in_bin infile in
    let reader = new FileStream.compressed_reader t inch in
    let buffer = Buffer.create 1000 in
    let output () = 
        let str = Buffer.contents buffer in
        out str;
        flush ();
        Buffer.clear buffer;
    in
    try
        while true do 
            try
                for i = 0 to 1000 do
                    let ch = reader#getch in
                    Buffer.add_char buffer ch;
                done;
                output ();
            with
            | End_of_file as err ->
                    output ();
                    raise err
        done;
    with
    | End_of_file ->  ()

let () =
    match files with
    | _ :: outfile :: rest ->
            let ch = open_out_bin outfile in
            Lz.output_header Lz.latest ch;
            List.iter (output ch) rest
    | _ -> print_endline "concatenate outputfile inputfile inputfile ...."
