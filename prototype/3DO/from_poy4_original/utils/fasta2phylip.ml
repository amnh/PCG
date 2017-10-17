open Scripting.DNA 

let input_sequences = Fasta.of_channel true stdin

let taxa, characters, max_len = 
    let res =
        List.fold_left (fun acc (name, seq) ->
            match acc with
            | None ->  (* First element in the list, just process the input *)
                    Some (1, Seq.length seq, String.length name)
            | Some (taxa, characters, max_len) ->
                    if characters <> Seq.length seq then
                        failwith ("The terminal " ^ name ^ " has unqual length")
                    else
                        Some 
                        (taxa + 1, characters, max (String.length name) max_len)) 
        None input_sequences
    in
    match res with
    | None -> failwith "Empty file?"
    | Some res -> res

let print_name name = 
    let len = String.length name in 
    let max_len = if max_len < 10 then 10 else max_len + 1 in
    let dif = max_len - len in
    print_string name;
    for i = 1 to dif do
        print_string " "
    done

let print_taxon (name, seq) = 
    print_name name;
    print_endline (Seq.to_string seq)


let () = 
    Printf.printf "%d %d\n" taxa characters;
    List.iter print_taxon input_sequences


