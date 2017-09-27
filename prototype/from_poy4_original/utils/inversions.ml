let gen_inversion_distance a b circular =
    let sta, stb = UtlGrappa.standardize a b in
    let genes = Grappa.genes [|sta; stb|] in
    sta, stb, genes, Grappa.inversion_distance genes.(0) genes.(1) (Array.length sta) circular


let inversion_distance a b circular =
    let _, _, _, dist = gen_inversion_distance a b circular in
    dist


let inversion_events a b circular = 
    let sta, stb, genes, dist = gen_inversion_distance a b circular in
    dist, Grappa.inversions genes.(0) genes.(1) (Array.length a) dist 

let do_inversion a (x, y) =
    let genes = Array.length a in
    assert (x > 0);
    assert (y > 0);
    assert (x <= genes);
    assert (y <= genes);
    let x = x - 1
    and y = y - 1 in
    let source = ref (y + 1) in
    Array.init genes (fun pos ->
        if pos < x || pos > y then a.(pos)
        else 
            let () = decr source in
            (- a.(!source)))

let merge_concurrent lst = 
    (* We want to take a list of pairs, and group together those pairs that
    * could happen at the same time without affecting the result, that is, all
    * the pairs that belong to disjoint sets *)
    let intersect (a, b) (c, d) = 
        if a < c then
            (b - a) >= (c - a)
        else (d - c) >= (a - c)
    in
    let make_groups acc pair =
        match acc with
        | h :: t ->
                if List.exists (intersect pair) h then
                    [pair] :: acc
                else (pair :: h) :: t
        | [] -> [[pair]]
    in
    let groups = List.fold_left make_groups [] lst in
    List.rev_map List.rev groups

module Draw = struct
    type coordinate = int * int


    let quadrilat_coordinates (x, y) side height =
        [|(x, y); (x + side, y); (x + side, y + height); 
        (x, y + height); (x, y)|]

    let square_coordinates (x, y) side =
        quadrilat_coordinates (x, y) side side

    let square corner side = 
        Graphps.draw_poly (square_coordinates corner side)

    let quadrilat corner side height = 
        Graphps.set_color Graphps.magenta;
        Graphps.fill_poly
        (quadrilat_coordinates corner side height);
        Graphps.set_color Graphps.black

    let array table arr (x, y) side =
        Array.iteri (fun pos gene ->
            let x = x + (pos * side) in
            square (x, y) side;
            let todraw = 
                (if gene > 0 then "" else "~") ^
                Hashtbl.find table (abs gene) 
            in
            let w, h = Graphps.text_size todraw in
            Graphps.moveto 
            (x + (side / 2) - (w / 2)) (y + (side / 2) - (h / 2));
            Graphps.draw_string todraw) arr
end

let print_genes table (x, y) array side separation =
    let len = Array.length array in
    let y = ref (((len - 1) * side) + ((len - 1) * separation)) in
    for i = 0 to len - 1 do
        Draw.array table array.(i) (x, !y) side;
        y := !y - (side + separation);
    done

let print_rearrangements (x, y) rearrangements side separation =
    let len = Array.length rearrangements in
    let y = ref ((len * side) + ((len - 1) * separation)) in
    Array.iteri (fun pos rearrangements ->
        List.iter (fun (first, last) ->
            let first = first - 1 
            and last = last - 1 in
            Draw.quadrilat (x + (first * side), !y) (side * (1 + (last - first)))
            separation) rearrangements;
        y := !y - separation - side) rearrangements

let generate_transformations origin rearrangements = 
    let rearrangements = merge_concurrent rearrangements in 
    let _, modifications = 
        List.fold_left (fun (prev, acc) rearrangements ->
            let resulting_genome = 
                List.fold_left do_inversion prev rearrangements 
            in
            (resulting_genome, (resulting_genome :: acc))) 
            (origin, [origin]) rearrangements
    in
    Array.of_list (List.rev modifications), Array.of_list rearrangements

let print_inversions back_table a b circular file side separation = 
    let len = Array.length a in
    let _, rearrangements = inversion_events a b circular in
    let genomes, rearrangements = generate_transformations a rearrangements in
    let () = 
        match file with
        | None -> Graphps.open_graph ""
        | Some file -> 
                Graphps.open_ps file;
                Graphps.open_graph ("" ^ string_of_int ((2 + len) * side) ^ "x" ^
                string_of_int (((Array.length rearrangements) * separation) +
                ((2 + (Array.length rearrangements)) * side)) ^ "")

    in
    print_genes back_table (side, side) genomes side separation;
    print_rearrangements (side, side) rearrangements side separation;
    Graphps.close_graph ()


let to_array a b = 
    let initial_split string = Str.split (Str.regexp " ") string in
    let second_split string = Str.full_split (Str.regexp "~") string in
    let htbl = Hashtbl.create 97 in
    let back_htbl = Hashtbl.create 97 in
    let cnt = ref 0 in
    let add_text x = 
        try Hashtbl.find htbl x with
        | Not_found -> 
                incr cnt;
                Hashtbl.add htbl x !cnt;
                Hashtbl.add back_htbl !cnt x;
                !cnt
    in
    let a = List.map second_split (initial_split a) 
    and b = List.map second_split (initial_split b) in
    let process_item = function 
        | [Str.Delim "~"; Str.Text text] -> (- (add_text text))
        | [Str.Text text] -> add_text text
        | _ -> assert false
    in
    let a = List.map process_item a
    and b = List.map process_item b in
    back_htbl, Array.of_list a, Array.of_list b

let output_file = ref None
let separation_between_genomes = ref 60
let gene_width = ref 25
let sequences = ref []

let circular = ref false

let () = 
    let options = [
        ("-circular", (Arg.Unit (fun () -> circular := true)), 
        "Are the input chromosomes circular?");
        ("-output", (Arg.String (fun x -> output_file := Some x)), 
        "STRING The postscript file where the output image is generated");
        ("-inter-chromosome-space", (Arg.Int (fun x ->
            separation_between_genomes := x)), 
        "INT The number of points between the chromosomes in the image");
        ("-gene_width", (Arg.Int (fun x ->
            gene_width := x)), 
        "INT The number of points of width of each gene in the image")]
    in
    let usage = "./inversions [OPTIONS] \"chromosome1\" \"chromosome2\"" in
    let annon x = sequences := x :: !sequences in
    Arg.parse options annon usage

let () =
    match !sequences with
    | [a; b] ->
            let table, a, b = to_array a b in
            print_inversions table a b !circular !output_file !gene_width
            !separation_between_genomes 
    | _ -> 
            prerr_string "Illegal number of chromosomes. See inversions --help\n"
