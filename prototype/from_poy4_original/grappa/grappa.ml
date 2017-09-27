

(* read file of format:
    * >S1
    * 1 2 3 4 5
    * >S2
    * 1 -3 -2 4 5 
    * ...
    * S1 and S2 are the taxon names always start with a >
    * numbers are the traits  *)
type genome
type genome_arr



external c_get_num_genome : genome_arr -> int = "grappa_CAML_get_num_genome"
external c_get_num_gene : genome_arr -> int = "grappa_CAML_get_num_gene"


(* takes a genome and the number of genes and print it *)
external c_print_genome : genome -> int -> unit = "grappa_CAML_print_genome"

(* takes a genome and the number of genes and print it *)
external c_print_genome_arr : genome_arr -> int -> int -> unit = "grappa_CAML_print_genome_arr"


(*give index in array and returns the one genome at that index *)
external c_get_one_genome : genome_arr -> int -> genome = "grappa_CAML_get_one_genome"

(*  for the distance returned *)
external c_cmp_inv_dis : genome -> genome -> int -> int -> int =  "grappa_CAML_cmp_inv_dis"

let inversion_distance a b c d = 
    c_cmp_inv_dis a b c (if d then 1 else 0)


(* See [inversions] *)
external c_inversions : 
    genome -> genome -> int -> int -> (int * int) list  = 
        "grappa_CAML_inversions"

(** [inversions g1 g2 ngenes dist] produce a list of tuples, with a sequence of
* inversions that convert [g2] into [g1], with [ngenes] genes, and at inversion
* distance [dist]. *)
let inversions a b c d = List.rev (c_inversions a b c d)

external c_create_empty_genome_arr : int -> int -> genome_arr = "grappa_CAML_create_empty_genome_arr"

external c_set : genome_arr -> int -> int -> int -> unit = "grappa_CAML_set"

let genomes arr = 
    (* Create an array of genomes *)
    let total = Array.length arr in
    if total = 0 then c_create_empty_genome_arr 0 0
    else 
        let genes = Array.length arr.(0) in
        let res = c_create_empty_genome_arr total genes in
        let () = 
            Array.iteri (fun a chrom ->
                Array.iteri (c_set res a) chrom) arr
        in
        res

let genes arr = 
    let genomes = genomes arr in
    Array.init (Array.length arr) (c_get_one_genome genomes)
