open Printf;;
let _ =
(*    print_string "Enter the file name";
    print_newline ();
    let infile = read_line () in *)
(*    let m = Grappa.c_read_input "../../Grappa/camp13_full" in *)
    let infile = "2genomeorderofmedian" in
    Gc.full_major ();
    print_newline ();
(*    let num_genomes, num_genes = Grappa.c_calc_num_genomes_genes infile in *)


    let m = Grappa.c_read_input infile 2 312 in 

    let num_genome = Grappa.c_get_num_genome m in
    let num_gene = Grappa.c_get_num_gene m in
    fprintf stdout "Number genomes: %i, number genes: %i" num_genome num_gene; print_newline ();
    Grappa.c_print_genome_arr m num_genome num_gene;
    Gc.full_major (); 
    print_endline "Ensure after reading input";


    let g0 = Grappa.c_get_one_genome m 0 in 
    Grappa.c_print_genome g0 num_gene;  
    
    let g1 = Grappa.c_get_one_genome m 1 in 
    Grappa.c_print_genome g1 num_gene;  

    let inv_dis = Grappa.c_cmp_inv_dis g0 g1 num_gene 0 in 
    fprintf stdout "Inversion distance: %i" inv_dis; print_newline ();
    Gc.full_major (); 
    print_endline "Ensure after getting one genome";  
    

