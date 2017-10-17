let input_script = Sys.argv.(1)
let output_file = input_script ^ ".ss"

POY run ([input_script]) ;;


let data = Phylo.Runtime.data () 

let nodes = 
	let run = Phylo.get_console_run () in
	run.Scripting.nodes

let () =
	let ch = open_out output_file in
	let number_of_terminals = List.length nodes in
	let () = Printf.fprintf ch "dpread 'comment' %d %d\n" number_of_terminals number_of_terminals in
	let get_name taxonx = Data.code_taxon (Phylo.Node.taxon_code taxonx) data in
	let cnt = ref 0 in
let () = 
	List.iter (fun taxon -> Printf.fprintf ch "%s %d\n" (get_name taxon) !cnt; incr cnt) nodes
in
	let () = Printf.fprintf ch ";" in
	Printf.fprintf ch "\ncost [ 0 $ %d" number_of_terminals;
	List.iter (fun taxon1 ->
		Printf.fprintf ch "\n";
		List.iter (fun taxon2 ->
			let distance = (Phylo.Node.distance 0. taxon1 taxon2) in
			Printf.fprintf ch "%f " distance) nodes) nodes

(*
let [tree] = Phylo.Runtime.trees ();;

open Phylo.PhyloTree

let all_roots = fold_edges (fun acc edge ->
	let rerooted = reroot edge tree in
	let assigned = AllDirChar.M.assign_single rerooted in
	assigned :: acc) [] tree;;


let to_formatter tree = AllDirChar.M.to_formatter [] data tree

let () = 
	List.iter (PoyFormaters.trees_to_formater (Some "new_test_for_megan.txt") []) (List.map to_formatter all_roots)
*)
