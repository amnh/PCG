let rec to_phyloxml tree : Xml.xml =
    match tree with
    | Parser.Tree.Leaf name ->  (RXML -clade -"branch_length" { int 1 } -- -name { string name } -- -- )
    | Parser.Tree.Node (chld, name) ->
            (RXML -clade -"branch_length" { int 1 } -- -name { string name } --  
                { set List.map (fun x -> `Single (to_phyloxml x)) chld }
            --)

let phyloxml_phylogeny name tree : Xml.xml =
    (RXML -phylogeny (rooted="true") -name { string name } -- 
        { single to_phyloxml tree } --)

module Search = Ptree.Search (Phylo.Node) (Phylo.Edges) (Phylo.TreeOps)

let phylo_xml data trees : Xml.xml =
    let trees = 
        Sexpr.map 
        (fun t -> Search.build_tree t.Ptree.tree 
            (fun code ->
                try Data.code_taxon code data with
                | Not_found -> string_of_int code) (fun _ _ -> false) "root")
        trees
    in
    let trees = 
        let cnt = ref 0 in
        Sexpr.map (fun x ->
                incr cnt;
                phyloxml_phylogeny (string_of_int !cnt) x) trees
    in
    (RXML -phyloxml
        ("xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance")
        ("xsi:schemaLocation"=
            "http://www.phyloxml.org http://www.phyloxml.org/1.00/phyloxml.xsd")
        (xmlns="http://www.phyloxml.org") { (trees :> Xml.xml Xml.contents) } --)

let phyloxml_poy_function arguments run =
    match arguments with
    | `String filename ->
            let ch = open_out filename in
            let xml = phylo_xml run.Scripting.data run.Scripting.trees in
            output_string ch "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
            Xml.to_file ch xml;
            close_out ch;
            run
    | _ -> failwith "Usage: phyloxml (STRING), where the argument is the filename."

let () = Phylo.register_function "phyloxml" phyloxml_poy_function
