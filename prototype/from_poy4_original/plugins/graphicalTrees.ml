(* This is a plugin to output the POY trees in svg or an html file with a VML
* (for internet explorer). Adds the commands svg ("filename") and vml
* ("filename"). *)

module type VG = sig
    type point = (int * int)
    val print_line : point -> point -> Xml.xml Xml.structured
    val print_text : point -> string -> Xml.xml Xml.structured
    val header : string
    val wrap_xml : Xml.xml Xml.structured -> Xml.xml 
    val group : string -> Xml.xml Xml.structured -> Xml.xml Xml.structured
    val depth_correction : int
    val height_correction : int
end

module SVG : VG = struct
    (* print a line between two points. *)
    type point = (int * int)

    let print_line (ox, oy) (dx, dy) =
        (PXML -line (stroke=black) ("stroke-width"=3) (x1=[int ox]) (y1=[int oy]) (x2=[int dx]) (y2=[int dy]) -- )

    (* print text in a point *)
    let print_text (x, y) text =
        (PXML -text (x=[int x]) (y=[int y]) ("font-size"=11) ("font-family"=Verdana) 
                { string text } --)

    let header = "<?xml version=\"1.0\" standalone=\"no\"?>\n<!DOCTYPE svg \
        PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \
            \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"

    let wrap_xml (contents : Xml.xml Xml.structured) : Xml.xml = 
        (RXML -svg 
                (width="100%") 
                (height="100%") 
                (version="1.1")
                (xmlns="http://www.w3.org/2000/svg")
                -desc "A phylogenetic tree in SVG generated from POY"--
                -g (id="AllTrees")
                    { Xml.coherce contents } -- --)
 
    let group id contents =
        (PXML -g (id=[string id]) { Xml.coherce contents } --)

    let depth_correction = 3
    let height_correction = 5
end


module VML : VG = struct
    type point = (int * int) 

    let print_line (ox, oy) (dx, dy) =
        (PXML -"v:line"
                (from=[string (string_of_int ox ^ "pt " ^ string_of_int oy ^
                "pt")])
                ("to"=[string (string_of_int dx ^ "pt " ^ string_of_int dy ^
                "pt")])
                --)

    let header = ""

    let print_text (x, y) text =
        let x = string_of_int x
        and y = string_of_int y in
        let position = 
            "position:absolute;top:" ^ y ^ "pt;left:" ^ x ^ "pt; width:100pt;height=30pt"
        in
        (PXML -"v:shape" 
                    (style=[string position]) 
                    -"v:textbox" { string text } -- --)

    let group id contents =
        contents

    let wrap_xml (contents : Xml.xml Xml.structured) : Xml.xml =
        let c2 = ((Xml.eagerly_compute contents) )  in
        let body : Xml.xml Xml.contents = 
            (PXML -body -style "v\\: * { behavior:url(#default#VML);display:inline-block }" --  
                -"xml:namespace" (ns="urn:schemas-microsoft-com:vml")
                (prefix="v") --
                { c2 } --)
        in
        ((RXML -html { body } --) : Xml.xml)
        
    let depth_correction = 0
    let height_correction = (0)

end

(* longitude of a branch *)
let branch_length = 30

(* separation between branches *)
let branch_distance = 20

module Make (VG : VG) = struct
(* How to print a tree. this function outputs a triple consisting of the xml of
* the tree, the coordinate at which the root of this tree is located, and the
* bottom coordinate where the last leaf is located *)
let rec print_tree ((depth, height) as coords) tree =
    match tree with
    | Parser.Tree.Leaf x -> 
            VG.group x ( VG.print_text (depth + VG.depth_correction, height +
            VG.height_correction) x ), coords, coords
    | Parser.Tree.Node ((h :: t), name) ->
                let next_point (x, y) = (x, y + branch_distance) in
                let xml, (x, y), max = 
                    print_tree (depth + branch_length, height) h
                in
                let xml = ( VG.print_line (depth, y) (x, y) ) :: [xml] in
                let (xml, (last_x, last_y), max) = 
                    List.fold_left (fun (xml, point, max) tree ->
                        let point = next_point max in
                        let nextxml, point, max = print_tree point tree in
                        let (x, y) = point in
                        (VG.print_line (depth, y) (x, y)) :: nextxml :: xml, point, max) 
                    (xml, (x, y), max) t
                in
                let xml = List.map Xml.eagerly_compute xml in
                let my_coordinate = (depth, ((y + last_y) / 2 )) in
                (VG.group name
                    (`Set
                    [ Xml.eagerly_compute (VG.print_line (depth, y) (depth,
                    last_y)); 
                    `Set xml ])), my_coordinate, max
    | _ -> assert false

(* A function to avoid temporary variables. This allows one to convert something
* like
* let tmp = f a in
* let tmp = g tmp in
* let tmp = h tmp in
* let tmp = i tmp in
* tmp
* into 
*
* a --> f --> g --> h --> i *)
let (-->) a b = b a 

module Search = Ptree.Search (Phylo.Node) (Phylo.Edges) (Phylo.TreeOps)

(* Given the data (which contains names of terminals, characters, and so), and a
* set of trees, represent them as a svg graphic *)
let svg data trees =
    let easy_tree tree = 
        (* This function produces a tree that is easy to traverse with pattern
        * matchin *)
        Search.build_tree tree.Ptree.tree 
            (fun code ->
                try Data.code_taxon code data with
                | Not_found -> string_of_int code) (fun _ _ ->
            false)  "root"
    in
    let trees = trees --> Sexpr.to_list --> List.map easy_tree in
    let (xml, pos) = 
        List.fold_left (fun (xml, pos) tree ->
            let new_xml, pos, (_, y) = print_tree pos tree in
            let new_xml = Xml.eagerly_compute new_xml in
            (new_xml :: xml), (30, y + 30)) ([], (30, 30)) trees
    in
    VG.wrap_xml (`Set xml)

(*  Dump the trees contained in run in the filename as a SVG image *)
let dump_svg filename run =
    let to_dump = svg run.Scripting.data run.Scripting.trees in
    let ch = open_out filename in
    output_string ch VG.header;
    Xml.to_file ch to_dump;
    close_out ch

end

module SvgTree = Make (SVG)
module VmlTree = Make (VML)

(* The functions that we will register in POY *)
let svg_function args run =
    match args with
    | `String filename -> SvgTree.dump_svg filename run; run
    | _ -> failwith "Usage: svg (STRING), where the string is the filename"

(* The functions that we will register in POY *)
let vml_function args run =
    match args with
    | `String filename -> VmlTree.dump_svg filename run; run
    | _ -> failwith "Usage: vml (STRING), where the string is the filename"

(* Ready to go! *)
let () = Phylo.register_function "svg" svg_function
let () = Phylo.register_function "vml" vml_function
