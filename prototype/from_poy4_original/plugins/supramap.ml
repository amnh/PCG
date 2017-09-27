(* A POY plugin that outputs a supramap file *)
open Phylo.Kml

let altitude_difference = 50000.

(* We first define how are we going to produce the ancestor of a pair of
* temporal and gis information *)
let ancestor a b =
    (* We extract the contents of the data structure to make the functions below
    * les verbose *)
    let agis = a.TemporalGIS.coordinates
    and bgis = b.TemporalGIS.coordinates 
    and adate = a.TemporalGIS.date
    and bdate = b.TemporalGIS.date in
    (* The altitude is the maximum of both plus the delta factor *)
    let altitude = 
        altitude_difference +.  (max agis.GIS.altitude bgis.GIS.altitude)
    in
    (* The horizontal location is just in between the vertices, but the altitude
    * is not in between them, instead is what we have chossen. *)
    let center = { GIS.center_points agis bgis with GIS.altitude = altitude } in
    (* The date will be the minimum between them *)
    let date = TemporalGIS.min_date adate bdate in
    (* We are ready to build the ancestor *)
    { TemporalGIS.coordinates = center; date = date }


(* Now we define a function to produce the tree with the general layout of
* supramap. *)
let rec adjust_tree tree =
    match tree with
    | Parser.Tree.Leaf _ -> tree (* We don't change the contents of a leaf *)
    | Parser.Tree.Node ([chld1; chld2], (node, gis)) ->
            (* We must first adjust the two children *)
            let chld1 = adjust_tree chld1
            and chld2 = adjust_tree chld2 in
            let gis_1 = KTree.extract_gis chld1
            and gis_2 = KTree.extract_gis chld2 in
            let ancestor = ancestor gis_1 gis_2 in
            Parser.Tree.Node ([chld1; chld2], (node, ancestor))
    | Parser.Tree.Node (_, _) -> 
            (* We just received a tree that is not binary. We can't handle this
            * case *)
            failwith "Non-binary intput tree"


(* Now we need to produce all the nice styles that will be used in
our supramap *)
let styles () =
    let supramap_url suffix =
        (* A function to prepend the correct URL to the resources of the styles
        * we define *)
        ("http://supramap.osu.edu/supramap/" ^ suffix) 
    in
    (* this is not really a valid XML because it has no root, but a collection
    * of documents. Therefore we start
    * the definition with -set instead of a plain - which produces an XML with
    * root *)
    (PXML -set
            -Style (id="00a") 
                -IconStyle -scale 0.35 -- 
                    -Icon -href { 
                        (* This example shows how to inject code to generate
                        * output. In this case, between {} I include OCaml code,
                        * that produces a string holding the url that needs to
                        * be printed *)
                        string supramap_url "images/circle.png" } --
                -- --
                -LabelStyle -scale 0 -- --
                -LineStyle -width 1.5 -- --
            --
            -Style (id="00b")
                -IconStyle -scale 0.75 -- 
                    -Icon -href { string supramap_url "images/circle.png" } --
                -- --
                -LabelStyle --
                -LineStyle -width 1.5 -- --
            -- 
            -Style (id="00c")
                -IconStyle -scale 0.95 -- 
                    -Icon -href { string supramap_url "images/circle.png" } --
                -- --
                -LabelStyle --
                -LineStyle -width 4 -- --
            --
            -StyleMap (id="00") 
                -Pair -key normal -- -styleUrl "#00a" -- --
                -Pair -key highlight -- -styleUrl "#00c" -- --
            --
            -StyleMap (id="01") 
                -Pair -key normal -- -styleUrl "#00b" -- --
                -Pair -key highlight -- -styleUrl "#00c" -- --
            --
        --)

(* Apply the function [contents] on the value if not None, otherwise produce 
* the empty XML. This is convenient if, for example, there is Some ancestor,
* or None at all *)
let optional contents v =
    match v with
    | None -> (PXML ---)
    | Some v -> contents v

(* A function to produce a row in a table of HTML *)
let do_row title contents =
    let c = 
        (* We must first extract the string representation of the contents of
        * the row. But the contents can not be structured data, it can only be a
        * simple value, so we match those cases, otherwise we will leave the
        * cell empty *)
        match contents with
        | #Xml.unstructured as contents -> Xml.value_to_string contents
        | _ -> ""
    in
    (* Now we create the row, aligned to the right *)
    (PXML 
        -tr -td (align=right) -font (size="+1") [string title] -- --
            -td -a (href=[string ("#pm_" ^ c)]) 
            [(contents :> Xml.xml Xml.contents)] -- -- --)

let make_pair (l, r) = 
    (* A function to generate a pair of rows, with the two descendants of a
    * vertex. *)
    (PXML 
        -set 
            {do_row "Descendant:" l} 
            {do_row "Descendant:" r} 
        --)

let family_summary_html topology node () = 
    (* We include the () at the end to produce a delayed type. 
    * You will see the note in the next function, [node_information] which
    * explains better what is the idea behind it. *)
    let ancestor = KTree.ancestor topology node
    and sister = KTree.sister topology node
    and children = KTree.children topology node in
    (PXML
        -table 
            -tr -td (colspan=2) (align=center)
                -b -font (size="+2") (color="#000000")
                    "Family Summary"
                -- --
            -- --
            { optional (do_row "Ancestor:") ancestor }
            { optional (do_row "Sister:") sister }
            { optional make_pair children } --)

let ( --> ) a b = b a
(* Now we are ready to define the functions to format the actual list of
* transformations *)

(* First we must be able to recognize if something is a transition, when
* comparing a pair of strings of transformations between nucleotides *)
let is_transition str1 str2 =
    match str1, str2 with
    | "A", "G"
    | "G", "A"
    | "C", "T"
    | "T", "C" -> true
    | _ -> false

(* If it is not a transition, then it's a transversion *)
let is_transversion str1 str2 = not (is_transition str1 str2)

(* Now the colors that we will use in the columns of the tables *)
let white = `String "#FFFFFF"
let gray = `String "#E1E1E1"

let string_of_states states =
    (* Given an XML like set of states consisting of a set of
    * <value>state</value>, concatenate them in a string separated only by
    * semicolons *)
    states
    --> Xml.children Xml.Characters.value 
    --> Sexpr.map (fun x -> x --> Xml.value --> Xml.value_to_string)
    --> Sexpr.to_list
    --> String.concat ";"

let type_of_event data name node_states ance_states =
    let name = Xml.value_to_string name in
    let code = Data.character_code name data in
    let is_nucleotide = 
        (* Most of the time the alphabet comes from POY directly,
        * but some times (for example in NEXUS files), it might be a
        * little bit strange, so we better simplify the alphabet to
        * the minimum expression and compare with it *)
        code 
        --> Data.get_alphabet data 
        --> Alphabet.to_sequential 
        --> Alphabet.to_list 
        --> List.map fst 
        --> (fun alph ->
            alph = ["A"; "C"; "G"; "T"] || 
                alph = ["A"; "C"; "G"; "T"; "-"])
    in
    let res =
        if is_nucleotide then
            if ance_states = "-" then "Ins"
            else if node_states = "-" then "Del"
            else if is_transition node_states ance_states then "Ti"
            else if is_transversion node_states ance_states then "Tv"
            else "ABC"
        else "ABC"
    in
    `String res

(* We define a function that outputs the HTML needed when comparing a pair of
* characters between the [node] and the [ancestor] of the node *)
let compare_character data node ancestor =
    let tag = Xml.tag node in
    assert (tag = (Xml.tag ancestor));
    let is_static_homology = 
        (* We should use the final states if dealing with one of the following
        * classes of characters, otherwise we should print the contents of the
        * Single states *)
        tag = Xml.Characters.nonadditive || 
        tag = Xml.Characters.additive || 
        tag = Xml.Characters.sankoff
    in
    let cost = Xml.attribute Xml.Characters.cost node in
    let definite = Xml.attribute Xml.Characters.definite node in
    if (`Bool true) = definite then
        (* We are only interested in the final and single clases *)
        let name = Xml.attribute Xml.Characters.name node in
        let clas = Xml.attribute Xml.Characters.cclass node in
        assert (clas = Xml.attribute Xml.Characters.cclass ancestor);
        if is_static_homology && ((`String Xml.Nodes.final) = clas) then
            let node_states = string_of_states node
            and ance_states = string_of_states ancestor in
            let type_of_event = 
                type_of_event data name node_states ance_states 
            in
            (* Time to produce the HTML row *)
            (PXML
            -tr -td (align=left) (bgcolor=[white]) { Xml.coherce name } --
                -td (align=left) (bgcolor=[gray]) -font (size="+1") 
                    { string ance_states } -- --
                -td (align=left) (bgcolor=[white]) -font (size="+1") 
                    { string node_states } -- --
                -td (align=left) (bgcolor=[gray]) 
                    { type_of_event } --
                -td (align=left) (bgcolor=[white]) 
                    { Xml.coherce cost } -- --)
        else (PXML ---)
    else (PXML ---)

let diagnosis_html data tree node () =
    (* At each vertex we will be printing the transformations between the vertex
    * and its ancestor, so we must first collect that information *)
    let ancestor = KTree.ancestor tree node in
    match ancestor with
    | None -> 
            (* We are at the root, we don't show any transforms *)
            (PXML ---)
    | Some ancestor ->
            (* OK, we are not at the root, lets get the phylogenetic information
            * of the node, and the ancestor of the node. *)
            let ancestor = 
                (*To get the ancestor phylogenetic information, it must 
                * not a more complex structured type (like a node in an XML
                * file), so we make sure its the case,
                * and get the node if indeed, otherwise the assertion fails *)
                match ancestor with
                | #Xml.unstructured as ancestor -> KTree.node tree ancestor
                | _ -> assert false
            and node = KTree.node tree node in
            (* Good, so we have to traverse the list of characters and do
            * accordingly *)
            let get_list_of_all_characters xml =
                xml 
                --> Xml.structured 
                --> Xml.eagerly_compute 
                --> Sexpr.to_list
            in
            let node_characters = get_list_of_all_characters node 
            and ance_characters = get_list_of_all_characters ancestor in
            let title color content = 
                (* A function to create a title *)
                (PXML -td (align=left) (bgcolor=[color]) 
                    -i { string content } -- --) 
            in
            (PXML 
                -table (border=0) (align=left) -font (color="#A4BFDB")
                    -tr -td (colspan=3) (align=left) -b -font 
                        (size="+2") (color="#000000") Transformations
                    -- -- --
                    -tr
                        { title white "Position" }
                        { title gray "Ancestor" }
                        { title white "Descendant" }
                        { title gray "Type" }
                        { title white "Cost" }
                    --
                    { set List.map2 (compare_character data) node_characters 
                    ance_characters }
                -- -- --)

let node_information data tree name =
    (PXML -text -body (bgcolor="#ffffff") 
        -table (width=400) 
            -tr 
                -td { 
                    (* In a previous example we produced a `String with the
                    * URL to be printed. This example is a little bit more
                    * complex, as it produces a `Delayed constructor, which
                    * contains a _function_ that should be executed when the
                    * time to print this part of the XML comes. In this way we
                    * don't hold things in memory and reduce dramatically memory
                    * consumption. The family summary is tiny, but the
                    * transforms, which is below, is much larger. *)
                    delayed (family_summary_html tree name) } -- 
                {if KTree.is_root tree name then (PXML ---) 
                else
                   (PXML -td 
                   { delayed (diagnosis_html data tree name) } --)}
            -- -- -- --)


(* We are now ready to produce the plugin, in this example we only generate one
* folder *)
let tree_folder = {
    KFile.name = "Tree";
    node_information = node_information;
    create_node = None; 
}

let plugin = {
    KFile.folders = [tree_folder];
    adjust_tree = adjust_tree;
    styles = styles;
}

(* We register it and ready to go baby! *)
let () = KFile.register_plugin "supramap" plugin

(* Now we define a plugin that dumps one character in a KMKL a file, nothing
* more. All the arguments are obligatory  *)
let dump_character arguments run =
    let usage = "kml (csv:STRING, name:STRING, file:STRING)" in
    let error_message = "Illegal command. Usage: " ^ usage in
    match arguments with
    | `List lst ->
            (* We sort the arguments to make it order independent *)
            let lst = List.sort compare lst in
            (match lst with
            | [ `Labled ("csv", `String csv);
            `Labled ("file", `String output_file);
            `Labled ("name", `String character) ] ->
                    let _ =
                        (CPOY 
                            select (characters, names:([character]))
                            report ([output_file], kml:(supramap, [csv]))
                            ) 
                        --> PoyCommand.of_parsed false 
                        --> Phylo.run ~start:run
                    in
                    run
            | _ -> failwith error_message)
    | _ -> failwith error_message

let () = Phylo.register_function "kml" dump_character
