(* \chaptertitle{PdfDoc}{Document-level functions} *)
open Utility
open Pdf

(* \section{Types} *)

(* \intf The type of the four rotations of pages. This defines how a viewing
application (e.g Acrobat) displays the page. *)
type rotation =
  Rotate0 | Rotate90 | Rotate180 | Rotate270

(*\intf  A type representing a page. [content] is the list of objects
containing the graphical content stream (see the [Pdfpages] module), [mediabox]
the page size, [resources] the page's resource dictionary, [rotate] its rotation
and [rest] any other entries to reside in the page dictionary. *)
type page =
  {content : pdfobject list;
   mediabox : pdfobject;
   resources : pdfobject;
   rotate : rotation;
   rest : pdfobject} (*r A dictionary of the other records in the page. *)

(* Make a PDF rectangle from a [Paper.papersize]. *)
let rectangle_of_paper (u, w, h) =
  let w', h' =
    let f = Units.convert 100. u Units.PdfPoint in
      f w, f h
  in
    Array [Real 0.; Real 0.; Real w'; Real h']

(* \intf Create a page with empty content, media box from the given paper size,
empty resources, zero rotation and no extra dictionary entries. *)
let blankpage papersize =
  {content = [];
   mediabox = rectangle_of_paper papersize;
   resources = Dictionary [];
   rotate = Rotate0;
   rest = Dictionary []}

(* \section {Utilities} *)
  
(* \intf Utility function to convert from rotation to integers. *)
let int_of_rotation = function
  Rotate0 -> 0 | Rotate90 -> 90 | Rotate180 -> 180 | Rotate270 -> 270

(* \intf The reverse. raises [Pdf.PDFError] if its input modulo 360 is not 0, 90, 
180, 270, -90, -180 or -270. *)
let rotation_of_int i =
  match i mod 360 with
  | 0 -> Rotate0
  | 90 | -270 -> Rotate90
  | 180 | -180 -> Rotate180
  | 270 | -90 -> Rotate270
  | _ -> raise (PDFError "Bad /Rotate")

(* \section {Extracting the page tree} *)

(* \intf Given a page tree, find the first page resources, contents and
mediabox.  The resources and mediabox may be inherited from any node above in
the page tree. *)
let rec find_pages pages pdf resources mediabox rotate =
  match lookup_fail "/Type not in page dict" pdf "/Type" pages with
  | Name "/Pages" ->
      begin match
        lookup_fail "No /Kids in page tree" pdf "/Kids" pages
      with
      | Array kids ->
          let kids =
            map
              (function
               | Indirect k ->
                   (try Pdf.lookup_obj pdf k with
                     Not_found -> raise (PDFError "missing kid\n"))
               | _ -> raise (PDFError "malformed kid\n"))
              kids
          in
            let resources =
              match lookup_direct pdf "/Resources" pages with
              | Some x -> Some x
              | None -> resources
            and mediabox =
              match lookup_direct pdf "/MediaBox" pages with
              | Some x -> Some x
              | None -> mediabox
            and rotate =
              match lookup_direct pdf "/Rotate" pages with
              | Some (Integer r) -> rotation_of_int r
              | _ -> rotate
            in
              flatten
                (map
                  (fun k -> find_pages k pdf resources mediabox rotate)
                  kids)
      | _ -> raise (PDFError "Malformed /Kids in page tree node")
      end
  | Name "/Page" ->
      let resources =
        match lookup_direct pdf "/Resources" pages with
        | Some x -> Some x
        | None -> resources
      and mediabox =
        match lookup_direct pdf "/MediaBox" pages with
        | Some x -> Some x
        | None -> mediabox
      and contents =
        lookup_direct pdf "/Contents" pages
      and rotate =
        match lookup_direct pdf "/Rotate" pages with
        | Some (Integer r) -> rotation_of_int r
        | _ -> rotate
      in
        [{resources =
           (match resources with
           | Some r -> r
           | None -> raise (PDFError "Missing /Resources"));
         content =
           (match contents with
           | None -> []
           | Some (Array cs) -> map (direct pdf) cs;
           | Some pdfobject ->
               begin match direct pdf pdfobject with
               | Stream _ as stream -> [stream]
               | _ -> raise (PDFError "Bad /Contents")
               end);
         mediabox =
           (match mediabox with
           | Some m -> m
           | None -> raise (PDFError "Bad /MediaBox"));
         rotate = rotate;
         rest =
           fold_left remove_dict_entry pages
           ["/Resources"; "/Contents"; "/MediaBox"; "/Rotate"; "/Parent"; "/Type"]
        }]
  | _ -> raise (PDFError "find_pages: Not a page tree node or page object")

(* \intf Given a pdf, return a list of (resources, contents, mediabox) triples. *)
let pages_of_pagetree pdf =
  let document_catalog =
    try Pdf.lookup_obj pdf pdf.root with
      Not_found -> raise (PDFError "/Root entry is incorrect")
  in 
    let pages =
      lookup_fail "No or malformed /Pages" pdf "/Pages" document_catalog
    in
      find_pages pages pdf None None Rotate0

(* \intf Make a collection of pages capable of being merged -- in other words rename
their resources so as not to clash. *)
let source k =
  let k = ref k in (fun () -> incr k; !k)

let freshname source =
  "/r" ^ string_of_int (source ())

let resource_keys =
  ["/Font"; "/ExtGState"; "/ColorSpace";
   "/Pattern"; "/Shading"; "/XObject"; "/Properties"]

let make_changes pdf pages =
  let src = source 0 in
    let entries_of_page entry pageseq page =
      let entries =
        match Pdf.lookup_direct pdf entry page.resources with
        | Some (Pdf.Dictionary es) -> es
        | _ -> []
      in
        map (fun (k, v) -> entry, pageseq, k, freshname src) entries
    in
      let pagenums = ilist 1 (length pages) in
        let entries name =
          map2 (entries_of_page name) pagenums pages
        in
          let entries = flatten & flatten (map entries resource_keys) in
            let table = Hashtbl.create 10000 in
              iter
                (fun (entry, pageseq, k, name) ->
                   Hashtbl.add table (entry, pageseq, k) name)
                entries;
              table

let change_operator lookup seqnum = function
  | Pdfpages.Op_Tf (f, s) ->
      Pdfpages.Op_Tf (lookup "/Font" seqnum f, s)
  | Pdfpages.Op_gs n ->
      Pdfpages.Op_gs (lookup "/ExtGState" seqnum n)
  | Pdfpages.Op_CS n ->
      begin try Pdfpages.Op_CS (lookup "/ColorSpace" seqnum n) with
        _ -> Pdfpages.Op_CS n end
  | Pdfpages.Op_cs n ->
      begin try Pdfpages.Op_cs (lookup "/ColorSpace" seqnum n) with
        _ -> Pdfpages.Op_cs n end
  | Pdfpages.Op_SCNName (s, ns) ->
      Pdfpages.Op_SCNName (lookup "/Pattern" seqnum s, ns)
  | Pdfpages.Op_scnName (s, ns) ->
      Pdfpages.Op_scnName (lookup "/Pattern" seqnum s, ns)
  | Pdfpages.Op_sh s ->
      Pdfpages.Op_sh (lookup "/Shading" seqnum s)
  | Pdfpages.Op_Do x ->
      Pdfpages.Op_Do (lookup "/XObject" seqnum x)
  | Pdfpages.Op_DP (n, Name p) ->
      Pdfpages.Op_DP (n, Name (lookup "/Properties" seqnum p))
  | Pdfpages.Op_BDC (n, Name p) ->
      Pdfpages.Op_BDC (n, Name (lookup "/Properties" seqnum p))
  | x -> x

let renumber_pages pdf pages =
  match pages with
  | [] -> []
  | pages ->
      let changes = make_changes pdf pages in
        let rec lookup dictname page oldkey =
          try Hashtbl.find changes (dictname, page, oldkey) with
            Not_found -> raise (Pdf.PDFError "key failure")
        in
        let change_content seqnum resources content =
          let operators = Pdfpages.parse_operators pdf resources content in
            let operators' =
              map (change_operator lookup seqnum) operators
            in
              [Pdfpages.stream_of_ops operators']
        and change_resources seqnum resources =
          let newdict name =
            match Pdf.lookup_direct pdf name resources with
            | Some (Pdf.Dictionary fonts) ->
                Pdf.Dictionary
                  (map (fun (k, v) -> lookup name seqnum k, v) fonts)
            | _ -> Pdf.Dictionary []
          in
            let newdicts = map newdict resource_keys in
              let resources = ref resources in
                iter2
                  (fun k v ->
                    resources := Pdf.add_dict_entry !resources k v)
                  resource_keys
                  newdicts;
                !resources
        in
          let process_page seqnum page =
            {page with
               content = change_content seqnum page.resources page.content;
               resources = change_resources seqnum page.resources}
          in
            map2 process_page (indx pages) pages

(* \section{Adding a page tree} *)

(* New code for better page trees *)

(* Each branch contains a list of pages to go at that branch, and pointers to
two more page tree nodes.  Each leaf contains just a page list. Page lists must
be non-null.

Leaves and branches also hold a parent pointer, and the object number of that
leaf or branch. *) 
type ptree =
  | Lf of page list * int * int
  | Br of page list * ptree * ptree * int * int

(* Split a list into three equal-ish sized parts *)
let split3 l =
  match splitinto ((length l + 2) / 3) l with
  | [a; b; c] -> a, b, c
  | _ -> raise (Invalid_argument "split3")

(* Build the pages *)
let rec pagetree objnumsource pages parent =
  if length pages < 10 then Lf (pages, parent, objnumsource ()) else
    let left, this, right = split3 pages in
      let this_num = objnumsource () in
        let left_tree = pagetree objnumsource left this_num
        and right_tree = pagetree objnumsource right this_num in
          Br (this, left_tree, right_tree, parent, this_num)

(* Make a page. Returns, objectnumber, page pdfobject, extra objects to be added. *)
let mkpage getobjnum parent page =
  let content, extras =
    match page.content with
    | [] -> [], []  (*r Null Contents not allowed. *)
    | cs ->
       let indirects, objects =
          split
            (map
              (fun c ->
                 let i = getobjnum () in Indirect i, (i, c))
              cs)
        in
          [("/Contents", Array indirects)], objects 
  in
    let page =
      Dictionary
        ([("/Type", Name "/Page");
          ("/Parent", Indirect parent);
          ("/Resources", page.resources);
          ("/MediaBox", page.mediabox);
          ("/Rotate", Integer (int_of_rotation page.rotate))]
      @
        (match page.rest with
         | Dictionary d -> d
         | _ -> raise (PDFError "mkpage"))
      @ 
        content)
    in
      getobjnum (), page, extras

(* Build a list of objnum, pdfobject pairs from the ptree. The pages in the
ptree are just missing their parent entries, so we add those. *)
(*i let extras = ref [] i*)

let rec objects_of_ptree getobjnum extras = function
  | Lf (pages, parent, this) ->
      let page_objects =
        map
         (fun (o, p, x) -> extras =@ x; (o, p))
         (map (mkpage getobjnum this) pages)
      in
        let page_tree_node =
          let pdfobject =
            let parent_entry =
              if parent = 0 then [] else ["/Parent", Indirect parent]
            in
              Dictionary
                (["/Type", Name "/Pages";
                  "/Kids",
                     Array (
                       map (fun x -> Pdf.Indirect x) (fst & split page_objects));
                  "/Count", Integer (length pages)]
                 @ parent_entry)
          in
           this, pdfobject 
        in
          page_tree_node::page_objects
  | Br (pages, left, right, parent, this) ->
      let objs_left = objects_of_ptree getobjnum extras left
      and objs_right = objects_of_ptree getobjnum extras right in
        let left_num =
          match objs_left with
          | (n, _)::_ -> n
          | [] -> assert false
        and right_num =
          match objs_right with
          | (n, _)::_ -> n
          | [] -> assert false
        and count_left =
          match objs_left with
          | (_, Dictionary d)::_ ->
              begin match lookup "/Count" d with
              | Some (Integer i) -> i 
              | _ -> assert false
              end
          | _ -> assert false
        and count_right =
          match objs_right with
          | (_, Dictionary d)::_ ->
              begin match lookup "/Count" d with
              | Some (Integer i) -> i 
              | _ -> assert false
              end
          | _ -> assert false
        in
          let this_objects =
            let page_objects =
              map
               (fun (o, p, x) -> extras =@ x; (o, p))
               (map (mkpage getobjnum this) pages)
            in
              let page_tree_node =
                let pdfobject =
                  let parent_entry =
                    if parent = 0 then [] else ["/Parent", Indirect parent]
                  in
                    let kids = fst & split page_objects in
                      Dictionary
                        (["/Type", Name "/Pages";
                          "/Kids",
                             Array
                               (map
                                  (fun x -> Pdf.Indirect x)
                                  ([left_num] @ kids @ [right_num]));
                          "/Count", Integer (count_left + count_right + length kids)]
                         @ parent_entry)
                in
                 this, pdfobject 
              in
                page_tree_node::page_objects
           in
             this_objects @ objs_left @ objs_right
     
(* \intf Take a list of pages and a PDF. Build a page tree in the PDF, returning
the new pdf and the object number assigned to the top page node. All references
to objects not forming part of the tree nodes themselves are left unchanged. *)
let add_pagetree pages pdf =
  let extras = ref [] in
    let getobjnum = source (Pdf.maxobjnum pdf) in
      let ptree = pagetree getobjnum pages 0 in
        let objects = objects_of_ptree getobjnum extras ptree in
          let topnode = match hd objects with (n, _) -> n in
            let pdf = fold_left addobj_given_num pdf objects in
              let pdf = fold_left addobj_given_num pdf !extras in
                pdf, topnode

(* \intf Add a root entry, replacing the Type and Pages entry, and any entries
in [extras]. Preserves any entries in any existing root (e.g Metadata pointer). 
*)
let add_root pageroot extras pdf =
  let existing_entries =
    try
      match Pdf.lookup_obj pdf pdf.root with
      | Dictionary d -> d
      | _ -> []
    with
    _ -> []
  in
    let root =
      Pdf.Dictionary
        (fold_right (* Right so that /Type, /Pages overwrite *)
           (fun (k, v) d -> add k v d)
              ([("/Type", Pdf.Name "/Catalog"); ("/Pages", Pdf.Indirect pageroot)] @ existing_entries)
              extras)
    in
      let pdf, rootnum = Pdf.addobj pdf root in
        let trailerdict' =
          match pdf.Pdf.trailerdict with
          | Dictionary d -> Dictionary (add "/Root" (Pdf.Indirect rootnum) d)
          | _ -> raise (PDFError "add_root: bad trailer dictionary")
        in
          {pdf with
             Pdf.root = rootnum;
             Pdf.trailerdict = trailerdict'}

(* Return a new PDF containing everything the old one does, but with new pages.

Other objects (e.g destinations in the document outline) may point to the
individual page objects, so we must renumber these. We can only do this if the
number of pages are the same. We do this [if replace_numbers is true]. *)
let change_pages ?(change_references = true) basepdf pages' =
  let pdf = ref Pdf.empty in
    Pdf.objiter (fun k v -> pdf := Pdf.addobj_given_num !pdf (k, v)) basepdf;
    let old_page_numbers = Pdf.page_reference_numbers basepdf in
    let pdf, pagetree_num = add_pagetree pages' !pdf in
      let pdf =
        {pdf with
           Pdf.major = basepdf.Pdf.major;
           Pdf.minor = basepdf.Pdf.minor;
           Pdf.trailerdict = basepdf.Pdf.trailerdict}
      in
        let existing_root_entries =
          try
            match Pdf.lookup_obj basepdf basepdf.root with
            | Dictionary d -> d
            | _ -> []
          with
          _ -> []
        in
          let pdf = add_root pagetree_num existing_root_entries pdf in
            let new_page_numbers = Pdf.page_reference_numbers pdf in
              if change_references && length old_page_numbers = length new_page_numbers
                then
                  let changes = combine old_page_numbers new_page_numbers in
                    Pdf.objmap
                      (Pdf.renumber_object_parsed pdf (hashtable_of_dictionary changes))
                      pdf
                else
                  pdf

(* Ensure that there are no inherited attributes in the page tree --- in other
words they are all explicit. This is required before writing a file with
linearization *)
let pagetree_make_explicit pdf =
  let pages = pages_of_pagetree pdf in
    change_pages pdf pages

let _ =
  Pdfwrite.pagetree_make_explicit := pagetree_make_explicit

