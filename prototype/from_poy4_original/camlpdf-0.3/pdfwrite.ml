(* \chaptertitle{PDFWrite}{Flattening PDF} *)
open Utility
open Io

let print_ints is =
  iter (fun x -> print_int x; print_string " ") is;
  print_newline ()

(* Flatten a PDF data structure to an output. The specification suggests
restricting lines to 255 characters for compatibility with very old PDF
software; we don't currently do this. *)

(* \section{Utilities} *)

(* Renumber a PDF's objects to [1]\ldots [n]. *)

(* Calculate the substitutions required to renumber the document. *)
let changes pdf =
  let card = Pdf.objcard pdf in
    let order = ilist_fail_null 1 card
    and change_table = Hashtbl.create card in
      List.iter2 (Hashtbl.add change_table) (Pdf.objnumbers pdf) order;
      change_table
      
(* \section{Header and Cross-reference table.} *)

(* The file header. We include four larger-than-127 bytes as requested by the
standard to help FTP programs distinguish binary/text transfer modes. *)
let header pdf =
  "%PDF-" ^
  string_of_int pdf.Pdf.major ^
  "." ^
  string_of_int pdf.Pdf.minor ^
  "\n%\128\129\130\131\n"

(* Build an cross-reference table string. *)
let pad_to_ten ch s =
  let l = String.length s in
    if l > 10 then
      (* [int64] values could be too big *)
      raise (Pdf.PDFError "xref too big")
    else
      (fold_left ( ^ ) "" (many ch (10 - l))) ^ s

let string_of_xref n =
  pad_to_ten "0" (Int64.to_string n) ^ " 00000 n \n" 

(* Write the cross-reference table to a channel. xrefs is a list of positions,
~-1L meaning a free entry. *)
let write_xrefs xrefs i =
  let os = output_string i in
    os "xref\n";
    os ("0 " ^ string_of_int (length xrefs + 1) ^ " \n");
    os "0000000000 65535 f \n";
    iter os (map string_of_xref xrefs)

(* \section{PDF Strings} *)

(* Convert a string to one suitable for output. The function [escape] escapes
parentheses and backslashes. *)
let make_pdf_string s =
  let rec escape = function
    | [] -> []
    | ('(' | ')' | '\\') as c::cs -> '\\'::c::escape cs
    | '\n'::cs -> '\\'::'n'::escape cs
    | '\r'::cs -> '\\'::'r'::escape cs
    | '\t'::cs -> '\\'::'t'::escape cs
    | '\b'::cs -> '\\'::'b'::escape cs
    | '\012'::cs -> '\\'::'f'::escape cs
    | c::cs -> c::escape cs
  and enclose s = "(" ^ s ^ ")" in
    enclose & implode & escape & explode s

(* \section{Flattening PDF to strings} *)

(* We have two kinds of flat data to write: Strings and streams (we cannot
represent streams as strings, since there is a langauge limit on the length of
strings. *)
type writeout =
  | WString of string
  | WStream of Pdf.stream

(* We want real numbers with no exponents (format compliance), and no trailing
zeroes (compactness). *)
let format_real r =
  let chars = rev (explode (Printf.sprintf "%f" r)) in
    let chars' = dropwhile (eq '0') chars in
      let chars'' = dropwhile (eq '.') chars' in
        implode (rev chars'')

(* Character codes in a name $<$ 33 or $>$ 126 are replaced with hashed combinations
(e.g \#20 for space). If the name contains the null character, an exception is
raised. *)
let rec make_pdf_name_inner prev = function
  | [] -> rev prev
  | '\000'::_ ->
      raise (Pdf.PDFError "Name cannot contain the null character")
  | h::t when
      h < '\033' || h > '\126' || Pdf.is_delimiter h || h = '#'
    ->
      let chars =
        '#'::explode (Printf.sprintf "%X" (int_of_char h))
      in
        make_pdf_name_inner (rev chars @ prev) t
  | h::t -> make_pdf_name_inner (h::prev) t

let make_pdf_name n =
  match explode n with
  | '/'::more -> "/" ^ (implode & make_pdf_name_inner [] more)
  | _ -> raise (Pdf.PDFError "bad name")

(* Calculate a strings and streams representing the given pdf datatype instance,
assuming it has no unresolved indirect references. *)
let rec strings_of_pdf = function
  | Pdf.Null -> [WString "null"]
  | Pdf.Boolean b -> [WString (string_of_bool b)]
  | Pdf.Integer n -> [WString (string_of_int n)]
  | Pdf.Real r -> [WString (format_real r)]
  | Pdf.String s -> [WString (make_pdf_string s)]
  | Pdf.Name n -> [WString (make_pdf_name n)]
  | Pdf.Array elts ->
      let strings =
        map
          (function
            | WString x -> WString (x ^ " ")
            | _ -> raise (Pdf.PDFError "direct stream object"))
          (flatten (map strings_of_pdf elts))
      in
        [WString "[ "] @ strings @ [WString "]"]
  | Pdf.Dictionary entries ->
      let strings =
        map
          (fun (k, v) ->
             [WString (make_pdf_name k ^ " ")] @
             strings_of_pdf v
             @ [WString " "])
          entries
      in
        [WString "<< "] @ flatten strings @ [WString ">>"]
  | Pdf.Stream {contents = (dict, data)} ->
      strings_of_pdf dict @
        [(WString "\010stream\010"); (WStream data); (WString "\010endstream")]
  | Pdf.Indirect n ->
      [WString (string_of_int n ^ " 0 R")]

(* \intf Produce a single string from a PDF object. Only use for things which will
always fall under the string size limit. *)
let string_of_pdf s =
  let strings =
    map (function (WString x) -> x | _ -> "") (strings_of_pdf s)
  in
    fold_left ( ^ ) "" (interleave " " strings)

let string_of_pdf_obj pdf o =
  Printf.sprintf "OBJECT %i\n" o ^
  string_of_pdf (Pdf.lookup_obj pdf o)

(* Calculate strings, one for each indirect object in the body. *)
let strings_of_object (n, pdfobject) =
  [WString (string_of_int n ^ " 0 obj\n")] @
  strings_of_pdf pdfobject @
  [WString "\nendobj\n"]

(* \section{Stream output} *)

(* Output a stream. *)
let output_stream o s =
  Pdf.getstream s;
  match s with
  | Pdf.Stream {contents = _, Pdf.Got arr} ->
      if stream_size arr > 0 then
        for i = 0 to stream_size arr - 1 do
          o.output_byte arr.{i}
        done
  | _ -> raise (Pdf.PDFError "output_stream")

(* \section{Encrypting a PDF while writing} *)
type encryption_method =
  | PDF40bit
  | PDF128bit
  | AES128bit of bool (*r true = encrypt metadata, false = don't. *)

type encryption = 
  {encryption_method : encryption_method;
   owner_password : string;
   user_password : string;
   permissions : Pdfcrypt.permission list}

let crypt_if_necessary pdf = function
  | None -> pdf
  | Some enc ->
      let f =
        match enc.encryption_method with
        | PDF40bit -> Pdfcrypt.encrypt_pdf_40bit
        | PDF128bit -> Pdfcrypt.encrypt_pdf_128bit
        | AES128bit em -> Pdfcrypt.encrypt_pdf_AES em
      in
        f enc.user_password enc.owner_password enc.permissions pdf

(* \section{Linearized (Fast Web View) writing} *)

(* The Part 6 (First Page Section) object numbers.
(1) Page object for first page
(2) Outline hierarchy, if PageMode is UseOutlines
(3) All objects the page object refers to, except page nodes or other page objects *)
let part6_parts_of_pdf pdf =
  let catalog = Pdf.catalog_of_pdf pdf in
    let first_page_objnum =
      match Pdf.page_reference_numbers pdf with
      | [] -> raise (Pdf.PDFError "No pages in document")
      | i::_ -> i
    in
      let outline_objnums =
        match Pdf.lookup_direct pdf "/PageMode" catalog with
        | Some (Pdf.Name "/UseOutlines") ->
            Pdf.reference_numbers_of_dict_entry pdf catalog "/Outlines"
        | _ -> []
      in
        let referenced_from_page =
          Pdf.objects_referenced
            ~don't_follow_dict_entries:["/Thumb"]
            ~don't_follow_if_dict_contains:
               [("/Type", Pdf.Name "/Page"); ("/Type", Pdf.Name "/Pages")]
            pdf (Pdf.lookup_obj pdf first_page_objnum)
        in
          setify_preserving_order
            (first_page_objnum :: outline_objnums @ referenced_from_page)

(* The Part 4 (Catalog and Document-Level Objects) object numbers. *)
let part4_parts_of_pdf pdf =
  let catalog_num =
    match pdf.Pdf.trailerdict with
    | Pdf.Dictionary d ->
        begin match lookup "/Root" d with
        | Some (Pdf.Indirect i) -> i
        | _ -> raise (Pdf.PDFError "Bad catalog")
        end
    | _ -> raise (Pdf.PDFError "Bad catalog")
  in     
    let catalog = Pdf.catalog_of_pdf pdf in
      let indirects_from_catalog
        ?(don't_follow_dict_entries = [])
        ?(don't_follow_if_dict_contains = [])
        entry
      =
        match catalog with
        | Pdf.Dictionary d ->
            begin match lookup entry d with
            | Some v ->
                  Pdf.objects_referenced
                    ~don't_follow_dict_entries ~don't_follow_if_dict_contains pdf v
            | _ -> []
            end
        | _ -> raise (Pdf.PDFError "bad catalog")
      in
        let sources_follow =
          ["/ViewerPreferences"; "/PageMode"; "/Threads"; "/OpenAction"; "/Encrypt"]
        in
          let objnum_of_acroform =
            match catalog with
            | Pdf.Dictionary d ->
                begin match lookup "/AcroForm" d with
                | Some (Pdf.Indirect i) -> [i]
                | _ -> []
                end
            | _ -> []
          in
            (* Catalog number is the head. *)
            setify_preserving_order
              (catalog_num::
                flatten
                  (map (indirects_from_catalog
                  ~don't_follow_dict_entries:["/Parent"]
                  ~don't_follow_if_dict_contains:["/Type", Pdf.Name "/Page";
                  "/Type", Pdf.Name "/Pages"]) sources_follow) @
                  objnum_of_acroform)

(* Part 7: For each page, objects reachable from this page which are reachable
   from no others;
   Part 8: Objects referenced from > 1 page;
   Part 9: Anything else  *)
let print_nums ls =
  iter (Printf.printf "%i ") ls;
  flprint "\n"

let get_main_parts p3nums pdf =
  let objects_left = setminus (Pdf.objnumbers pdf) p3nums
  and pagenums = 
    match Pdf.page_reference_numbers pdf with
    | [] -> raise (Pdf.PDFError "This PDF has no pages")
    | _::t -> t
  in
    let pages = map (Pdf.lookup_obj pdf) pagenums in
      let objects_from_each_page =
        map
          (Pdf.objects_referenced
             ~don't_follow_dict_entries:
                ["/Thumb"; "/Parent"]
             ~don't_follow_if_dict_contains:
                [("/Type", Pdf.Name "/Page"); ("/Type", Pdf.Name "/Pages")]
             pdf)
          pages
      in
        (*i Printf.printf "objects from each page...\n";
        iter
        (fun (n, objs) -> Printf.printf "PAGE %i" n; print_nums objs)
          (combine (indx objects_from_each_page) objects_from_each_page); i*)
        let histogram =
          collate compare & sort compare & flatten objects_from_each_page
        in
          (*i Printf.printf "get main parts: histogram:\n";
          iter print_nums histogram; i*)
          let shared_objects =
            flatten & map (function x -> [hd x]) &
            keep (function [] | [_] -> false | _ -> true) histogram
          in
            let shared_objects = setminus shared_objects p3nums in
              (*i Printf.printf "shared objects...\n";
              print_nums shared_objects; i*)
              let unshared_lists =
                map (lose (member' shared_objects)) objects_from_each_page
              in
                (* Put them in order (page object first) and flatten *)
                let part7_pages =
                  map2 (fun p l -> p::lose (eq p) l) pagenums unshared_lists
                in
                  let unshared_objects = flatten part7_pages in
                    let unshared_objects = setminus unshared_objects p3nums in
                      let part9 =
                        setminus (setminus objects_left shared_objects) unshared_objects
                      in
                        part7_pages, unshared_objects, shared_objects, part9

(* We output 10-character blanks XXXXXXXXXX, overwriting them when we know the
values, at the end of the process. *)

(* Return all trailerdict entries except for size, root and prev, as a partial
dictionary entry list represented as a string. Number changes will need to have
occured for everything in the trailerdict by now, since we're creating X O R
references to them...*)
let rest_of_trailerdict_entries pdf =
  let str =
    string_of_pdf
      (fold_left Pdf.remove_dict_entry pdf.Pdf.trailerdict ["/Prev"; "/Size"; "/Root"])
  in
    implode & rev & tl & tl & rev & tl & tl & explode str

let flatten_W o = function
 | WString s -> output_string o s
 | WStream data -> output_stream o (Pdf.Stream {contents = Pdf.Null, data})

(* Renumber old numbers to new ones, renumbering any other objects in the PDF
which clash. Returns the new PDF. *)
let lin_changes old_nums new_nums pdf =
  assert (length old_nums = length new_nums);
  if old_nums = [] then hashtable_of_dictionary [] else
    let clash_changes =
      let maxnum = Pdf.maxobjnum pdf + 1 in
        let new_objnums = ilist maxnum (maxnum + length new_nums - 1) in
          combine new_nums new_objnums
    in
      let changes = clash_changes @ combine old_nums new_nums in
        hashtable_of_dictionary changes

let lin_renumber old_nums new_nums pdf =
  assert (length old_nums = length new_nums);
  match new_nums with
  | [] -> pdf
  | _ -> Pdf.renumber (lin_changes old_nums new_nums pdf) pdf

(* Rember the items in [l] according to the (parital) changes given. *)
let list_renumber old_nums new_nums pdf l =
  let changes = lin_changes old_nums new_nums pdf in
    map (fun x -> try Hashtbl.find changes x with Not_found -> x) l

(* List of (object number, final position of object in file) pairs *)
type xrefblank =
  | PDFObj of int
  | LinearizationDictionaryPosition
  | PrimaryHintStreamPosition
  | FileLength
  | HintOffset
  | HintLength
  | EndOfFirstPage
  | MainXRefTableFirstEntry
  | Prev

(* List of (object number, position of first X in file) pairs. *)
(*i let x_positions = ref []

(* List of (object number, position of object in file) pairs. *)
let object_positions = ref []

(* List of (special, position in file) pairs. *)
let specials = ref [] i*)

(* Replace the markers with the (now calculated) contents *)
let replace_xs o object_positions x_positions specials =
  iter
    (function
     | PDFObj i, xpos ->
         begin match lookup i !object_positions with
         | Some pos ->
             o.seek_out xpos;
             output_string o (pad_to_ten "0" (Int64.to_string pos))
         | None -> raise (Pdf.PDFError "Linearization inconsistency")
         end
     | other, xpos ->
         let pad =
           match other with
           | LinearizationDictionaryPosition
           | PrimaryHintStreamPosition -> "0"
           | _ -> " "
         in
           match lookup other !specials with
           | Some pos ->
               o.seek_out xpos;
               output_string o (pad_to_ten pad (Int64.to_string pos))
           | _ -> ())
    !x_positions 

(* Outputting specials markers *)
let output_special_xref_line o xrefblank x_positions =
  x_positions =:: (xrefblank, o.pos_out ());
  output_string o "XXXXXXXXXX 00000 n \n"

let output_xref_line o x_positions objnum =
  output_special_xref_line o (PDFObj objnum) x_positions

let output_special o xrefblank x_positions =
  x_positions =:: (xrefblank, o.pos_out ());
  output_string o "XXXXXXXXXX"

(* The minimum number of bits needed to represent the number given. *)
let bits_needed n =
  if n = 0 then 0 else log2of (pow2lt n * 2)

(* The number of bytes which an object will use up in the file. *)
let object_bytes pdf objnum =
  let strings = strings_of_object (objnum, Pdf.lookup_obj pdf objnum)
  and length_of_string = function
    | WString s -> String.length s
    | WStream (Pdf.Got data) -> stream_size data
    | WStream (Pdf.ToGet (_, _, length)) -> i64toi length
  in
    fold_left ( + ) 0 (map length_of_string strings) 

(* Same for list of objects *)
let objects_bytes pdf objs =
  fold_left ( + ) 0 (map (object_bytes pdf) objs)

(* Calculates a bitstream representing the page offset hint table. *)
let page_offset_hint_table pdf pages first_page_objects shared_objects object_positions =
  assert (length pages > 0);
  let objects_reachable_from_each_page =
    let referenced page_objnum =
      Pdf.objects_referenced
        ~don't_follow_if_dict_contains:
           [("/Type", Pdf.Name "/Page"); ("/Type", Pdf.Name "/Pages")]
           pdf (Pdf.lookup_obj pdf page_objnum)
    in
      map
        (function p -> keep (member' shared_objects) & setify & referenced & hd p)
        pages
  in
  let page_lengths = map length pages
  and page_byte_lengths = map (objects_bytes pdf) pages in
  let least_in_page = hd & sort compare & page_lengths
  and most_in_page = hd & sort rev_compare & page_lengths
  and least_bytes_in_page = hd & sort compare & page_byte_lengths
  and most_bytes_in_page = hd & sort rev_compare & page_byte_lengths in
  (* Least number of objects in a page *)
  let item1 = least_in_page
  (* Location of first page's page object *)
  and item2 = i64toi (lookup_failnull (hd (hd pages)) !object_positions)
  (* Number of bits needed to represent the difference between the greatest and
  least number of objects in a page *)
  and item3 = bits_needed (most_in_page - least_in_page) 
  (* Least length of a page in the file in bytes *)
  and item4 = least_bytes_in_page
  (* Number of bits needed to represent the difference between the greatest and
  least length of a page in the file in bytes *)
  and item5 = bits_needed (most_bytes_in_page - least_bytes_in_page)
  (* Number of bits needed to represent the greatest number of shared object
  references. (in other words, in part 8) *)
  and item10 =
    bits_needed (hd & sort rev_compare &
    (length (hd pages)::map length objects_reachable_from_each_page))
  (* Number of bits needed to represent the numerically greatest shared object
  identifier used by the pages *)
  and item11 = bits_needed (max 0 (length shared_objects + length first_page_objects - 1))
  (* Number of bits needed to represent the numerator of the fractional position
  for each shared object reference. *)
  and item12 = 1
  (* The denominator of the fractional position for each shared object
  reference. *)
  and item13 = 1
  and b = Io.make_write_bitstream () in
    (*i Printf.printf "%i %i %i %i %i %i %i %i %i %i %i %i %i\n"
    item1 item2 item3 item4 item5 0 0 0 item5 item10 item11 item12 item13; i*)
    (* Write the header *)
    Io.putval b 32 (i32ofi item1);
    Io.putval b 32 (i32ofi item2);
    Io.putval b 16 (i32ofi item3);
    Io.putval b 32 (i32ofi item4);
    Io.putval b 16 (i32ofi item5);
    Io.putval b 32 0l;
    Io.putval b 16 0l;
    Io.putval b 32 0l;
    Io.putval b 16 (i32ofi item5);
    Io.putval b 16 (i32ofi item10);
    Io.putval b 16 (i32ofi item11);
    Io.putval b 16 (i32ofi item12);
    Io.putval b 16 (i32ofi item13);
    (* Now the per-page entries *)
    (* Items 1 *)
    for x = 1 to length pages do
      Io.putval b item3 (i32ofi (length (select x pages) - item1))
    done;
    (* Item 2 *)
    for x = 1 to length pages do
      Io.putval b item5 (i32ofi (select x page_byte_lengths - item4))
    done;
    (* Item 3 *)
    for x = 1 to length pages do
      if x = 1 then
        if length pages > 1
          then Io.putval b item10 0l
          else Io.putval b item10 (i32ofi (length (hd pages)))
      else
        Io.putval b item10 (i32ofi (length (select x objects_reachable_from_each_page)))
    done;
    (* Item 4 *)
    for x = 1 to length pages do
      if x = 1 && length pages > 1 then () else
        let shared_objects_reachable =
          select x objects_reachable_from_each_page
        in
          let table =
            let all_objs = first_page_objects @ shared_objects in
              hashtable_of_dictionary (combine all_objs (indx all_objs))
          in
            iter
              (fun s ->
                Io.putval b item11 (i32ofi (Hashtbl.find table s)))
              shared_objects_reachable
    done;
    (* Item 5 *)
    for x = 1 to length pages do
      if x = 1 && length pages > 1 then () else
        let shared_objects_reachable =
          select x objects_reachable_from_each_page
        in
          for y = 1 to length shared_objects_reachable do
            Io.putval b item12 0l (* Always use 0 / 1 fraction *)
          done
    done;
    (* Item 7 (No item 6) *)
    for x = 1 to length pages do
      Io.putval b item5 0l (* Ignored *)
    done;
    b

(* Shared object hint table *)
let shared_object_hint_table
  pdf first_page_objects shared_objects shared_object_positions
=
  assert (length shared_objects = length shared_object_positions);
  (*i Printf.printf "Making shared_object_hint_table: %i shared objects, %i first page ones\n"
  (length shared_objects) (length first_page_objects); i*)
  (*i let lookup_pos = combine shared_objects shared_object_positions in i*)
  let lengths_of_shared_objects =
    map (object_bytes pdf) (shared_objects @ first_page_objects)
  in
    let least =
      match sort compare lengths_of_shared_objects with
      | [] -> 0
      | h::_ -> h
    and greatest =
      match sort rev_compare lengths_of_shared_objects with
      | [] -> 0
      | h::_ -> h
    in
      let b = Io.make_write_bitstream () in
        (* Object number of first object in shared objects section *)
        let item1 = match shared_objects with [] -> 0 | h::_ -> h
        (* Location of the first object in the shared objects section *)
        and item2 = 0 (*i match shared_objects with [] -> 0 | h::_ -> lookup_failnull h lookup_pos i*)
        (* The number of shared object entries for the first page (including
        unshared objects *)
        and item3 = length first_page_objects
        and item4 = length first_page_objects + length shared_objects
        (* The least length of a shared object group in bytes (= least length of an
        object in bytes) *)
        and item6 = least
        (* Number of bits required to encode the difference between the greatest and
        smallest length of an shared object group (=object) in bytes *)
        and item7 = bits_needed (greatest - least)
        in
          Io.putval b 32 (i32ofi item1);
          Io.putval b 32 (i32ofi item2);
          Io.putval b 32 (i32ofi item3);
          Io.putval b 32 (i32ofi item4);
          Io.putval b 16 0l;
          Io.putval b 32 (i32ofi item6);
          Io.putval b 16 (i32ofi item7);
          (*i Printf.printf "shared object table header: %i %i %i %i %i %i %i\n"
          item1 item2 item3 item4 0 item6 item7; i*)
          (* Main Section, Sequence One (First Page Objects) *)
          (* Item 1s (byte lengths) *)
          iter
            (fun x ->
              let len = object_bytes pdf x - item6 in 
                Io.putval b item7 (i32ofi len))
            first_page_objects;
          (* Item 2s *)
          iter (function _ -> Io.putval b 1 0l) first_page_objects;
          (* Item 4s *)
          iter (function _ -> Io.putval b 0 0l) first_page_objects;
          (* Main Section, Sequence Two (Shared Objects (Part 8)) *)
          (* Item 1s *)
          iter
            (fun x ->
              let len = object_bytes pdf x - item6 in
                Io.putval b item7 (i32ofi len))
            shared_objects;
          (* Item 2s *)
          iter (function _ -> Io.putval b 1 0l) shared_objects;
          (* Item 4s *)
          iter (function _ -> Io.putval b 0 0l) shared_objects;
          b
          
(* This is filled in by the Pdfdoc module at code-loading time. It remains
static thereafter. *)
let pagetree_make_explicit = ref ident

(* OBJECT NUMBERS:
1..n    Objects not related to the first page
n+1     Linearization dictionary
n+2     Catalog
n+3     First page's page object
n+4..m  Rest of first page and related content
m + 1   Primary hint stream. *)
let pdf_to_output_linearized encrypt pdf o =
  let specials = ref []
  and object_positions = ref []
  and x_positions = ref [] in
  let pdf = !pagetree_make_explicit pdf in
  let pdf = Pdf.remove_unreferenced pdf in 
  let writeobj pdf p =
    let obj = 
      try Pdf.lookup_obj pdf p with
        | Not_found -> Pdf.Null
    in
      object_positions =:: (p, o.pos_out ()); 
      iter
        (flatten_W o)
        (strings_of_object (p, obj))
  in
  let p4objs = part4_parts_of_pdf pdf
    (* First object is catalog *)
  and p6objs = part6_parts_of_pdf pdf in
    (* First object is first page's page object number *)
  assert (length p4objs > 0 && length p6objs > 0);
  let objects_in_rest_of_file =
    Pdf.objcard pdf - length p4objs - length p6objs
  in
  (* Part 1: Header *)
  output_string o (header pdf);
  (* Part 2: Linearization parameter dictionary *)
  let lin_dict_obj_number = objects_in_rest_of_file + 1 in
  specials =:: (LinearizationDictionaryPosition, o.pos_out ());
  output_string o
(string_of_int lin_dict_obj_number ^ " 0 obj\n<< /Linearized 1.0\n/L ");
  output_special o FileLength x_positions;
  output_string o "\n/H [ ";
  output_special o HintOffset x_positions;
  output_string o " ";
  output_special o HintLength x_positions;
  output_string o "]\n";
  output_string o ("/O " ^ string_of_int (objects_in_rest_of_file + 3) ^ "\n");
  output_string o "/E ";
  output_special o EndOfFirstPage x_positions;
  output_string o
("\n/N " ^ (string_of_int & length & Pdf.page_reference_numbers pdf) ^ "\n/T");
  output_special o MainXRefTableFirstEntry x_positions;
  output_string o "\n>>\nendobj\n";
  (* Part 3: First page cross-reference table and trailer *)
  let p3length = length p4objs + length p6objs + 2 in
  let p3nums =
    if p3length = 0 then [] else
      ilist_null
        (objects_in_rest_of_file + 2)
        (objects_in_rest_of_file + 2 + length p4objs + length p6objs - 1)
  in
  let order = (hd p4objs::hd p6objs::tl p4objs @ tl p6objs) in
  let new_p6objs = list_renumber order p3nums pdf p6objs in 
  let pdf = lin_renumber order p3nums pdf in
  let p7_pages, p7nums, p8nums, p9nums = get_main_parts p3nums pdf in
  let p7length = objects_bytes pdf p7nums in
  let p8lengths = map (object_bytes pdf) p8nums in
  let main_nums = p7nums @ p8nums @ p9nums in
  let new_main_nums =
    if length main_nums > 0 then ilist 1 (length main_nums) else []
  in
  let list_renumber = list_renumber main_nums new_main_nums pdf in
  let p7_pages = map list_renumber p7_pages in
  let new_p6objs = list_renumber new_p6objs
  and new_p8nums = list_renumber p8nums in
  let pdf = lin_renumber main_nums new_main_nums pdf in
  let pdf = crypt_if_necessary pdf encrypt in
  let position_of_first_page_xref_table = o.pos_out () in
  output_string o
("xref\n" ^ string_of_int (objects_in_rest_of_file + 1) ^
" " ^ string_of_int p3length ^ " \n");
  output_special_xref_line o LinearizationDictionaryPosition x_positions;
  iter (output_xref_line o x_positions) p3nums;
  output_special_xref_line o PrimaryHintStreamPosition x_positions;
  output_string o
("trailer\n << /Size " ^ string_of_int (Pdf.objcard pdf + 3) ^ " /Prev ");
  output_special o Prev x_positions;
  output_string o
(" /Root " ^ string_of_int (objects_in_rest_of_file + 2) ^
" 0 R " ^ rest_of_trailerdict_entries pdf ^ ">>\n" ^ "startxref\n0\n%%EOF\n");
  (* Part 4 and Part 6: Document-level and first page  *)
  iter (writeobj pdf) p3nums;
  specials =:: (EndOfFirstPage, o.pos_out ());
  (* Part 5: Primary hint stream *)
  let all_pages = tl p3nums::p7_pages in
  let p8positions = cumulative_sum (p7length + i64toi (o.pos_out ())) p8lengths in
  let offset_table = page_offset_hint_table pdf all_pages new_p6objs new_p8nums object_positions in
  let shared_table = shared_object_hint_table pdf new_p6objs new_p8nums p8positions in
  let stream_content =
    Io.bytestream_of_write_bitstream &
    Io.write_bitstream_append_aligned offset_table shared_table
  in
  let hintstream_dict =
    Pdf.Dictionary
      [("/Length", Pdf.Integer (stream_size stream_content));
       ("/S", Pdf.Integer (stream_size (bytestream_of_write_bitstream offset_table)))]
  in
  let stream_wstrings =
    strings_of_pdf
      (Pdf.Stream (ref (hintstream_dict, Pdf.Got (stream_content))))
  and hint_num = Pdf.objcard pdf + 2 in
  let hs_offset = o.pos_out () in
  specials =:: (PrimaryHintStreamPosition, hs_offset);
  specials =:: (HintOffset, hs_offset);
  output_string o ((string_of_int hint_num) ^ " 0 obj\n");
  iter (flatten_W o) stream_wstrings;
  output_string o "\nendobj\n";
  let hs_length = i64sub (o.pos_out ()) hs_offset in
  specials =:: (HintLength, hs_length);
  (* Parts 7, 8 and 9: Remaining pages and other objects. *)
  iter (writeobj pdf) new_main_nums;
  (* Part 11: Main cross-reference table and trailer *)
  specials =:: (Prev, o.pos_out ());
  let main_size = length p7nums + length p8nums + length p9nums + 1 in
  output_string o ("xref\n0 " ^ string_of_int main_size ^ "\n");
  specials =:: (MainXRefTableFirstEntry, o.pos_out ());
  output_string o ("0000000000 65536 f \n");
  iter (output_xref_line o x_positions) new_main_nums;
  output_string o ("trailer\n<< /Size " ^ string_of_int main_size ^ " >>\nstartxref\n");
  output_string o (Int64.to_string position_of_first_page_xref_table);
  output_string o "\n%%EOF\n";
  specials =:: (FileLength, o.pos_out ());
  replace_xs o object_positions x_positions specials

(* \section{Main functions} *)

(*i let print_pdf_objs pdf =
  Pdf.objiter
    (fun n obj ->
       Printf.printf "%i 0 obj:\n\n" n;
       Printf.printf "%s\n" (string_of_pdf obj))
    pdf i*)

(* \intf Flatten a PDF document to an [Io.output]. *)
let pdf_to_output ?(linearize = false) ?encrypt pdf o =
  if linearize then pdf_to_output_linearized encrypt pdf o else
    let pdf = Pdf.renumber (changes pdf) pdf in
      let pdf = crypt_if_necessary pdf encrypt in
        output_string o (header pdf);
        let xrefs = ref [] in
          Pdf.objiter
            (fun ob p ->
               let strings = strings_of_object (ob, p) in
                 xrefs =:: o.pos_out ();
                 iter (flatten_W o) strings)
            pdf;
          let xrefstart = o.pos_out () in
            write_xrefs (rev !xrefs) o;
            output_string o "trailer\n";
            let trailerdict' =
              match pdf.Pdf.trailerdict with
              | Pdf.Dictionary trailerdict ->
                  Pdf.Dictionary
                    (add "/Size" (Pdf.Integer (length !xrefs + 1))
                      (add "/Root" (Pdf.Indirect pdf.Pdf.root) trailerdict))
              | _ ->
                  raise
                    (Pdf.PDFError "Pdf.pdf_to_channel: Bad trailer dictionary")
            in
              iter (flatten_W o) (strings_of_pdf trailerdict');
              output_string o
                ("\nstartxref\n" ^ Int64.to_string xrefstart ^ "\n%%EOF\n")

let change_id pdf f =
  match pdf.Pdf.trailerdict with
  | Pdf.Dictionary d ->
      {pdf with
         Pdf.trailerdict = Pdf.Dictionary (add "/ID" (Pdf.generate_id pdf f) d)}
  | _ -> raise (Pdf.PDFError "Bad trailer dictionary")

(* \intf Write a PDF to a channel. Don't use [mk_id] when the file is encrypted.*)
let pdf_to_channel ?(linearize = false) ?encrypt ?(mk_id = false) pdf ch =
  let pdf =
    if mk_id then change_id pdf "" else pdf
  in
    pdf_to_output ~linearize ?encrypt pdf (output_of_channel ch)

(* \intf Similarly to a named file. If [mk_id] is set, the /ID entry in the
document's trailer dictionary is updated using the current date and time and the
filename. Don't use [mk_id] when the file is encrypted. *)
let pdf_to_file ?(linearize = false) ?encrypt ?(mk_id = true) pdf f =
  let pdf' =
    if mk_id then change_id pdf f else pdf
  in
    let ch = open_out_bin f in
      pdf_to_channel ~linearize ?encrypt ~mk_id:false pdf' ch;
      close_out ch

