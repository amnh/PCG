(* \part{CamlPDF}\chaptertitle{PDF}{Representing PDF files} *)

(* This module declares a data type which represents an Adobe PDF document,
and defines various simple operations on it. *)
open Utility
open Io

(* \section{Data Type for Representing PDF Documents} *)

(* Predicate on characters delimiting entities. *)
let is_delimiter = function
  | '(' | ')' | '<' | '>' | '[' | ']' | '{' | '}' | '%' | '/' -> true
  | _ -> false

(* \intf Streams of binary data, byte-addressable, can either be in memory ([Got]) or
still in an input channel ([ToGet]). *)
type stream =
  | Got of bytestream
  | ToGet of input * int64 * int64 (*r input, position, length *)

(* \intf Type for individual PDF objects. A [Name] includes the initial `/'. A
[Stream] consists of a reference to a pair of the stream dictionary (another
[pdfobject]) and a [stream]. Thus a [pdfobject] is technically mutable.  However,
at the user level, it is intended to be immutable: changes should be limited to
encoding and decoding of the stream.

Note that pdfobjects are not always amenable to polymorphic equality testing,
since the [Io.input] in the [ToGet] part of a [stream] contains functional
values. *)
type pdfobject =
  | Null
  | Boolean of bool
  | Integer of int
  | Real of float
  | String of string
  | Name of string 
  | Array of pdfobject list
  | Dictionary of (string * pdfobject) list
  | Stream of (pdfobject * stream) ref
  | Indirect of int

(* Pdf objects are stored in an efficient map structure. *)
module PdfObjMap =
  Map.Make
    (struct
       type t = int
       let compare = compare
    end)

(* An object is either lexed, or needs to be lexed from a position in the
input. *)
type objectdata =
  | Parsed of pdfobject
  | ToParse

(* We hold the maximum object number in use, [maxobjnum] to allow easy
production of new keys for the map. *)
type pdfobjects =
  {maxobjnum : int;
   parse : (PdfObjMap.key -> pdfobject) option;
   pdfobjects : (objectdata ref * int) PdfObjMap.t} (*r int is generation *)

(* \intf PDF Document. The major and minor version numbers, the root object number,
the list of objects and the trailer dictionary.

This represents the contents of a PDF file's user objects (object streams and
other mechanisms involved only in reading and writing are abstracted away). *)
type pdfdoc =
  {mutable major : int; 
   mutable minor : int;
   mutable root : int;
   mutable objects : pdfobjects; 
   mutable trailerdict : pdfobject} 

(* \intf The null PDF document. *)
let empty =
  {major = 1;
   minor = 0;
   root = 0;
   objects = {maxobjnum = 0; parse = None; pdfobjects = PdfObjMap.empty};
   trailerdict = Dictionary []}

(* \intf General exception for low-level errors. *)
exception PDFError of string

(* \section{Utility functions} *)

(* \intf Predicate on those characters considered whitespace in PDF files. *)
let is_whitespace = function
  | '\000' | '\009' | '\010' | '\012' | ' ' | '\013' -> true
  | _ -> false

(* \intf Get a stream from disk if it hasn't already been got. *)
let getstream = function
  | Stream ({contents = (d, ToGet (i, o, l))} as stream) ->
      if l = 0L then stream := (d, Got (mkstream 0)) else
        let s = mkstream (i64toi l) in
          begin try
            i.seek_in o;
            for c = 0 to i64toi l - 1 do
              s.{c} <- i.input_byte ()
            done;
            stream := (d, Got s)
          with
            EndOfInput ->
              raise (PDFError "Pdf.getstream: can't read stream.")
          end
  | Stream _ -> ()
  | _ -> raise (PDFError "Pdf.getstream: not a stream")

let recurse_array f elts =
  Array (map f elts)

(* \intf Similarly for dictionaries. *)
let recurse_dict f elts =
  let names, objects = split elts in
    let objects' = map f objects in
      Dictionary (combine names objects')

(* \intf Return a float from a PDF number. *)
let getnum = function
  | Real a -> a
  | Integer a -> float a
  | _ -> raise (PDFError "Pdf.getnum: not a number")

(* \intf Parse a PDF rectangle data structure. Returns min x, min y, max x, max y. *)
let parse_rectangle = function
  | Array [a; b; c; d] ->
      begin try
        let x, y, x', y' =
          getnum a, getnum b, getnum c, getnum d
        in
          fmin x x', fmin y y', fmax x x', fmax y y'
      with
        PDFError _ -> raise (PDFError "Pdf.parse_rectangle: bad rectangle")
      end
  | _ -> raise (PDFError "Pdf.parse_rectangle: not a rectangle")

let change_obj doc i obj =
  match fst (PdfObjMap.find i doc.objects.pdfobjects) with
  | {contents = Parsed _} -> assert false
  | {contents = ToParse} as r -> r := Parsed obj

(* Parse an object [n] in document [pdf], updating the object in the document so
it is ready-parsed should it be required again. *)
let parse_lazy pdf n =
  match pdf.objects.parse with
  | None -> assert false
  | Some f ->
      let obj = f n in
        change_obj pdf n obj;
        obj

(* \intf Look up an object. On an error return [Pdf.Null] *)
let lookup_obj doc i =
  try
    match fst (PdfObjMap.find i doc.objects.pdfobjects) with
    | {contents = Parsed obj} -> obj
    | {contents = ToParse} -> parse_lazy doc i
  with
    Not_found -> Null

let catalog_of_pdf pdf =
  try lookup_obj pdf pdf.root with
    Not_found -> raise (PDFError "No catalog")

(* \intf Given any pdf document and object, follow indirections to yield a
direct object. A hanging indirect is defined as [Null]. *)
let rec direct pdf = function
  | Indirect i ->
      begin try
        match fst (PdfObjMap.find i pdf.objects.pdfobjects) with
        | {contents = Parsed pdfobject} -> direct pdf pdfobject
        | {contents = ToParse} -> parse_lazy pdf i
      with
        Not_found -> Null
      end
  | obj -> obj

(* \intf Apply a function on Stream objects to all streams in a PDF document. We
assume stream dictionaries don't have indirect references to an object which
itself contains a stream. *)
let map_stream f pdf =
  let rec map_stream_inner f i = function
    | {contents = Parsed (Stream _ as stream)}, g -> ref (Parsed (f stream)), g
    | {contents = Parsed obj}, g -> ref (Parsed (obj)), g
    | {contents = ToParse}, g -> map_stream_inner f i (ref (Parsed (parse_lazy pdf i)), g)
  in
    let objects' =
      {pdf.objects with
         pdfobjects = PdfObjMap.mapi (map_stream_inner f) pdf.objects.pdfobjects}
    in
      {pdf with objects = objects'}

(* \intf Iterate over a stream. *)
let iter_stream f pdf =
  let rec iter_stream_inner f i = function
    | {contents = Parsed (Stream _ as stream)}, g -> f stream
    | {contents = ToParse} as r, g ->
        r := Parsed (parse_lazy pdf i);
        iter_stream_inner f i (r, g)
    | _ -> ()
  in
    PdfObjMap.iter (iter_stream_inner f) pdf.objects.pdfobjects

(* \intf Lookup a key in a dictionary, following indirect references,  returning
[None] on any failure. This works on both plain dictionaries and streams. *)
let lookup_direct pdf key dict =
  match direct pdf dict with
  | Dictionary d | Stream {contents = (Dictionary d, _)} ->
      begin match lookup key d with
      | None -> None
      | Some o -> Some (direct pdf o)
      end
  | _ -> None

(* \intf Look up under a key and its alternate. Return the value associated with the key that worked, or [None] if neither did. *)
let lookup_direct_orelse pdf k k' d =
  match lookup_direct pdf k d with
  | None -> lookup_direct pdf k' d
  | result -> result

(* \intf Look something up in a dictionary, failing with given exception if not
found. We make direct both the dictionary and the result of the lookup. This
also allows us to look things up in a stream dictionary transparently. *)
let lookup_exception exp pdf key dict =
  let dict' =
    match direct pdf dict with
    | Dictionary d | Stream {contents = Dictionary d, _} -> d
    | o -> raise (PDFError "not a dictionary")
  in
    match lookup key dict' with
    | None -> raise exp
    | Some v -> direct pdf v

(* \intf A specialised one raising [PDFError]. *)
let lookup_fail text =
  lookup_exception (PDFError text)

(* \intf Parse a matrix. *)
let parse_matrix pdf name dict =
  match lookup_direct pdf name dict with
  | None -> Transform.i_matrix
  | Some (Array [a; b; c; d; e; f]) ->
      let a = getnum a and b = getnum b and c = getnum c
      and d = getnum d and e = getnum e and f = getnum f in
        {Transform.a = a; Transform.b = b; Transform.c = c;
         Transform.d = d; Transform.e = e; Transform.f = f}
  | _ -> raise (PDFError "Malformed matrix")

(* \intf Make a matrix *)
let make_matrix tr =
  Array
    [Real tr.Transform.a; Real tr.Transform.b; Real tr.Transform.c;
     Real tr.Transform.d; Real tr.Transform.e; Real tr.Transform.f]

(* \intf Iterate over the objects in a document, in order of increasing object
number. *)
let objiter f doc =
  let f' k v =
    match v with
    | {contents = Parsed obj}, _ -> f k obj
    | {contents = ToParse}, _ -> f k (parse_lazy doc k)
  in
    PdfObjMap.iter f' doc.objects.pdfobjects

(* \intf Same, but also pass generation number. *)
let objiter_gen f doc =
  let f' k v =
    match v with
    | {contents = Parsed obj}, g -> f k g obj
    | {contents = ToParse}, g -> f k g (parse_lazy doc k)
  in
    PdfObjMap.iter f' doc.objects.pdfobjects

(* \intf Map on objects. *)
let objmap f doc =
  let f' i = function
    | {contents = Parsed obj}, g -> ref (Parsed (f obj)), g
    | {contents = ToParse}, g -> ref (Parsed (parse_lazy doc i)), g
  in
    {doc with objects =
       {doc.objects with
          pdfobjects = PdfObjMap.mapi f' doc.objects.pdfobjects}}

let maxobjnum pdf =
  pdf.objects.maxobjnum

(* Return a list of object numbers. *)
let objnumbers pdf =
  let keys = ref [] in
    objiter (fun k _ -> keys =:: k) pdf;
    rev !keys

(* \intf Cardinality of object set. O(n). *)
let objcard pdf =
  let card = ref 0 in
    objiter (fun _ _ -> incr card) pdf;
    !card

(* Remove an object. *)
let removeobj doc o =
  {doc with objects =
    {doc.objects with pdfobjects = PdfObjMap.remove o doc.objects.pdfobjects}}

(* Return a list of (k, v) pairs. *)
let list_of_objs doc =
  let objs = ref [] in
    objiter (fun k v -> objs =:: (k, Parsed v)) doc;
    !objs

(* \intf Add an object. We use the first number larger than the maxobjnum, and update that. *)
let addobj doc obj =
  let new_objnum = doc.objects.maxobjnum + 1 in
    let new_doc = 
      {doc with objects =
        {doc.objects with
           maxobjnum =
             new_objnum;
           pdfobjects =
             PdfObjMap.add new_objnum (ref (Parsed obj), 0) doc.objects.pdfobjects}}
    in
      new_doc, new_objnum

(* \intf The same, but use a given object number. *)
let addobj_given_num ?(gen=0) doc (num, obj) =
  {doc with objects =
     {doc.objects with
        maxobjnum =
          max doc.objects.maxobjnum num;
        pdfobjects =
          PdfObjMap.add num (ref (Parsed obj), gen) doc.objects.pdfobjects}}

(* Make a objects entry from a list of (number, object) pairs. *)
let objects_of_list parse l =
  let maxobj = ref 0
  and map = ref PdfObjMap.empty in
    iter
      (fun (k, v) ->
         maxobj := max !maxobj k;
         map := PdfObjMap.add k v !map)
      l;
    {parse = parse; pdfobjects = !map; maxobjnum = !maxobj}

(* Renumber an object given a change table (A hash table mapping old to new
numbers). *)
let rec renumber_object_parsed pdf changes obj =
  match obj with
  | Indirect i ->
      let i' =
        try Hashtbl.find changes i with
          Not_found -> i (*r A dangling indirect is valid. *)
      in
        Indirect i'
  | Array a ->
      recurse_array (renumber_object_parsed pdf changes) a
  | Dictionary d ->
      recurse_dict (renumber_object_parsed pdf changes) d
  | Stream {contents = (p, s)} ->
      Stream {contents = renumber_object_parsed pdf changes p, s}
  | pdfobject -> pdfobject

let renumber_object pdf changes objnum = function
  | ToParse ->
      renumber_object_parsed pdf changes (parse_lazy pdf objnum)
  | Parsed obj ->
      renumber_object_parsed pdf changes obj

(* Perform all renumberings given by a change table. *)
let renumber change_table pdf =
  let root' =
    try Hashtbl.find change_table pdf.root with Not_found -> pdf.root
  and trailerdict' =
    renumber_object pdf change_table 0 (Parsed pdf.trailerdict)
  and objects' =
    let nums, objs = split (list_of_objs pdf) in
      let objs' =
        map2 (renumber_object pdf change_table) nums objs
      and nums' =
        map (fun k -> try Hashtbl.find change_table k with _ -> k) nums
      in
        objects_of_list
          pdf.objects.parse
          (combine nums' (map (fun x -> ref (Parsed x), 0) objs'))
  in
    {pdf with
     root = root';
     objects = objects';
     trailerdict = trailerdict'}
 
(* \intf Renumber the objects (including root and trailer dictionary) in a list of
pdfs so they are mutually exclusive. We iterate over the key lists to build
a list of change tables which are applied to the input PDFs. NOTE: This can't
be used on PDFs where the generation numbers still matter (i.e before
decryption). *)
let renumber_pdfs pdfs =
  let keylists = map objnumbers pdfs
  and base = ref 1
  and tables = ref [] in
    iter
      (fun k ->
         let length = length k in
           let table = Hashtbl.create length in
             List.iter2 (Hashtbl.add table) k (ilist !base (!base + length - 1));
             tables =:: table;
             base += length)
      keylists;
    map2 renumber (rev !tables) pdfs

(* Used for sets of object numbers. *)
module RefSet =
  Set.Make
    (struct
       type t = int
       let compare = compare
    end)

(* Give a list of object numbers referenced in a given [pdfobject] *)
let rec
  referenced ?(don't_follow_dict_entries = []) ?(don't_follow_if_dict_contains = []) pdf found i
= function
  | Parsed (Indirect i) ->
      if not (RefSet.mem i !found) then
        begin
          let obj = 
            try lookup_obj pdf i with
              Not_found -> Null
          in
            match obj with
            | Dictionary d ->
                if not (member true (map (member' don't_follow_if_dict_contains) d)) then
                  begin
                  found := RefSet.add i !found;
                  referenced
                    ~don't_follow_dict_entries ~don't_follow_if_dict_contains pdf found i (Parsed obj)
                  end
            | _ ->
              found := RefSet.add i !found;
              referenced
                ~don't_follow_dict_entries ~don't_follow_if_dict_contains pdf found i (Parsed obj)
        end
  | Parsed (Array a) ->
      iter
        (referenced
          ~don't_follow_dict_entries ~don't_follow_if_dict_contains pdf found i)
        (map (fun x -> Parsed x) a)
  | Parsed (Dictionary d) ->
      iter
        (referenced
           ~don't_follow_dict_entries ~don't_follow_if_dict_contains pdf found i)
        (map
          (fun x -> Parsed (snd x))
          (lose (fun (k, _) -> member k don't_follow_dict_entries) d))
  | Parsed (Stream s) ->
      referenced
        ~don't_follow_dict_entries ~don't_follow_if_dict_contains pdf found i (Parsed (fst !s))
  | Parsed _ ->
      ()
  | ToParse ->
      referenced
        ~don't_follow_dict_entries ~don't_follow_if_dict_contains pdf found i
        (Parsed (parse_lazy pdf i))

(* \intf Remove any unreferenced objects. *)
let remove_unreferenced pdf =
  let found = ref RefSet.empty in
    referenced pdf found pdf.root (Parsed (lookup_obj pdf pdf.root));
    referenced pdf found 0 (Parsed pdf.trailerdict);
    found := RefSet.add pdf.root !found;
    let eltnumbers = RefSet.elements !found in
      (* If not found, just ignore. *)
      let elements =
        map
          (fun n -> try lookup_obj pdf n with Not_found -> Null)
          eltnumbers
      in
        fold_left
          addobj_given_num
          {pdf with 
             objects =
               {maxobjnum = 0;
                parse = pdf.objects.parse;
                pdfobjects = PdfObjMap.empty}}
          (combine eltnumbers elements)

(* \intf Objects referenced from a given one. *)
let objects_referenced
  ?(don't_follow_dict_entries = []) ?(don't_follow_if_dict_contains = []) pdf pdfobject
=
  let set = ref RefSet.empty in
    referenced
      ~don't_follow_dict_entries ~don't_follow_if_dict_contains pdf set 0 (Parsed pdfobject);
    RefSet.elements !set

(* \intf The same, but return the objects too. *)
let objects_referenced_and_objects
  ?(don't_follow_dict_entries = []) ?(don't_follow_if_dict_contains = []) pdf pdfobject
=
  let nums =
    objects_referenced
    ~don't_follow_dict_entries ~don't_follow_if_dict_contains pdf pdfobject
  in
    combine nums (map (lookup_obj pdf) nums)

(* \intf Remove a dictionary entry. Also works for streams. *)
let rec remove_dict_entry dict key =
  match dict with
  | Dictionary d -> Dictionary (remove key d)
  | Stream ({contents = (dict', stream)} as s) ->
      s := (remove_dict_entry dict' key, stream);
      Stream s
  | _ -> raise (PDFError "remove_dict_entry: not a dictionary")

(* \intf Replace dict entry, raising [Not_found] if it's not there. Also works
for streams.*)
let rec replace_dict_entry dict key value =
  match dict with
  | Dictionary d -> Dictionary (replace key value d)
  | Stream ({contents = (dict', stream)} as s) ->
      s := (replace_dict_entry dict' key value, stream);
      Stream s
  | _ -> raise (PDFError "replace_dict_entry: not a dictionary.")

(* \intf Add a dict entry, replacing if there. Also works for streams. *)
let rec add_dict_entry dict key value =
  match dict with
  | Dictionary d -> Dictionary (add key value d)
  | Stream ({contents = (dict', stream)} as s) ->
      s := (add_dict_entry dict' key value, stream);
      Stream s
  | _ -> raise (PDFError "add_dict_entry: not a dictionary.")

(* Find the contents of a stream as a bytestream. *)
let rec bigarray_of_stream s =
  getstream s;
  match s with
  | Stream {contents = _, Got bytestream} -> bytestream
  | _ -> failwith "couldn't extract raw stream"

(* \intf Given a dictionary and a prefix (e.g gs), return a name, starting with the
prefix, which is not already in the dictionary (e.g /gs0). *)
let unique_key prefix obj =
  let elts = match obj with
    | Dictionary es
    | Stream {contents = Dictionary es, _} -> es
    | _ -> raise (PDFError "unique_key: Not a dictionary or stream")
  in
    let names = fst (split elts)
    and name_of_num n = "/" ^ prefix ^ string_of_int n
    and num = ref 0 in
      while member (name_of_num !num) names do incr num done;
      name_of_num !num

(* \intf Given a PDF and potential filename, calculate an MD5 string and build a
suitable /ID entry from it. *)
let generate_id pdf path =
  let digest =
    Digest.string (path ^ string_of_float (Unix.gettimeofday ()))
  in
    Array [String digest; String digest]

(* Find the page reference numbers, given the top level node of the page tree *)
let rec page_reference_numbers_inner pdf pages_node node_number =
  match lookup_direct pdf "/Type" pages_node with
  | Some (Name "/Pages") ->
      begin match lookup_direct pdf "/Kids" pages_node with
      | Some (Array elts) ->
          flatten
            (map
              (function
               | Indirect i ->
                   page_reference_numbers_inner
                     pdf (direct pdf (Indirect i)) i
               | _ -> raise (PDFError "badly formed page tree"))
              elts)
      | _ -> raise (PDFError "badly formed page tree")
      end
  | Some (Name "/Page") -> [node_number]
  | _ -> raise (PDFError "badly formed page tree")

let page_reference_numbers pdf =
  let root = lookup_obj pdf pdf.root in
    let pages_node =
        match lookup_direct pdf "/Pages" root with
        | Some p -> p
        | None -> raise (PDFError "badly formed page tree")
    in
      page_reference_numbers_inner pdf pages_node ~-1

(* Find all the indirect numbers reachable from an entry in a dictionary,
including the indirect of that dictionary entry, if it's an indirect. *)
let reference_numbers_of_dict_entry pdf dict entry =
  match dict with
  | Dictionary d ->
      begin match lookup entry d with
      | Some x -> objects_referenced pdf x
      | None ->
          raise (PDFError "reference_numbers_of_dict_entry: no entry")
      end
  | _ ->
      raise (PDFError "reference_numbers_of_dict_entry: not a dictionary")

(* Find the indirect reference given by the value associated with a key in a
dictionary. *)
let find_indirect key dict =
  match dict with
  | Dictionary d ->
      begin match lookup key d with
      | Some (Indirect i) -> Some i
      | _ -> None
      end
  | _ -> raise (PDFError "find_indirect: not a dictionary")

