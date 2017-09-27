(** Representing PDF files in memory *)

(** A stream is either in memory, or at a position and of a length in an
[Io.input] *)
type stream =
  | Got of Utility.bytestream
  | ToGet of Io.input * int64 * int64

(** PDF objects. *)
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

(** The type of a map from PDF object numbers to PDF objects *)
type pdfobjects

(** A Pdf document. Major and minor version numbers, pointer to root, file
objects and the trailer dictionary as a [Dictionary pdfobject]. *)
type pdfdoc =
  {mutable major : int;
   mutable minor : int;
   mutable root : int;
   mutable objects : pdfobjects;
   mutable trailerdict : pdfobject}

(** The empty document (PDF 1.0, no objects, no root, empty trailer dictionary.
Note this is not a well-formed PDF. *)
val empty : pdfdoc

(** Any function may return this. *)
exception PDFError of string

(** Get a stream from disc if it hasn't already been got. The input is a
[Stream pdfobject]. *)
val getstream : pdfobject -> unit

(** Return a float from either a [Real] or an [Int] *)
val getnum : pdfobject -> float

(** Parse a PDF rectangle structure into min x, min y, max x, max y. *) 
val parse_rectangle : pdfobject -> float * float * float * float

(** Calling [parse_matrix pdf name dict] parses a PDF matrix found under
key [name] in dictionary [dict] into a [Transform.transform_matrix]. If there is
no matrix, the identity matrix is returned. *)
val parse_matrix : pdfdoc -> string -> pdfobject -> Transform.transform_matrix 

(** Build a matrix [pdfobject]. *)
val make_matrix : Transform.transform_matrix -> pdfobject

(** Lookup an object in a document, parsing it if required. Raises [Not_found]
if the object does not exist. *)
val lookup_obj : pdfdoc -> int -> pdfobject

(** [lookup_fail errtext doc key dict] looks up a key in a PDF dictionary or the
dictionary of a PDF stream. Fails with [PDFError errtext] if the key is not
found. Follows indirect object links. *)
val lookup_fail : string -> pdfdoc -> string -> pdfobject -> pdfobject

(** Same, but with customised exception. *)
val lookup_exception : exn -> pdfdoc -> string -> pdfobject -> pdfobject

(** [lookup_direct doc key dict] looks up the key returning an option type. *) 
val lookup_direct : pdfdoc -> string -> pdfobject -> pdfobject option

(** Same, but allow alternative key. *)
val lookup_direct_orelse :
  pdfdoc -> string -> string -> pdfobject -> pdfobject option

(** Remove a dictionary entry, if it exists. *)
val remove_dict_entry : pdfobject -> string -> pdfobject

(** Replace a dictionary entry, raising [Not_found] if it's not there. *)
val replace_dict_entry : pdfobject -> string -> pdfobject -> pdfobject

(** Add a dictionary entry, replacing if already there. *)
val add_dict_entry : pdfobject -> string -> pdfobject -> pdfobject

(** Make a PDF object direct -- that is, follow any indirect links. *)
val direct : pdfdoc -> pdfobject -> pdfobject

(** Iterate over the objects in a document. The iterating functions recieves both
object number and object from the object map. *)
val objiter : (int -> pdfobject -> unit) -> pdfdoc -> unit

(** Iterate over the objects in a document. The iterating functions recieves
object number, generation number and object from the object map. *)
val objiter_gen : (int -> int -> pdfobject -> unit) -> pdfdoc -> unit

(** Map over all pdf objects in a document. Does not include trailer dictionary. *)
val objmap : (pdfobject -> pdfobject) -> pdfdoc -> pdfdoc 

(** Return the cardinality of the object map. *)
val objcard : pdfdoc -> int

(** Add an object. Returns the ammended document, and the number chosen. *)
val addobj : pdfdoc -> pdfobject -> pdfdoc * int

(** Same, but pick a number ourselves. *)
val addobj_given_num : ?gen:int -> pdfdoc -> (int * pdfobject) -> pdfdoc

(** Map over just the stream objects in a document. *)
val map_stream : (pdfobject -> pdfobject) -> pdfdoc -> pdfdoc

(** Iterate over just the stream objects in a document. *)
val iter_stream : (pdfobject -> unit) -> pdfdoc -> unit

(** Make a number of PDF documents contain no mutual object numbers. They can
then be merged etc. without clashes. *)
val renumber_pdfs : pdfdoc list -> pdfdoc list

(** Garbage-collect a pdf document. *)
val remove_unreferenced : pdfdoc -> pdfdoc

(** Given a dictionary and a prefix (e.g gs), return a name, starting with the
prefix, which is not already in the dictionary (e.g /gs0). *)
val unique_key : string -> pdfobject -> string

(**/**)
type objectdata =
  | Parsed of pdfobject
  | ToParse

val renumber : (int, int) Hashtbl.t -> pdfdoc -> pdfdoc

val is_whitespace : char -> bool

val recurse_dict :
  (pdfobject -> pdfobject) -> (string * pdfobject) list -> pdfobject 

val recurse_array :
  (pdfobject -> pdfobject) -> pdfobject list -> pdfobject 

val bigarray_of_stream : pdfobject -> Utility.bytestream

val objnumbers : pdfdoc -> int list

val maxobjnum : pdfdoc -> int

val objects_of_list :
  (int -> pdfobject) option -> (int * (objectdata ref * int)) list -> pdfobjects

val objects_referenced :
  ?don't_follow_dict_entries:string list ->
  ?don't_follow_if_dict_contains:(string * pdfobject) list ->
  pdfdoc -> pdfobject -> int list

(** Generate and ID for a PDF document given its prospective file name (and using
the current date and time) . If the file name is blank, the ID is still likely to
be unique, being based on date and time only. *)
val generate_id : pdfdoc -> string -> pdfobject

val is_delimiter : char -> bool

val page_reference_numbers : pdfdoc -> int list

val reference_numbers_of_dict_entry : pdfdoc -> pdfobject -> string -> int list

val objects_referenced_and_objects :
  ?don't_follow_dict_entries:string list ->
  ?don't_follow_if_dict_contains:(string * pdfobject) list ->
  pdfdoc -> pdfobject -> (int * pdfobject) list

val catalog_of_pdf : pdfdoc -> pdfobject

val find_indirect : string -> pdfobject -> int option

val renumber_object_parsed : pdfdoc -> (int, int) Hashtbl.t -> pdfobject -> pdfobject

