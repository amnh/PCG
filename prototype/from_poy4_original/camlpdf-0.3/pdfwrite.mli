(** Writing PDF files *)

(** Encryption methods. The boolean for [AES128bit] indicates encryption of
metadata or lack thereof. *)
type encryption_method =
  | PDF40bit
  | PDF128bit
  | AES128bit of bool

(** The type of an encryption with certain user permissions. *)
type encryption = 
  {encryption_method : encryption_method;
   owner_password : string;
   user_password : string;
   permissions : Pdfcrypt.permission list}

(** Write a PDF document to an [Io.output], optionally encrypting and/or
linearizing. May raise [Io.EndOfOutput]. *)
val pdf_to_output : ?linearize:bool -> ?encrypt:encryption -> Pdf.pdfdoc -> Io.output -> unit

(** Similarly to an OCaml channel. If [mk_id] set, build a new /ID (don't use
[mk_id] on encrypted documents).*)
val pdf_to_channel : ?linearize:bool -> ?encrypt:encryption -> ?mk_id:bool -> Pdf.pdfdoc -> out_channel -> unit

(** Similarly to a named file. If [mk_id] is set, the /ID entry in the document's
trailer dictionary is updated using the current date and time and the filename.
Don't use [mk_id] on encrypted documents. *)
val pdf_to_file : ?linearize:bool -> ?encrypt:encryption -> ?mk_id:bool -> Pdf.pdfdoc -> string -> unit

(** Calculate a string of a pdf object. Due to OCaml's modest limit
on string length, this should be used only when the length of the output is
known to be limited (for example for debug purposes). *)
val string_of_pdf : Pdf.pdfobject -> string

(**/**)
(* For dodgy internal module recursion use only *)
val pagetree_make_explicit : (Pdf.pdfdoc -> Pdf.pdfdoc) ref

