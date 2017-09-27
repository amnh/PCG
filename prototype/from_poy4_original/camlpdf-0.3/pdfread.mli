(** Reading PDF files *)

exception PDFReadError of string
exception PDFSemanticError of string
(** There can be many things malformed with a PDF file, in addition to the
usual difficulties concerned with file I/O. CamlPDF wraps all these up in these
two exceptions. *)

(** Read a PDF from an [Io.input], with an optional user password which, if
absent, is assumed to be the empty string. *)
val pdf_of_input : ?upw:string -> Io.input -> Pdf.pdfdoc

(** Same, but delay loading of streams and parsing of objects. *)
val pdf_of_input_lazy : ?upw:string -> Io.input -> Pdf.pdfdoc

(** Read a PDF from an OCaml channel. *)
val pdf_of_channel : ?upw:string -> in_channel -> Pdf.pdfdoc

(** Same, but delay loading of streams and parsing of objects. *)
val pdf_of_channel_lazy : ?upw:string -> in_channel -> Pdf.pdfdoc

(** Read a PDF from the given filename. *)
val pdf_of_file : ?upw:string -> string -> Pdf.pdfdoc

(**/**)
(* For internal use by other parts of the library *)

val read_header : Io.input -> int * int

type lexeme =
  | LexNull
  | LexBool of bool
  | LexInt of int
  | LexReal of float
  | LexString of string
  | LexName of string
  | LexLeftSquare
  | LexRightSquare
  | LexLeftDict
  | LexRightDict
  | LexStream of Pdf.stream
  | LexEndStream
  | LexObj
  | LexEndObj
  | LexR
  | LexComment
  | StopLexing
  | LexNone

val lex_stream_data : Io.input -> int -> bool -> lexeme

val getuntil_white_or_delimiter : Io.input -> char list

val lex_number : Io.input -> lexeme

val lex_string : Io.input -> lexeme

val lex_hexstring : Io.input -> lexeme

val lex_comment : Io.input -> lexeme

val lex_dictionary : Io.input -> lexeme list

val parse : ?objnum':int -> lexeme list -> int * Pdf.pdfobject

val dropwhite : Io.input -> unit

val print_lexeme : lexeme -> unit

val string_of_lexeme : lexeme -> string

