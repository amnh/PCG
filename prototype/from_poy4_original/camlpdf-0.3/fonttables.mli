(** Standard PDF Fonts *)

(** Calculate the width, in millipoints, of a string in the given font, taking
into account kerning. *)
val textwidth : Pdftext.standard_font -> string -> int

