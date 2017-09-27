(** Parsing Fonts and Extracting Text *)

(** {2 Data Types } *)

type type3_glpyhs =
  {fontbbox : float * float * float * float;
   fontmatrix : Transform.transform_matrix;
   charprocs : (string * Pdf.pdfobject) list;
   type3_resources : Pdf.pdfobject}

type simple_fonttype =
  | Type1
  | MMType1
  | Type3 of type3_glpyhs
  | Truetype

type fontmetrics = int array

type fontdescriptor

type differences

type encoding =
  | ImplicitInFontFile
  | StandardEncoding
  | MacRomanEncoding
  | WinAnsiEncoding
  | MacExpertEncoding
  | CustomEncoding of encoding * differences
  | FillUndefinedWithStandard of encoding

type simple_font =
  {fonttype : simple_fonttype;
   basefont : string;
   fontmetrics : fontmetrics option;
   fontdescriptor : fontdescriptor option;
   encoding : encoding}

type standard_font =
  | TimesRoman
  | TimesBold
  | TimesItalic
  | TimesBoldItalic
  | Helvetica
  | HelveticaBold
  | HelveticaOblique
  | HelveticaBoldOblique
  | Courier
  | CourierBold
  | CourierOblique
  | CourierBoldOblique
  | Symbol
  | ZapfDingbats

type cid_system_info =
  {registry : string;
   ordering : string;
   supplement : int}

type composite_CIDfont =
  {cid_system_info : cid_system_info;
   cid_basefont : string;
   cid_fontdescriptor : fontdescriptor;
   cid_widths : (int * int) list;
   cid_default_width : int}
  
type cmap_encoding =
  | Predefined of string
  | CMap of int (* indirect reference to CMap stream *)

type font =
  | StandardFont of standard_font * encoding
  | SimpleFont of simple_font
  | CIDKeyedFont of string * composite_CIDfont * cmap_encoding

(** {2 Reading a Font} *)

(** Read a font from a given document and object *)
val read_font : Pdf.pdfdoc -> Pdf.pdfobject -> font

(** {2 Text Extraction} *)

(** The type of text extractors. *)
type text_extractor

(** Build a text extractor from a document and font object *)
val text_extractor_of_font : Pdf.pdfdoc -> Pdf.pdfobject -> text_extractor

(** Return a list of unicode points from a given extractor and string (for
example from a [Pdfpages.Op_Tj] or [Op_TJ] operator). *)
val codepoints_of_text : text_extractor -> string -> int list

(** Same, but return UTF16BE *)
val utf16be_of_text : text_extractor -> string -> string

(** Same, but return Latin1 (Lossy) *)
val latin1_string_of_text : text_extractor -> string -> string

(** Decode a single character code in a standard font *)
val decode_char : encoding -> char -> char

(** Decode a single character code in a type3 font to a glyph name *)
val decode_type3_char : encoding -> char -> string

