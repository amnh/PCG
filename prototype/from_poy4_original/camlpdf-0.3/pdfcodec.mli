(** Encoding and decoding PDF streams *)

(**
{b Currently supported:}
- Decoders: ASCIIHexDecode, ASCII85Decode, FlateDecode,
LZWDecode, RunLengthDecode.
- Encoders: ASCIIHexDecode, ASCII85Decode, FlateDecode, RunLengthDecode.
- Predictors: PNG (all), TIFF (8-bit only).

*)

(** There was bad data. *)
exception Couldn'tDecodeStream of string

(** PdfCaml doesn't support this encoding or its predictor. *)
exception DecodeNotSupported

(** Given a document and stream, decode. The pdf document is updated
with the decoded stream. May return either of the exceptions above. *)
val decode_pdfstream : Pdf.pdfdoc -> Pdf.pdfobject -> unit

(** Given a document and stream decode just one stage. May return either of the
exceptions above. *)
val decode_pdfstream_onestage : Pdf.pdfdoc -> Pdf.pdfobject -> unit

(** Given a document and stream decode until there's an unknown decoder. May
return [Couldn'tDecodeStream]. *)
val decode_pdfstream_until_unknown : Pdf.pdfdoc -> Pdf.pdfobject -> unit

(** Supported encodings. *)
type encoding =
  | ASCIIHex
  | ASCII85
  | RunLength
  | Flate

(** Encode a PDF stream with an encoding. *)
val encode_pdfstream : Pdf.pdfdoc -> encoding -> Pdf.pdfobject -> unit

(**/**)

(* Given an [Io.input] with pointer at the first byte and an inline image
stream dictionary, decode the first decoder and its predictor. Return the data,
or [None] if this decoder isn't supported but the data pointer has been left in
the right place. The exceptions above can both be raised, in the case of bad
data or a completely unknown encoding. *)
val decode_from_input : Io.input -> Pdf.pdfobject -> Utility.bytestream option

