(** Extract Images *)
type pixel_layout =
  | BPP1
  | BPP8
  | BPP24
  | BPP48

(* FIXME: We need to deal with decode and other things for JPEG, if we're not
going to decode them. *)
type image =
  | JPEG of Utility.bytestream
  | JPEG2000 of Utility.bytestream
  | JBIG2 of Utility.bytestream
  | Raw of int * int * pixel_layout * Utility.bytestream

(** Given a pdf document, resources dictionary and a stream representing an
image, return a triple : width, height, and a stream of (width * height * 3)
bytes RGBRGB etc. In all instances, if JPEG or JPEG2000 or JBIG2 is the compression
 method, data is returned still encoded. *)
val get_image_24bpp :
  Pdf.pdfdoc -> Pdf.pdfobject -> Pdf.pdfobject -> image

(*i (** Similarly, but a bit-packed, rows-padded-to-bytes bytestream. *)
val get_image_1bpp :
  Pdf.pdfdoc -> Pdf.pdfobject -> Pdf.pdfobject -> image

  val get_image_8bpp

(** Similarly, but a 48bpp image is returned. *)
val get_image_48bpp :
  Pdf.pdfdoc -> Pdf.pdfobject -> Pdf.pdfobject -> i*)

