(** Units and unit conversion *)

(** The type of units *)
type unit = PdfPoint | Inch | Centimetre | Millimetre | Pixel

(** [convert d u u'] produces a convertor converting from unit [u] to [u'] with
dpi [d] *)
val convert : float -> unit -> unit -> float -> float

