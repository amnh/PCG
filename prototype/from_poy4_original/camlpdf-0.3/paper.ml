(* \chaptertitle{Paper}{Standard Media Sizes} *)

(* \intf Type for paper sizes --- unit, width, height. *)
type papersize = Units.unit * float * float

(* \intf Make a paper size landscape *)
let landscape (u, w, h) = u, h, w

(* \intf European `A' sizes. *)
let a0 = Units.Millimetre, 841., 1189.
and a1 = Units.Millimetre, 594., 841.
and a2 = Units.Millimetre, 420., 594.
and a3 = Units.Millimetre, 297., 420.
and a4 = Units.Millimetre, 210., 297.
and a5 = Units.Millimetre, 148., 210.
and a6 = Units.Millimetre, 105., 148.
and a7 = Units.Millimetre, 74., 105.
and a8 = Units.Millimetre, 52., 74.

(* \intf US Imperial sizes. *)
let usletter = Units.Inch, 8.5, 11.

