(** Media Sizes *)

(** A paper size consists of its unit, width and height. *)
type papersize = Units.unit * float * float

(** Flip a paper size between landscape and portrait, swapping its dimensions. *)
val landscape : papersize -> papersize

val a0 : papersize
val a1 : papersize
val a2 : papersize
val a3 : papersize
val a4 : papersize
val a5 : papersize
val a6 : papersize
val a7 : papersize
val a8 : papersize
(** ISO A series paper sizes, portrait. *)

val usletter : papersize
(** United States paper sizes. portrait. *)

