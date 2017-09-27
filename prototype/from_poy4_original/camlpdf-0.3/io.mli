(** Generic Input/Ouput *)

(** Type for inputs. *)
type input =
  {pos_in : unit -> int64;
   seek_in : int64 -> unit;
   input_char : unit -> char;
   input_byte : unit -> int;
   in_channel_length : unit -> int64;
   set_offset : int64 -> unit}

(** Type for outputs *)
type output =
  {pos_out : unit -> int64;
   seek_out : int64 -> unit;
   output_char : char -> unit;
   output_byte : int -> unit;
   out_channel_length : unit -> int64}

(** Raised upon reading past the end of an input. *)
exception EndOfInput

(** Raised upon writing past the end of an output. *)
exception EndOfOutput

val output_of_stream : Utility.stream -> output

val output_of_channel : out_channel -> output

val input_of_channel : in_channel -> input

val input_of_stream : Utility.stream -> input

val input_of_bytestream : Utility.bytestream -> input
(** Conversion functions. *)

(** Move forward one character *)
val nudge : input -> unit

(** Move backward one. *)
val rewind : input -> unit

(** Look at the next character without advancing the pointer. *)
val peek_char : input -> char

(** Look at the next byte without advancing the pointer. *)
val peek_byte : input -> int

(** Output a string. *)
val output_string : output -> string -> unit

(** Read the previous character, moving the pointer back one. *)
val read_char_back : input -> char

(** Read a line. cf Pervasives.read_line. *)
val read_line : input -> string

(** Extract a bytestream from an input or output. *)
val bytestream_of_input_channel :
  in_channel -> Utility.bytestream

(** Write a bytestream to an output channel *)
val bytestream_to_output_channel :
  out_channel -> Utility.bytestream -> unit

(** {2 Read Bit streams} *)

(** The type of MSB-first bitstreams *)
type bitstream

(** Make a bitstream from an input. *)
val bitstream_of_input : input -> bitstream

(** Get a bit *)
val getbit : bitstream -> bool

(** Ditto but as an integer *)
val getbitint : bitstream -> int

(** Align on a byte boundary *)
val align : bitstream -> unit

(** Get a 32-bit value *)
val getval_32 : bitstream -> int -> int32

(** {2 Write Bit streams} *)

(** The type of MSB-first bitstreams for writing. *)
type bitstream_write

(** Return a new write bistream. *)
val make_write_bitstream : unit -> bitstream_write

(** Debug printer. *)
val print_bitstream : bitstream_write -> unit

(** Build a bytestream from a write bitstream, padding with zeroes. *)
val bytestream_of_write_bitstream : bitstream_write -> Utility.bytestream

(** Put a single bit, 0 or 1. *)
val putbit : bitstream_write -> int -> unit

(** Same, but input is boolean *)
val putbool : bitstream_write -> bool -> unit

(** Put a multi-bit value (given as an int32) containing the given number of
useful bits into a bitstream *)
val putval : bitstream_write -> int -> int32 -> unit

(** Byte-align. *)
val align_write : bitstream_write -> unit

(** Append two write bitstreams *)
val write_bitstream_append :
  bitstream_write -> bitstream_write -> bitstream_write

(** Same, but align at boundary *)
val write_bitstream_append_aligned :
  bitstream_write -> bitstream_write -> bitstream_write

(** Join several write bitstreams *)
val join_write_bitstreams :
  bitstream_write list -> bitstream_write

(**/**)

(* for debug only *)
val input_in_bitstream : bitstream -> input

