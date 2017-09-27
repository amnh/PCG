(** Zlib interface *)

(** The module [Gz] redefines most of the I/O functions of
   [Pervasives] to allow I/O on compressed files (using the
   [zlib] library). It doesn't use [zlib] lower-level
   functions, so there might be some way to write a more efficient
   interface ; this one is however, very small (and didn't take much
   time to write!). *)

(** {2 Datatypes & exceptions} *)

type in_channel
type out_channel

(** When exception [Error] is raised, the channel is automatically closed. *)
exception Error of string

val version : string


(** {2 Output funcions} *)
  
type zstrategy = | Default | Filtered | Huffman_only

(** [open_out] opens the given filename for writing. 
   @param compression specifies the level of compression : 0
   is no compression, 1 is fastest , 9 is best but slowest. The default
   is a compromise (6, I think). 
   @param strategy refer to the [zlib] manual (i.e the header 
   file [zlib.h]) *)
val open_out : ?compression:int -> ?strategy:zstrategy ->
  string -> out_channel

(** [setparams] modifies the two parameters of an opened channel *)
external setparams : out_channel -> compression:int -> strategy:zstrategy -> unit
  = "mlgz_gzsetparams"


(** These functions output substrings, strings, char or char value
  ([int] argument). The [external] ones use the [zlib]
  functions, the usual Caml ones are only wrappers around
  those. [output_string] and [write] will correctly handle
  null characters embedded in Caml strings. [output_value] uses
  [Marshal] module.
*)

external write : out_channel -> buf:string -> pos:int -> len:int -> unit
  = "mlgz_gzwrite"
external output_string : out_channel -> string -> unit
  = "mlgz_gzputs"
external output_char : out_channel -> char -> unit 
  = "mlgz_gzputc"
external output_byte : out_channel -> int -> unit 
  = "mlgz_gzputc"
val output_newline : out_channel -> unit
val output_endline : out_channel -> string -> unit
val output_value : out_channel -> 'a -> unit

type flush = | Sync_flush | Full_flush | Finish_flush
val flush : ?flush:flush -> out_channel -> unit
(** The [flush] function should be used with caution because it can
   degrade compression. 
   @param flush defaults to [Sync_flush].
*)

(** [seek_out] set the position of the next write operation on the
   channel. Only forward seeks are supported; [seek_out] then
   compresses a sequence of zeroes up to the new starting position. It
   @raise Invalid_argument if called with a negative offset.
*)
val seek_out : out_channel -> offset:int -> unit

val pos_out  : out_channel -> int

(** [close_out] flushes all pending output if necessary, closes the
   compressed file and deallocates all the (de)compression state. Any
   subsequent use of the channel will raise an [Error] exception. *)
external close_out : out_channel -> unit
  = "mlgz_gzclose"


(** {2 Input functions} *)

val open_in  : string -> in_channel

(** [read] reads characters from the stream and returns the number
   of bytes actually read ; it does not raise [End_of_file]. 
   [input_char] and [input_line] should
   appropriately raise [End_of_file] if necessary.
   [input_value] uses [Marshal] module.
*)

external read : in_channel -> buf:string -> pos:int -> len:int -> int
  = "mlgz_gzread"
external input_char : in_channel -> char 
  = "mlgz_gzgetc"
val input_line : in_channel -> string
val input_value : in_channel -> 'a

external rewind : in_channel -> unit
  = "mlgz_gzrewind"

val seek_in : in_channel -> offset:int -> unit
(** The [seek_in] function is emulated but can be extremely slow. *)

val pos_in  : in_channel -> int

external close_in : in_channel -> unit
  = "mlgz_gzclose"

(** {2 In-memory compression} *)

(** These functions compress and uncompress from a string to another 
   string. *)

external compress : ?compression:int -> string -> pos:int -> len:int -> string
  = "mlgz_compress"

external uncompress : string -> pos:int -> len:int -> string
  = "mlgz_uncompress"
