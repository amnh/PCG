type in_channel
type out_channel

type zstrategy = | Default | Filtered | Huffman_only

exception Error of string

let _ = Callback.register_exception "mlgz_exn" (Error "")

external libversion : unit -> string
  = "mlgz_zlibversion"

let version = libversion ()


(*  Utility functions *)

external open_gen : string -> string -> 'a 
  = "mlgz_gzopen_gen"

let open_out ?compression ?(strategy=Default) name = 
  let comp_mod = match compression with
    | None -> "" 
    | Some n when ( 0<=n && n<=9) -> string_of_int n
    | _ -> invalid_arg "Gz.open_out: bad compression level"
  and strat_mod = match strategy with
    | Default -> ""
    | Filtered -> "f"
    | Huffman_only -> "h"
  in
    (open_gen name ("wb" ^ comp_mod ^ strat_mod) : out_channel)

let open_in name = 
  (open_gen name "rb" : in_channel)

external setparams : out_channel -> compression:int -> strategy:zstrategy -> unit
  = "mlgz_gzsetparams"

external read : in_channel -> buf:string -> pos:int -> len:int -> int
  = "mlgz_gzread"
  
external write : out_channel -> buf:string -> pos:int -> len:int -> unit
  = "mlgz_gzwrite"

external output_string : out_channel -> string -> unit
  = "mlgz_gzputs"

external input_scan_line : in_channel -> buf:string -> int
  = "mlgz_input_scan_line"

let input_line chan =
  let buff_size = 4096 in
  let buffer = Buffer.create buff_size in
  let c_buf  = String.create (buff_size + 1) in
  let read_by = ref buff_size in
    while
      !read_by = buff_size
    do
      read_by := input_scan_line chan c_buf ;
      if !read_by > 0 && c_buf.[!read_by-1]='\n' then decr read_by ;
      Buffer.add_substring buffer c_buf 0 !read_by
    done ;
    Buffer.contents buffer

external output_char : out_channel -> char -> unit 
  = "mlgz_gzputc"

external output_byte : out_channel -> int -> unit 
  = "mlgz_gzputc"

let output_newline zfile =
  output_char zfile '\n'

let output_endline zfile str =
  output_string zfile str ;
  output_newline zfile

external input_char : in_channel -> char 
  = "mlgz_gzgetc"

external rewind : in_channel -> unit
  = "mlgz_gzrewind"

external close_in : in_channel -> unit
  = "mlgz_gzclose"

external close_out : out_channel -> unit
  = "mlgz_gzclose"

let output_value zfile v =
  output_string zfile
    (Marshal.to_string v [])

let input_value zfile =
  let h_size = Marshal.header_size in
  let buf = String.create h_size in
  let inp = read zfile buf 0 h_size in
    if inp < h_size then failwith "Gz.input_value" ;
    let t_size = Marshal.total_size buf 0 in
    let big_buf = String.create t_size in
      String.blit buf 0 big_buf 0 h_size ;
      let inp = read zfile big_buf h_size (t_size - h_size) in
	if inp < (t_size - h_size) then failwith "Gz.input_value" ;
	Marshal.from_string big_buf 0

type flush = | Sync_flush | Full_flush | Finish_flush
external flush : ?flush:flush -> out_channel -> unit
  = "mlgz_gzflush"

external seek_gen : 'a -> int -> bool -> unit
  = "mlgz_gzseek"

let seek_in zfile ~offset =
  seek_gen zfile offset false

let seek_out zfile ~offset =
  seek_gen zfile offset true

external pos_in : in_channel   -> int
  = "mlgz_gztell"

external pos_out : out_channel -> int
  = "mlgz_gztell"

(* in memory compression *)

external compress : ?compression:int -> string -> pos:int -> len:int -> string
  = "mlgz_compress"

external uncompress : string -> pos:int -> len:int -> string
  = "mlgz_uncompress"
