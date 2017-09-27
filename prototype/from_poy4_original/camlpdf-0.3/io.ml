(* \chaptertitle{IO}{General Input and Output} *)
open Utility

(* We use 64-bit sized files as standard. *)
open LargeFile

(* \section{Defining and creating input and output functions} *)

(* \intf A general type for input functions. This allows paramaterization over
channels, strings, bigarrays etc. *)
type input =
  {pos_in : unit -> int64;
   seek_in : int64 -> unit;
   input_char : unit -> char;
   input_byte : unit -> int;
   in_channel_length : unit -> int64;
   set_offset : int64 -> unit}

(* \intf A general type for output functions, allowing parameterisation as above. *)
type output =
  {pos_out : unit -> int64;
   seek_out : int64 -> unit;
   output_char : char -> unit;
   output_byte : int -> unit;
   out_channel_length : unit -> int64}

(* \intf Standard End-of-input error. This usually replaces [Pervasives.End_of_file],
but it depends upon which set of input functions are in use. *)
exception EndOfInput

(* \intf Standard End-of-output error, signaling that no more data could be written.
Usually an exceptional circumstance. *)
exception EndOfOutput

(* \intf Create input functions from a channel. *)
let input_of_channel ch =
  let offset = ref 0L in
    {pos_in =
       (fun () ->
          try i64sub (pos_in ch) !offset with
            End_of_file -> raise EndOfInput);
     seek_in =
       (fun x -> seek_in ch (i64add x !offset));
     input_char =
       (fun () ->
          try input_char ch with
            End_of_file -> raise EndOfInput);
     input_byte =
       (fun () ->
          try input_byte ch with
            End_of_file -> raise EndOfInput);
     in_channel_length =
       (fun () -> in_channel_length ch);
     set_offset =
       (fun o -> offset := o)
    }

(* \intf Create input functions from a [Utility.stream]. *)
let input_of_stream s =
  let input_int () =
    if s.pos > stream_size s.data - 1
      then
        begin
          s.pos <- s.pos + 1;
          raise EndOfInput
        end
      else
        begin
          s.pos <- s.pos + 1;
          s.data.{s.pos - 1}
        end
  in
    {pos_in =
       (fun () -> Int64.of_int s.pos);
     seek_in =
       (fun p -> s.pos <- Int64.to_int p);
     input_char =
       (fun () -> char_of_int (input_int ()));
     input_byte =
       input_int;
     in_channel_length =
       (fun () -> Int64.of_int (stream_size s.data));
     set_offset =
       (* FIXME. *)
       (fun _ -> ())
    }

(* \intf Create input functions from a [Utility.bytestream]. *)
let input_of_bytestream b =
  input_of_stream {pos = 0; data = b}

(* \intf Output functions over channels *)
let output_of_channel ch =
  {pos_out = (fun () -> pos_out ch);
   seek_out = seek_out ch;
   output_char = output_char ch;
   output_byte = output_byte ch;
   out_channel_length = (fun () -> out_channel_length ch)}

(* \intf Output functions over streams. If data is written past the end of a stream,
we extend the stream to that point plus one-third of that (new) size. Note that
this has an implication upon mixing reading and writing: the stream will have
junk in the extended section and will be longer than that which has been
written. *)
let output_of_stream s =
  let highest_written = ref 0L in
    let output_int i =
      if s.pos > stream_size s.data - 1
        then
          let newstream = mkstream (s.pos * 2 - s.pos / 2) in
            for x = 0 to stream_size s.data - 1 do
              newstream.{x} <- s.data.{x}
            done;
            newstream.{s.pos} <- i;
            highest_written := i64max !highest_written (i64ofi s.pos);
            s.pos <- s.pos + 1;
            s.data <- newstream
        else
          begin
            highest_written := i64max !highest_written (i64ofi s.pos);
            s.data.{s.pos} <- i;
            s.pos <- s.pos + 1
          end
    in
        {pos_out =
           (fun () -> Int64.of_int s.pos);
         seek_out =
           (fun p -> s.pos <- Int64.to_int p);
         output_char =
           (fun c -> output_int (int_of_char c));
         output_byte =
           output_int;
         out_channel_length =
           (fun () -> i64succ !highest_written)}

(* \section{Utility functions} *)

(* \intf Nudge forward one character. *)
let nudge i =
  ignore (i.input_char ())

(* \intf Read one character behind the current position, and reposition ourselves on
that character. *)
let read_char_back i =
  let pos = i.pos_in () in
    i.seek_in (Int64.pred pos);
    let chr = i.input_char () in
      i.seek_in (Int64.pred pos);
      chr

(* \intf Go back one character in a file. *)
let rewind i =
  ignore (read_char_back i)

(* \intf Read a character, leaving the position unchanged. *)
let peek_char i =
  let r = i.input_char () in
    rewind i; r

(* \intf Read a byte, leaving the position unchanged. *)
let peek_byte i =
  let r = i.input_byte () in
    rewind i; r

(* \intf Output a string. *)
let output_string o s =
  String.iter o.output_char s

(* \intf Make a bytestream of an input channel. *)
let bytestream_of_input_channel ch =
  let fi = input_of_channel ch in
    let size = i64toi (fi.in_channel_length ()) in
      let s = mkstream size in
        for x = 1 to size do
          s.{x - 1} <- fi.input_byte ()
        done;
        s

(* \intf Save a bytestream to a channel. *)
let bytestream_to_output_channel ch data =
  for x = 1 to stream_size data do
    output_byte ch data.{x - 1}
  done

(* Like [Pervasives.read_line] *) 
let read_line i =
  (* Raise EndOfInput if at end *)
  ignore (i.input_char ());
  rewind i;
  let char = ref ' ' 
  and chars = ref [] in
    try
      while char := i.input_char (); !char <> '\n' do
        chars =:: !char
      done;
      implode (rev !chars)
    with
      EndOfInput -> implode (rev !chars)

(* \section{Reading MSB-first Bit streams} *)

(*\intf The type of bit (MSB first) streams. *)
type bitstream =
  {input : input; (* The input from which bits are taken. It is advanced a byte at a time *)
   mutable currbyte : int; (* Current byte value from input *)
   mutable bit : int; (* Mask for getting the next bit (128, 64,... 2, 1 or 0 = none left) *)
   mutable bitsread : int (* A count of the number of bits read since inception. Debug use only *)}

(* \intf Make a [bitstream] from an [input]. *) 
let bitstream_of_input i =
  {currbyte = 0;
   bit = 0;
   bitsread = 0;
   input = i}

(* For debug only.... *)
let input_in_bitstream b =
  b.input

(* \intf Get a single bit. *)
let rec getbit b =
  if b.bit = 0 then
    begin
      b.currbyte <- b.input.input_byte ();
      b.bit <- 128;
      getbit b
    end
  else
    let r = b.currbyte land b.bit > 0 in
      b.bitsread <- b.bitsread + 1;
      b.bit <- b.bit / 2;
      r

(* \intf Get a bit as an integer, set = 1, unset = 0 *)
let getbitint i =
  if getbit i then 1 else 0

(* \intf Align on a byte boundary. *)
let align b =
  if b.bit > 0 then b.bitsread <- (b.bitsread / 8 + 1)  * 8;
  b.bit <- 0

(* Get [n] (up to 32) bits from [b], returned as an [int32], taken highest bit
first. Getting 0 bits gets the value 0.\SPEED{Far too slow}. *)
let char_of_bool = function true -> '1' | false -> '0'

let getval_32 b n =
  if n < 0 then raise (Invalid_argument "Io.getval_32") else
    if n = 0 then 0l else
      let bits = manyunique (mkunit getbit b) n in
        Int32.of_string ("0b" ^ implode (map char_of_bool bits))

(* \section{Writing MSB-first bit streams} *)

(* The type: A current byte, the position in the byte (0 = nothing in it, 7 =
almost full), and the list (in reverse order) of full bytes so far *)
type bitstream_write =
  {mutable wcurrbyte : int;
   mutable wbit : int;
   mutable bytes : int list}

let make_write_bitstream () =
  {wcurrbyte = 0;
   wbit = 0;
   bytes = []}

let copy_write_bitstream b =
  let b' = make_write_bitstream () in
    b'.wcurrbyte <- b.wcurrbyte;
    b'.wbit <- b.wbit;
    b'.bytes <- b.bytes;
    b'

let print_bitstream b =
  Printf.printf "wcurrbyte = %i, wbit = %i, %i bytes output\n"
  b.wcurrbyte b.wbit (length b.bytes)

(* Put a single bit into bitstream [b]*)
let putbit b bit =
  assert (bit = 0 || bit = 1);
  match b.wbit with
  | 7 ->
      b.bytes <- (b.wcurrbyte lor bit) :: b.bytes;
      b.wbit <- 0;
      b.wcurrbyte <- 0
  | _ ->
      b.wbit <- b.wbit + 1;
      b.wcurrbyte <- b.wcurrbyte lor (bit lsl (8 - b.wbit))

let putbool b bit =
  putbit b ((function false -> 0 | true -> 1) bit)

(* Put a multi-bit value [n] of bits [bs] (given as an [int32]) into bitstream [b]. *)
let rec putval b bs n =
  if bs < 0 || bs > 32 then raise (Invalid_argument "putval");
  match bs with
  | 0 -> ()
  | _ ->
      let bit =
        if land32 n (i32ofi (1 lsl (bs - 1))) > 0l then 1 else 0
      in
        putbit b bit;
        putval b (bs - 1) n

(* Align on a byte boundary, writing zeroes. *)
let align_write b =
  if b.wbit > 0 then
    for x = 1 to 8 - b.wbit do
      putbit b 0
    done

(* Get the output out. *)
let bytestream_of_write_bitstream b =
  align_write b;
  bytestream_of_list (rev b.bytes)

(* Return a list of booleans, representing (in order) the bits *)
let bits_of_write_bitstream b =
  let numbits = length b.bytes * 8 + b.wbit
  and bytestream = bytestream_of_write_bitstream b
  and bits = ref [] in
    let bitstream = bitstream_of_input (input_of_bytestream bytestream) in
      for x = 1 to numbits do
        bits =:: getbit bitstream
      done;
      rev !bits

(* Same, but from a list *)
let join_write_bitstreams ss =
  let c = make_write_bitstream () in
    iter
      (putbool c)
      (flatten (map bits_of_write_bitstream ss));
    c

(* Append b to a. Inputs unaltered. *)
let write_bitstream_append a b =
  join_write_bitstreams [a; b]

(* Same, but align at the join. *)
let write_bitstream_append_aligned a b =
  let c = copy_write_bitstream a in
    align_write c;
    write_bitstream_append c b

