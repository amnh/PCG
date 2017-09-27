(* \chaptertitle{PDFCodec}{PDF compression and decompression} *)
open Utility
open Io
open Pdf

(* \section{Preliminaries} *)

(* Get the next non-whitespace character in a stream. *)
let rec get_streamchar skipped s =
  let chr = s.input_char () in
    if is_whitespace chr then
      begin
        incr skipped;
        get_streamchar skipped s
      end
    else chr

(* Same, but return an option type instead of raising an exception at end of
input. *)
let get_streamchar_option skipped s =
  try Some (get_streamchar skipped s) with
    EndOfInput -> None

(* \intf Raised if there was bad data. *)
exception Couldn'tDecodeStream of string

(* \intf Raised if the codec was not supported. *)
exception DecodeNotSupported

(* \section{ASCIIHex} *)

(* We build a list of decoded characters from the input stream, and then
convert this to the output stream. *)
let encode_ASCIIHex stream =
  let size = stream_size stream in
    let stream' = mkstream (size * 2 + 1) in
      stream'.{size * 2} <- int_of_char '>'; (*r ['>'] is end-of-data *)
      for p = 0 to size - 1 do
        let chars = explode (Printf.sprintf "%02X" stream.{p}) in
          stream'.{p * 2} <- int_of_char (hd chars);
          stream'.{p * 2 + 1} <- int_of_char (hd (tl chars))
      done;
      stream'

(* Decode ASCIIHex *)
let decode_ASCIIHex i =
  let output = ref []
  and enddata = ref false in
    try
      while not !enddata do
        let b = get_streamchar (ref 0) i in
          let b' = get_streamchar (ref 0) i in
            match b, b' with
            | '>', _ -> set enddata
            | ('0'..'9' | 'a'..'f' | 'A'..'F') as c, '>' ->
                output =::
                  char_of_int (Scanf.sscanf (implode [c; '0']) "%x" ident);
                set enddata
            | ('0'..'9' | 'a'..'f' | 'A'..'F' as c),
              ('0'..'9' | 'a'..'f' | 'A'..'F' as c') ->
                output =::
                  char_of_int (Scanf.sscanf (implode [c; c']) "%x" ident)
            | _ -> raise Not_found (*r Bad data. *)
      done;
      bytestream_of_charlist (rev !output)
    with
      | EndOfInput ->
          (* We ran out of data. This is a normal exit. *)
          bytestream_of_charlist (rev !output)
      | Not_found | Scanf.Scan_failure _  ->
          raise (Couldn'tDecodeStream "ASCIIHex")

(* \section{ASCII85}*)

(* Decode five characters. *)
let decode_5bytes (c1, c2, c3, c4, c5) =
  let d x p =
    i32mul (i32ofi (int_of_char x - 33)) (i32ofi (pow p 85))
  in
    let total =
      fold_left i32add 0l [d c1 4; d c2 3; d c3 2; d c4 1; d c5 0]
    in
      let extract t =
        char_of_int (i32toi (lsr32 (lsl32 total (24 - t)) 24))
      in
        extract 24, extract 16, extract 8, extract 0

(* Main function *)
let decode_ASCII85 i =
  let output = ref []
  and enddata = ref false
  and skipped = ref 0 in
    try
      while not !enddata do
        let c1 = get_streamchar_option skipped i in
        (*r Ignore any whitespace skipped before getting to the first char of
         interest. This prevents us sliding too much back and picking up z
         characters twice. *)
        skipped := 0;
        let c2 = get_streamchar_option skipped i in
        let c3 = get_streamchar_option skipped i in
        let c4 = get_streamchar_option skipped i in
        let c5 = get_streamchar_option skipped i in
          match c1, c2, c3, c4, c5 with
          | Some 'z', _, _, _, _ ->
              i.seek_in (i64sub (i.pos_in ()) (i64add 4L (i64ofi !skipped)));
              output := '\000'::'\000'::'\000'::'\000'::!output
          | Some ('!'..'u' as c1), Some ('!'..'u' as c2), Some ('!'..'u' as c3),
            Some ('!'..'u' as c4), Some ('!'..'u' as c5) ->
              let b1, b2, b3, b4 = decode_5bytes (c1, c2, c3, c4, c5) in
                output := b4::b3::b2::b1::!output
          | Some '~', Some '>', _, _, _ ->
              set enddata
          | Some ('!'..'u' as c1), Some ('!'..'u' as c2),
            Some '~', Some '>', _ ->
              let b1, b2, b3, b4 = decode_5bytes (c1, c2, '~', '>', '!') in
                set enddata; output := b1::!output
          | Some ('!'..'u' as c1), Some ('!'..'u' as c2), Some ('!'..'u' as c3),
            Some '~', Some '>' ->
              let b1, b2, _, _ = decode_5bytes (c1, c2, c3, '~', '>') in
                set enddata; output := b2::b1::!output
          | Some ('!'..'u' as c1), Some ('!'..'u' as c2), Some ('!'..'u' as c3),
            Some ('!'..'u' as c4), Some '~' ->
              let b1, b2, b3, _ = decode_5bytes (c1, c2, c3, c4, '~') in
                set enddata; output := b3::b2::b1::!output
          | _ -> raise EndOfInput
      done;
      bytestream_of_charlist (rev !output)
    with
      EndOfInput -> raise (Couldn'tDecodeStream "ASCII85")

(* Encode a single symbol set. *)
let encode_4bytes = function
  | [b1; b2; b3; b4] ->
      let ( * ) = Int64.mul
      and ( - ) = Int64.sub
      and ( / ) = Int64.div 
      and rem = Int64.rem in
        let numbers =
          [i64ofi (int_of_char b1) * i64ofi (pow 3 256);
           i64ofi (int_of_char b2) * i64ofi (pow 2 256);
           i64ofi (int_of_char b3) * i64ofi (pow 1 256);
           i64ofi (int_of_char b4) * i64ofi (pow 0 256)]
        in
          let t = fold_left Int64.add Int64.zero numbers
          and one85 = i64ofi (pow 1 85) and two85 = i64ofi (pow 2 85)
          and three85 = i64ofi (pow 3 85) and zero85 = i64ofi (pow 0 85)
          and four85 = i64ofi (pow 4 85) in
            let t, c5 = t - rem t one85, rem t one85 / zero85 in
              let t, c4 = t - rem t two85, rem t two85 / one85 in
                let t, c3 = t - rem t three85, rem t three85 / two85 in
                  let t, c2 = t - rem t four85, rem t four85 / three85 in
                    i64toi (t / four85), i64toi c2, i64toi c3, i64toi c4, i64toi c5
  | _ -> assert false

(* Encode a stream. *)
let encode_ASCII85 stream =
  let output = ref []
  and enddata = ref false
  and istream = input_of_bytestream stream in
    while not !enddata do
      let b1 = try Some (istream.input_char ()) with EndOfInput -> None in
      let b2 = try Some (istream.input_char ()) with EndOfInput -> None in
      let b3 = try Some (istream.input_char ()) with EndOfInput -> None in
      let b4 = try Some (istream.input_char ()) with EndOfInput -> None in
        match b1, b2, b3, b4 with
        | Some b1, Some b2, Some b3, Some b4 ->
            output := [b1; b2; b3; b4]::!output
        | Some b1, Some b2, Some b3, None ->
            set enddata; output := [b1; b2; b3]::!output
        | Some b1, Some b2, None, None ->
            set enddata; output := [b1; b2]::!output
        | Some b1, None, None, None ->
            set enddata; output := [b1]::!output
        | None, _, _, _ -> set enddata
        | _ -> assert false
    done;
    let fix k = char_of_int (k + 33) in
      let charlists' =
        rev_map
          (fun l ->
             let len = length l in
               if len < 4
               then
                 let l' = l @ (many '\000' (4 - len)) in
                   let c1, c2, c3, c4, c5 = encode_4bytes l' in
                     take [fix c1; fix c2; fix c3; fix c4; fix c5] (len + 1)
               else
                   let c1, c2, c3, c4, c5 = encode_4bytes l in
                     if c1 + c2 + c3 + c4 + c5 = 0
                       then ['z']
                       else [fix c1; fix c2; fix c3; fix c4; fix c5])
          !output
        in
          bytestream_of_charlist (flatten charlists' @ ['~'; '>'])

(* \section{Flate} *)

(* Make a bytestream from a list of strings by taking the contents, in order
from the items, in order. *)
let bytestream_of_strings strings =
  let total_length =
    fold_left ( + ) 0 (map String.length strings)
  in
    let s = mkstream total_length
    and pos = ref 0 in
      iter
        (fun str ->
           for x = 0 to String.length str - 1 do
             s.{!pos} <- int_of_char str.[x]; incr pos
           done)
        strings;
      s

let flate_process f data =
  let strings = ref []
  and pos = ref 0
  and inlength = stream_size data in
    let input =
      (fun buf ->
         let s = String.length buf in
           let towrite = min (inlength - !pos) s in
             for x = 0 to towrite - 1 do
               buf.[x] <- char_of_int data.{!pos}; incr pos
             done;
             towrite)
    and output =
      (fun buf length ->
         if length > 0 then strings =:: String.sub buf 0 length)
    in
      f input output;
      bytestream_of_strings (rev !strings)

(* When decoding from an input (for an inline image), we proceed one byte at a
time so as to leave the input pointer at the correct place. *)
let decode_flate_input i =
  let strings = ref [] in
    let input =
      (fun buf ->
         let s = String.length buf in
           if s > 0 then
             begin buf.[0] <- i.input_char (); 1 end
           else 0)
    and output =
      (fun buf length ->
         if length > 0 then strings =:: String.sub buf 0 length)
    in
      Zlib.uncompress input output;
      bytestream_of_strings (rev !strings)

let encode_flate stream =
  flate_process Zlib.compress stream

let decode_flate stream =
  try flate_process Zlib.uncompress stream with
    Zlib.Error (a, b) -> raise (Couldn'tDecodeStream "Flate")

(* \section{LZW} *)

(* Decode LZW. *)
let decode_lzw early i =
  let prefix_code = Array.make 4096 0
  and append_character = Array.make 4096 0
  and bit_count = ref 0
  and bit_buffer = ref 0l
  and endflush = ref 4
  and code_length = ref 9
  and next_code = ref 258
  and new_code = ref 0
  and old_code = ref 256
  and character = ref 0 in
    let rec decode_string code str =
      if code > 255 then
        decode_string prefix_code.(code) (append_character.(code)::str)
      else
        code::str
    and input_code stream =
      while !bit_count <= 24 do
        let streambyte =
          try stream.input_byte () with
            EndOfInput ->
              if !endflush = 0 then raise EndOfInput else (decr endflush; 0)
        in
          bit_buffer := lor32 !bit_buffer (lsl32 (i32ofi streambyte) (24 - !bit_count));
          bit_count += 8
      done;
      let result = Int32.to_int (lsr32 !bit_buffer (32 - !code_length)) in
        bit_buffer := lsl32 !bit_buffer !code_length;
        bit_count -= !code_length;
        result
    and strip_cleartable_codes stream =
      while !old_code = 256 do
        old_code := input_code stream
      done
    and reset_table () =
      next_code := 258;
      code_length := 9;
      old_code := 256
    in
      match peek_byte i with 257 -> mkstream 0 | _ ->
        bit_count := 0; bit_buffer := 0l;
        endflush := 4; reset_table ();
        let outstream_data = {pos = 0; data = mkstream 16034} in
          let outstream = output_of_stream outstream_data
          and finished = ref false in
            strip_cleartable_codes i;
            match !old_code with
            | 257 -> mkstream 0
            | _ ->
                character := !old_code;
                outstream.output_byte !old_code;
                while not !finished do
                  new_code := input_code i;
                  match !new_code with
                  | 257 -> set finished
                  | 256 ->
                     reset_table ();
                     set_array prefix_code 0;
                     set_array append_character 0;
                     strip_cleartable_codes i;
                     character := !old_code;
                     outstream.output_byte !old_code
                  | _ ->
                    let chars =
                      if !new_code >= !next_code
                        then (decode_string !old_code []) @ [!character]
                        else decode_string !new_code []
                    in
                      character := hd chars;
                      iter outstream.output_byte chars;
                      prefix_code.(!next_code) <- !old_code;
                      append_character.(!next_code) <- !character;
                      incr next_code;
                      old_code := !new_code;
                      match !next_code + early with
                      | 512 | 1024 | 2048 -> incr code_length
                      | _ -> ()
                done;
                let out = mkstream outstream_data.pos in
                  for x = 0 to stream_size out - 1 do
                    out.{x} <- outstream_data.data.{x};
                  done;
                  out

(* \section{CCITT} *)          
(* Decode a CCITT-encoded stream. Parameter names:
  \begin{itemize}
     \item [eol] -- /EndOfLine
     \item [eba] -- /EncodedByteAlign
     \item [eob] -- /EndOfBlock
     \item [bone] -- /BlackIs1
     \item [dra] -- /DamagedRowsBeforeError
     \item [c] -- /Columns
     \item [r] -- /Rows
  \end{itemize}
*)

let rec read_white_code i =
  let a = getbitint i in
  let b = getbitint i in
  let c = getbitint i in
  let d = getbitint i in
    match a, b, c, d with
    | 0, 1, 1, 1 -> 2
    | 1, 0, 0, 0 -> 3
    | 1, 0, 1, 1 -> 4
    | 1, 1, 0, 0 -> 5
    | 1, 1, 1, 0 -> 6
    | 1, 1, 1, 1 -> 7
    | _ ->
  let e = getbitint i in
    match a, b, c, d, e with
    | 1, 0, 0, 1, 1 -> 8
    | 1, 0, 1, 0, 0 -> 9
    | 0, 0, 1, 1, 1 -> 10
    | 0, 1, 0, 0, 0 -> 11
    | 1, 1, 0, 1, 1 -> 64 + read_white_code i
    | 1, 0, 0, 1, 0 -> 128 + read_white_code i
    | _ ->
  let f = getbitint i in
    match a, b, c, d, e, f with
    | 0, 0, 0, 1, 1, 1 -> 1
    | 0, 0, 1, 0, 0, 0 -> 12
    | 0, 0, 0, 0, 1, 1 -> 13
    | 1, 1, 0, 1, 0, 0 -> 14
    | 1, 1, 0, 1, 0, 1 -> 15
    | 1, 0, 1, 0, 1, 0 -> 16
    | 1, 0, 1, 0, 1, 1 -> 17
    | 0, 1, 0, 1, 1, 1 -> 192 + read_white_code i
    | 0, 1, 1, 0, 0, 0 -> 1664 + read_white_code i
    | _ ->
  let g = getbitint i in
    match a, b, c, d, e, f, g with
    | 0, 1, 0, 0, 1, 1, 1 -> 18
    | 0, 0, 0, 1, 1, 0, 0 -> 19
    | 0, 0, 0, 1, 0, 0, 0 -> 20
    | 0, 0, 1, 0, 1, 1, 1 -> 21
    | 0, 0, 0, 0, 0, 1, 1 -> 22
    | 0, 0, 0, 0, 1, 0, 0 -> 23
    | 0, 1, 0, 1, 0, 0, 0 -> 24
    | 0, 1, 0, 1, 0, 1, 1 -> 25
    | 0, 0, 1, 0, 0, 1, 1 -> 26
    | 0, 1, 0, 0, 1, 0, 0 -> 27
    | 0, 0, 1, 1, 0, 0, 0 -> 28
    | 0, 1, 1, 0, 1, 1, 1 -> 256 + read_white_code i
    | _ ->
  let h = getbitint i in
    match a, b, c, d, e, f, g, h with
    | 0, 0, 1, 1, 0, 1, 0, 1 -> 0
    | 0, 0, 0, 0, 0, 0, 1, 0 -> 29
    | 0, 0, 0, 0, 0, 0, 1, 1 -> 30
    | 0, 0, 0, 1, 1, 0, 1, 0 -> 31
    | 0, 0, 0, 1, 1, 0, 1, 1 -> 32
    | 0, 0, 0, 1, 0, 0, 1, 0 -> 33
    | 0, 0, 0, 1, 0, 0, 1, 1 -> 34
    | 0, 0, 0, 1, 0, 1, 0, 0 -> 35
    | 0, 0, 0, 1, 0, 1, 0, 1 -> 36
    | 0, 0, 0, 1, 0, 1, 1, 0 -> 37
    | 0, 0, 0, 1, 0, 1, 1, 1 -> 38
    | 0, 0, 1, 0, 1, 0, 0, 0 -> 39
    | 0, 0, 1, 0, 1, 0, 0, 1 -> 40
    | 0, 0, 1, 0, 1, 0, 1, 0 -> 41
    | 0, 0, 1, 0, 1, 0, 1, 1 -> 42
    | 0, 0, 1, 0, 1, 1, 0, 0 -> 43
    | 0, 0, 1, 0, 1, 1, 0, 1 -> 44
    | 0, 0, 0, 0, 0, 1, 0, 0 -> 45
    | 0, 0, 0, 0, 0, 1, 0, 1 -> 46
    | 0, 0, 0, 0, 1, 0, 1, 0 -> 47
    | 0, 0, 0, 0, 1, 0, 1, 1 -> 48
    | 0, 1, 0, 1, 0, 0, 1, 0 -> 49
    | 0, 1, 0, 1, 0, 0, 1, 1 -> 50
    | 0, 1, 0, 1, 0, 1, 0, 0 -> 51
    | 0, 1, 0, 1, 0, 1, 0, 1 -> 52
    | 0, 0, 1, 0, 0, 1, 0, 0 -> 53
    | 0, 0, 1, 0, 0, 1, 0, 1 -> 54
    | 0, 1, 0, 1, 1, 0, 0, 0 -> 55
    | 0, 1, 0, 1, 1, 0, 0, 1 -> 56
    | 0, 1, 0, 1, 1, 0, 1, 0 -> 57
    | 0, 1, 0, 1, 1, 0, 1, 1 -> 58
    | 0, 1, 0, 0, 1, 0, 1, 0 -> 59
    | 0, 1, 0, 0, 1, 0, 1, 1 -> 60
    | 0, 0, 1, 1, 0, 0, 1, 0 -> 61
    | 0, 0, 1, 1, 0, 0, 1, 1 -> 62
    | 0, 0, 1, 1, 0, 1, 0, 0 -> 63
    | 0, 0, 1, 1, 0, 1, 1, 0 -> 320 + read_white_code i
    | 0, 0, 1, 1, 0, 1, 1, 1 -> 384 + read_white_code i
    | 0, 1, 1, 0, 0, 1, 0, 0 -> 448 + read_white_code i
    | 0, 1, 1, 0, 0, 1, 0, 1 -> 512 + read_white_code i
    | 0, 1, 1, 0, 1, 0, 0, 0 -> 576 + read_white_code i
    | 0, 1, 1, 0, 0, 1, 1, 1 -> 640 + read_white_code i
    | _ ->
  let j = getbitint i in
    match a, b, c, d, e, f, g, h, j with
    | 0, 1, 1, 0, 0, 1, 1, 0, 0 -> 704 + read_white_code i
    | 0, 1, 1, 0, 0, 1, 1, 0, 1 -> 768 + read_white_code i
    | 0, 1, 1, 0, 1, 0, 0, 1, 0 -> 832 + read_white_code i
    | 0, 1, 1, 0, 1, 0, 0, 1, 1 -> 896 + read_white_code i
    | 0, 1, 1, 0, 1, 0, 1, 0, 0 -> 960 + read_white_code i
    | 0, 1, 1, 0, 1, 0, 1, 0, 1 -> 1024 + read_white_code i
    | 0, 1, 1, 0, 1, 0, 1, 1, 0 -> 1088 + read_white_code i
    | 0, 1, 1, 0, 1, 0, 1, 1, 1 -> 1152 + read_white_code i
    | 0, 1, 1, 0, 1, 1, 0, 0, 0 -> 1216 + read_white_code i
    | 0, 1, 1, 0, 1, 1, 0, 0, 1 -> 1280 + read_white_code i
    | 0, 1, 1, 0, 1, 1, 0, 1, 0 -> 1344 + read_white_code i
    | 0, 1, 1, 0, 1, 1, 0, 1, 1 -> 1408 + read_white_code i
    | 0, 1, 0, 0, 1, 1, 0, 0, 0 -> 1472 + read_white_code i
    | 0, 1, 0, 0, 1, 1, 0, 0, 1 -> 1536 + read_white_code i
    | 0, 1, 0, 0, 1, 1, 0, 1, 0 -> 1600 + read_white_code i
    | 0, 1, 0, 0, 1, 1, 0, 1, 1 -> 1728 + read_white_code i
    | _ ->
  let k = getbitint i in
  let l = getbitint i in
    match a, b, c, d, e, f, g, h, j, k, l with
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 -> 1792 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0 -> 1856 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1 -> 1920 + read_white_code i
    | _ ->
  let m = getbitint i in
    match a, b, c, d, e, f, g, h, j, k, l, m with
    | 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 -> ~-1
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0 -> 1984 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1 -> 2048 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0 -> 2112 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1 -> 2176 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0 -> 2240 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1 -> 2304 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0 -> 2368 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1 -> 2432 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0 -> 2496 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1 -> 2560 + read_white_code i
    | _ -> raise (Failure "bad white code")

let rec read_black_code i =
  let a = getbitint i in
  let b = getbitint i in
    match a, b with
    | 1, 1 -> 2
    | 1, 0 -> 3
    | _ ->
  let c = getbitint i in
    match a, b, c with
    | 0, 1, 0 -> 1
    | 0, 1, 1 -> 4
    | _ ->
  let d = getbitint i in
    match a, b, c, d with
    | 0, 0, 1, 1 -> 5
    | 0, 0, 1, 0 -> 6
    | _ ->
  let e = getbitint i in
    match a, b, c, d, e with
    | 0, 0, 0, 1, 1 -> 7
    | _ ->
  let f = getbitint i in
    match a, b, c, d, e, f with
    | 0, 0, 0, 1, 0, 1 -> 8
    | 0, 0, 0, 1, 0, 0 -> 9
    | _ ->
  let g = getbitint i in
    match a, b, c, d, e, f, g with
    | 0, 0, 0, 0, 1, 0, 0 -> 10
    | 0, 0, 0, 0, 1, 0, 1 -> 11
    | 0, 0, 0, 0, 1, 1, 1 -> 12
    | _ ->
  let h = getbitint i in
    match a, b, c, d, e, f, g, h with
    | 0, 0, 0, 0, 0, 1, 0, 0 -> 13
    | 0, 0, 0, 0, 0, 1, 1, 1 -> 14
    | _ ->
  let j = getbitint i in
    match a, b, c, d, e, f, g, h, j with
    | 0, 0, 0, 0, 1, 1, 0, 0, 0 -> 15
    | _ ->
  let k = getbitint i in
    match a, b, c, d, e, f, g, h, j, k with
    | 0, 0, 0, 0, 1, 1, 0, 1, 1, 1 -> 0
    | 0, 0, 0, 0, 0, 1, 0, 1, 1, 1 -> 16
    | 0, 0, 0, 0, 0, 1, 1, 0, 0, 0 -> 17
    | 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 -> 18
    | 0, 0, 0, 0, 0, 0, 1, 1, 1, 1 -> 64 + read_black_code i
    | _ ->
  let l = getbitint i in
    match a, b, c, d, e, f, g, h, j, k, l with
    | 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1 -> 19
    | 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0 -> 20
    | 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0 -> 21
    | 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1 -> 22
    | 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0 -> 23
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1 -> 24
    | 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0 -> 25
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 -> 1792 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0 -> 1856 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1 -> 1920 + read_black_code i
    | _ ->
  let m = getbitint i in
    match a, b, c, d, e, f, g, h, j, k, l, m with
    | 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0 -> 26
    | 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1 -> 27
    | 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0 -> 28
    | 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1 -> 29
    | 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0 -> 30
    | 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1 -> 31
    | 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0 -> 32
    | 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1 -> 33
    | 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0 -> 34
    | 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1 -> 35
    | 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0 -> 36
    | 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1 -> 37
    | 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0 -> 38
    | 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1 -> 39
    | 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0 -> 40
    | 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1 -> 41
    | 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0 -> 42
    | 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1 -> 43
    | 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0 -> 44
    | 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1 -> 45
    | 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0 -> 46
    | 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1 -> 47
    | 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0 -> 48
    | 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1 -> 49
    | 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0 -> 50
    | 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1 -> 51
    | 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0 -> 52
    | 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1 -> 53
    | 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0 -> 54
    | 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1 -> 55
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0 -> 56
    | 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0 -> 57
    | 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1 -> 58
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1 -> 59
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0 -> 60
    | 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0 -> 61
    | 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0 -> 62
    | 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1 -> 63
    | 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0 -> 128 + read_black_code i
    | 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1 -> 192 + read_black_code i
    | 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1 -> 256 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1 -> 320 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0 -> 384 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1 -> 448 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0 -> 1984 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1 -> 2048 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0 -> 2112 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1 -> 2176 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0 -> 2240 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1 -> 2304 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0 -> 2368 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1 -> 2432 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0 -> 2496 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1 -> 2560 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 -> ~-1
    | _ ->
  let n = getbitint i in
    match a, b, c, d, e, f, g, h, j, k, l, m, n with
    | 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0 -> 512 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1 -> 576 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0 -> 640 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1 -> 704 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0 -> 768 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1 -> 832 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0 -> 896 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1 -> 960 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0 -> 1024 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1 -> 1088 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0 -> 1152 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1 -> 1216 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0 -> 1280 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1 -> 1344 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0 -> 1408 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1 -> 1472 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0 -> 1536 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1 -> 1600 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0 -> 1664 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1 -> 1728 + read_black_code i
    | _ -> raise (Failure "bad black code")

(* Group 4 Fax decoder. *)
type modes =
  | Pass
  | Horizontal
  | Vertical of int
  | Uncompressed
  | EOFB

let read_mode i =
  let a = getbitint i in
    match a with
    | 1 -> Vertical 0
    | _ ->
  let b = getbitint i in
  let c = getbitint i in
    match a, b, c with
    | 0, 1, 1 -> Vertical ~-1
    | 0, 1, 0 -> Vertical 1
    | 0, 0, 1 -> Horizontal
    | _ ->
  let d = getbitint i in
    match a, b, c, d with
    | 0, 0, 0, 1 -> Pass
    | _ ->
  let e = getbitint i in
  let f = getbitint i in
    match a, b, c, d, e, f with
    | 0, 0, 0, 0, 1, 1 -> Vertical ~-2
    | 0, 0, 0, 0, 1, 0 -> Vertical 2
    | _ ->
  let g = getbitint i in
    match a, b, c, d, e, f, g with
    | 0, 0, 0, 0, 0, 1, 1 -> Vertical ~-3
    | 0, 0, 0, 0, 0, 1, 0 -> Vertical 3
    | _ ->
  let h = getbitint i in
  let j = getbitint i in
  let k = getbitint i in
  let l = getbitint i in
  let m = getbitint i in
    match a, b, c, d, e, f, g, h, j, k, l, m with
    | 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1 -> Uncompressed
    | 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 ->
      let a = getbitint i in
      let b = getbitint i in
      let c = getbitint i in
      let d = getbitint i in
      let e = getbitint i in
      let f = getbitint i in
      let g = getbitint i in
      let h = getbitint i in
      let j = getbitint i in
      let k = getbitint i in
      let l = getbitint i in
      let m = getbitint i in
        begin match a, b, c, d, e, f, g, h, j, k, l, m with
        | 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 -> EOFB
        | _ -> raise (Failure "Not a valid code on EOFB")
        end
  | _ -> raise (Failure "Not a valid code")

let decode_CCITTFax k eol eba c r eob bone dra input =
  if k > 0 then raise DecodeNotSupported else
    let whiteval, blackval = if bone then 0, 1 else 1, 0
    and output = make_write_bitstream () in
      let b = bitstream_of_input input
      and column = ref 0
      and row = ref 0
      and refline = ref (Array.make c whiteval)
      and currline = ref (Array.make c 0)
      and white = ref true
      and output_line line =
        Array.iter (putbit output) line;
        align_write output
      in
        let output_span l v =
          if l < 0 then raise (Failure "Bad CCITT stream") else
            begin
              for x = !column to !column + l - 1 do !currline.(x) <- v; done;
              column += l
            end
        and find_b1 () =
          let pos = ref !column
          and curr, opp = if !white then whiteval, blackval else blackval, whiteval in
            let find v =
              while !refline.(!pos) <> v do incr pos done; !pos
            in
              try
                (* Careful to skip imaginary black at beginning *)
                ignore (if !column = 0 && !white then 0 else find curr);
                find opp
              with
                _ -> c
        and find_b2 () =
          let pos = ref !column
          and curr, opp = if !white then whiteval, blackval else blackval, whiteval in
            let find v =
              while !refline.(!pos) <> v do incr pos done; !pos
            in
              try
                (* Careful to skip imaginary black at beginning *)
                ignore (if !column = 0 && !white then 0 else find curr);
                ignore (find opp);
                find curr
              with
                _ -> c
        in
          try
            while true do
              if !column >= c then
                begin
                  output_line !currline;
                  refline := !currline;
                  column := 0;
                  set white;
                  if eba then align b;
                  incr row;
                  if !row >= r && r > 0 then raise EndOfInput
                end
              else
               begin
                  if k < 0 then
                    (* Group 4 *)
                    match read_mode b with
                    | Pass ->
                        output_span (find_b2 () - !column) (if !white then whiteval else blackval)
                    | Horizontal ->
                        if !white then
                          begin
                            output_span (read_white_code b) whiteval;
                            output_span (read_black_code b) blackval;
                          end
                        else
                          begin
                            output_span (read_black_code b) blackval;
                            output_span (read_white_code b) whiteval;
                          end
                    | Vertical n ->
                        output_span (find_b1 () - !column - n) (if !white then whiteval else blackval);
                        flip white
                    | EOFB -> raise EndOfInput
                    | Uncompressed -> raise DecodeNotSupported
                  else if k = 0 then
                    (* Group 3 *)
                    begin match (if !white then read_white_code else read_black_code) b with
                    | -1 ->
                       (* Pad it out *)
                       if !column > 0 then output_span (c - !column) whiteval
                    | l ->
                      begin
                        output_span l (if !white then whiteval else blackval);
                        flip white
                      end
                    end
                  else
                    raise DecodeNotSupported
                end
            done;
            mkstream 0
          with
            EndOfInput -> bytestream_of_write_bitstream output

(* \section{PNG and TIFF Predictors} *)

(* Get the value at index [i] from an int array [a], giving zero if the index is
too low. Fails in the usual manner if the index is too high. *)
let get0 a i =
  if i < 0 then 0 else a.(i)

(* TIFF prediction. 8bpp only for now. *)
let decode_tiff_predictor colors bpc columns stream =
  match bpc with
  | 8 ->
      let scanline_width = (colors * bpc * columns + 7) / 8 in
        for line = 0 to stream_size stream / scanline_width - 1 do
          let linestart = line * scanline_width in
            for p = 1 to scanline_width - 1 do
              stream.{linestart + p} <-
                (stream.{linestart + p - 1} + stream.{linestart + p}) mod 256
            done
        done;
        stream
  | _ ->
     raise DecodeNotSupported

(* Given two scanlines, the previous and current, and the predictor function
[p], calculate the output scanline as a list of bytes. *)
let decode_scanline_pair prior_encoded prior_decoded current pred bpc cols =
  let output = Array.copy current in
    begin match pred with
    | 0 -> (* None *)
        ()
    | 1 -> (* Sub *)
        for x = 0 to Array.length output - 1 do
          output.(x) <- (get0 current x + get0 output (x - cols)) mod 256
        done
    | 2 -> (* Up *)
        for x = 0 to Array.length output - 1 do
          output.(x) <- (get0 current x + get0 prior_decoded x) mod 256
        done
    | 3 -> (* Average -- No test case yet found. *)
        for x = 0 to Array.length output - 1 do
          output.(x) <-
            (get0 current x +
              (get0 output (x - cols) + get0 prior_decoded x) / 2) mod 256
        done
    | 4 -> (* Paeth *)
        let paeth a b c =
          let p = a + b - c in
            let pa = abs (p - a) and pb = abs (p - b) and pc = abs (p - c) in
              if pa <= pb && pa <= pc then a
              else if pb <= pc then b
              else c
          in
            for x = 0 to Array.length output - 1 do
              output.(x) <-
                let curr = get0 current x
                and currback = get0 output (x - cols)
                and decoded = get0 prior_decoded x
                and decodedback = get0 prior_decoded (x - cols) in
                  (curr + paeth currback decoded decodedback) mod 256
            done
    | _ -> raise DecodeNotSupported
    end;
    output

(* Main function. Given predictor, number of channels, bits-per-channel,
columns and the stream data, perform the decoding. *)
let decode_predictor pred colors bpc columns stream =
  if pred = 2 then decode_tiff_predictor colors bpc columns stream else
    let i = input_of_bytestream stream
    and scanline_width = (colors * bpc * columns + 7) / 8 in
      let blank () = ref (Array.make scanline_width 0) in
        let prev, curr, prior_decoded = blank (), blank (), blank ()
        and outputlines = ref []
        and finished = ref false
        and pred = ref 0
        and got_predictor = ref false in
          while not !finished do
            begin try
              clear got_predictor;
              pred := i.input_byte ();
              set got_predictor;
              prev := !curr;
              for x = 0 to scanline_width - 1 do
                !curr.(x) <- i.input_byte ()
              done
            with
              EndOfInput -> set finished
            end;
            (* We allow an unfinished final line only if we managed to get a
            predictor byte *)
            if !got_predictor then
              begin
                prior_decoded :=
                decode_scanline_pair
                  !prev !prior_decoded !curr !pred bpc ((bpc * colors + 7) / 8);
                outputlines =:: !prior_decoded
              end
          done;
          bytestream_of_arraylist (rev !outputlines)

(* \section{Run Length Encoding} *)
let encode_runlength stream =
  let i = Io.input_of_bytestream stream in
    let data_in = ref [] in
      begin try
        while true do data_in =:: i.input_byte () done
      with
        EndOfInput -> data_in := rev !data_in
      end;
      let rec runs_of_data prev = function
        | [] -> rev prev
        | h::t ->
            let same, rest = cleavewhile (eq h) (h::t) in
              runs_of_data ((length same, hd same)::prev) rest
      in
        let runs = ref (runs_of_data [] !data_in)
        and outbytes = ref []
        and chunksize = ref 0
        and chunkdata = ref [] in
          let writechunk () =
            if !chunksize > 0 then
              begin
                outbytes =:: !chunksize - 1; 
                iter (( =:: ) outbytes) (rev !chunkdata);
                chunkdata := [];
                chunksize := 0;
              end
          in
              while !runs <> [] do
                begin match hd !runs with
                | (l, x) when l < 1 ->
                    assert false
                | (l, x) when l < 3 ->
                    if l + !chunksize > 128 then writechunk ();
                    chunkdata =@ many x l;
                    chunksize += l 
                | (l, x) ->
                    writechunk ();
                    let l = ref l in
                      while !l > 0 do
                        outbytes =:: 257 - min !l 128;
                        outbytes =:: x;
                        l -= 128
                      done
                end;
                runs := tl !runs
              done;
              writechunk ();
              outbytes =:: 128; (*r End-of-data *)
              bytestream_of_list (rev !outbytes)

let decode_runlength i =
  let s =  {pos = 0; data = mkstream 4096} in
    let o = Io.output_of_stream s in
      let eod = ref false in
        try
          while not !eod do
            let l = i.input_byte () in
              if l < 128 then
                for x = 1 to l + 1 do
                  o.output_byte (i.input_byte ())
                done
              else if l > 128 then
                let towrite = i.input_byte () in
                  for x = 1 to 257 - l do
                    o.output_byte towrite
                  done
              else
                set eod
          done;
          let osize = i64toi (o.out_channel_length ()) in
            let output = mkstream osize in
              for x = 0 to osize - 1 do
                output.{x} <- s.data.{x}
              done;
              output
        with
          EndOfInput -> raise (Couldn'tDecodeStream "RunLength")

(* \section{Decoding PDF streams} *)
type source =
  | StreamSource of bytestream
  | InputSource of input

let decoder pdf dict source name =
  let input_of_source = function
    | InputSource i -> i
    | StreamSource s -> input_of_bytestream s
  in
    let i = input_of_source source in
      match name with
      | "/ASCIIHexDecode" | "/AHx" -> decode_ASCIIHex i
      | "/ASCII85Decode" | "/A85" -> decode_ASCII85 i
      | "/FlateDecode" | "/Fl" ->
          begin match source with
          | StreamSource s -> decode_flate s
          | InputSource i -> decode_flate_input i
          end
      | "/RunLengthDecode" | "/RL" -> decode_runlength i
      | "/LZWDecode" | "/LZW" ->
          let early =
            match lookup_direct_orelse pdf "/DecodeParms" "/DP" dict with
            | None -> 1
            | Some d ->
                match lookup_direct pdf "/EarlyChange" d with
                | Some (Integer n) -> n
                | None -> 1
                | _ -> raise (PDFError "malformed /EarlyChange")
          in
            decode_lzw early i
      | "/CCITTFaxDecode" | "/CCF" ->
          begin match lookup_direct_orelse pdf "/DecodeParms" "/DP" dict with
          | None -> decode_CCITTFax 0 false false 1728 0 true false 0 i
          | Some (Pdf.Dictionary _ as dparms)
          | Some (Array (dparms::_)) ->
              let dparms = direct pdf dparms in
              let k =
                match lookup_direct pdf "/K" dparms with
                | Some (Integer i) -> i
                | _ -> 0
              and eol =
                match lookup_direct pdf "/EndOfLine" dparms with
                | Some (Boolean b) -> b
                | _ -> false
              and eba =
                match lookup_direct pdf "/EncodedByteAlign" dparms with
                | Some (Boolean b) -> b
                | _ -> false
              and c =
                match lookup_direct pdf "/Columns" dparms with
                | Some (Integer i) -> i
                | _ -> 1728
              and r =
                match lookup_direct pdf "/Rows" dparms with
                | Some (Integer i) -> i
                | _ -> 0
              and eob =
                match lookup_direct pdf "/EndOfBlock" dparms with
                | Some (Boolean b) -> b
                | _ -> true
              and bone =
                match lookup_direct pdf "/BlackIs1" dparms with
                | Some (Boolean b) -> b
                | _ -> false
              and dra =
                match lookup_direct pdf "/DamagedRowsBeforeError" dparms with
                | Some (Integer i) -> i
                | _ -> 0
              in
                decode_CCITTFax k eol eba c r eob bone dra i
            | _ -> raise (Pdf.PDFError "bad Decodeparms")
            end
      | _ -> raise DecodeNotSupported

(* Decode at most one stage. *)
let decode_one pdf dict source =
  match lookup_direct_orelse pdf "/Filter" "/F" dict with
  | None | Some (Array []) ->
      begin match source with
      | StreamSource s -> s
      | InputSource i -> raise DecodeNotSupported
      end
  | Some (Name n) | Some (Array (Name n::_)) ->
      let decoded = decoder pdf dict source n in
        let decodeparms =
          match lookup_direct_orelse pdf "/DecodeParms" "/DP" dict with
          | Some (Dictionary d)
          | Some (Array (Dictionary d::_)) -> Dictionary d
          | _ -> Dictionary []
        in
          begin match lookup_direct pdf "/Predictor" decodeparms with
          | None | Some (Integer 1) -> decoded
          | Some (Integer pred) ->
              let colors =
                match lookup_direct pdf "/Colors" decodeparms with
                | Some (Integer n) -> n
                | None -> 1
                | _ -> raise (PDFError "malformed /Colors")
              and bits_per_component =
                match lookup_direct pdf "/BitsPerComponent" decodeparms with
                | Some (Integer n) -> n
                | None -> 8
                | _ -> raise (PDFError "malformed /BitsPerComponent")
              and columns =
                match lookup_direct pdf "/Columns" decodeparms with
                | Some (Integer n) -> n
                | None -> 1
                | _ -> raise (PDFError "malformed /Columns")
              in
                begin try
                  decode_predictor pred colors bits_per_component columns decoded
                with
                  _ -> raise (Couldn'tDecodeStream "Predictor")
                end
          | _ -> raise (PDFError "Malformed /Predictor")
          end
  | _ ->
    raise (PDFError "PDF.decode: Bad filter specification")

(* Remove a single decoder from a filter list. Also remove the first entry of a
 DecodeParms array *)
let remove_decoder d =
  let d' =
    match lookup "/Filter" d, lookup "/F" d with
    | None, None -> d
    | Some (Name _ | Array [_]), None -> lose (fun (n, _) -> n = "/Filter") d
    | None, Some (Name _ | Array [_]) -> lose (fun (n, _) -> n = "/F") d
    | Some (Array (_::t)), _ -> replace "/Filter" (Array t) d
    | _, Some (Array (_::t)) -> replace "/F" (Array t) d
    | _ -> raise (PDFError "PDF.remove_decoder: malformed /Filter")
  in
    match lookup "/DecodeParms" d', lookup "/DP" d' with
    | None, None -> d'
    | Some (Dictionary _ | Array []), _ -> remove "/DecodeParms" d'
    | _, Some (Dictionary _ | Array []) -> remove "/DP" d'
    | Some (Array (_::t)), _ -> replace "/DecodeParms" (Array t) d'
    | _, Some (Array (_::t)) -> replace "/DP" (Array t) d'
    | _ -> raise (PDFError "PDF.remove_decoder: malformed /DecodeParms")

(* \intf Decode at most one stage. *)
let rec decode_pdfstream_onestage pdf stream =
  getstream stream;
  match stream with
  | Stream ({contents = (Dictionary d as dict, Got s)} as stream_contents) ->
      begin match direct pdf (lookup_fail "no /Length" pdf "/Length" dict) with
      | Integer l -> () (*i if l <> stream_size s then raise (PDFError "Wrong /Length") i*)
      | _ -> raise (PDFError "No /Length")
      end;
      let stream' = decode_one pdf dict (StreamSource s) in
        let d' =
          replace "/Length" (Integer (stream_size stream')) (remove_decoder d)
        in
          stream_contents := Dictionary d', Got stream'
  | _ -> raise (PDFError "Pdf.decode_pdfstream: not a valid Stream")

(* \intf Decode until there's nothing left to do. *)
let rec decode_pdfstream pdf = function
  | Stream {contents = d, _} as stream ->
      getstream stream;
      begin match lookup_direct_orelse pdf "/Filter" "/F" d with
      | None -> ()
      | Some (Name _ | Array _) ->
            begin
              decode_pdfstream_onestage pdf stream;
              match stream with
              | Stream {contents = d', _} ->
                  if d = d' then () else
                  decode_pdfstream pdf stream
              | _ -> assert false
            end
      | _ -> raise (PDFError "Pdf.remove_decoder: malformed /Filter")
      end
  | _ -> raise (PDFError "Pdf.decode_pdfstream: malformed Stream")

(* \intf Decode a stream until a decoding isn't supported. *)
let decode_pdfstream_until_unknown pdf s =
  try decode_pdfstream pdf s with
    DecodeNotSupported -> ()

(* Decode the first decoder from an input. Any further ones can be done in the
usual fashion. Fails if no decoder (you should have dealt with this already). *)
let decode_from_input i dict =
  match lookup_direct_orelse empty "/F" "/Filter" dict with
  | Some (Name n) ->
      Some (decode_one Pdf.empty dict (InputSource i))
  | Some (Array (h::t)) ->
      let stream = decode_one Pdf.empty dict (InputSource i) in
        let rec decode_rest stream = function
          | [] -> stream
          | Name n::more ->
              let dict' = remove_dict_entry dict "/Filter" in
                let dict'' = remove_dict_entry dict' "/F" in
                  let stream' =
                    decode_one Pdf.empty dict'' (StreamSource stream)
                  in
                    decode_rest stream' more
          | _ -> raise (PDFError "Malformed filter array")
        in
          Some (decode_rest stream t)
  | _ -> raise (Couldn'tDecodeStream "No or bad filter")

(* \section {Encoding streams} *)

(* \intf Supported encodings. *)
type encoding =
  | ASCIIHex
  | ASCII85
  | RunLength
  | Flate

(* The name of an encoding. *)
let name_of_encoding = function
  | ASCIIHex -> "/ASCIIHexDecode"
  | ASCII85 -> "/ASCII85Decode"
  | RunLength -> "/RunLengthDecode"
  | Flate -> "/FlateDecode"

(* Add an encoding to the dictionary [d]. *)
let add_encoding length pdf encoding d =
  let filter' =
    match lookup_direct pdf "/Filter" d with
    | None ->
        Name (name_of_encoding encoding)
    | Some (Name n) ->
        Array (Name (name_of_encoding encoding)::[Name n])
    | Some (Array a) ->
        Array (Name (name_of_encoding encoding)::a)
    | _ -> raise (PDFError "Malformed /Filter")
  in
    replace_dict_entry (add_dict_entry d "/Filter" filter') "/Length" (Integer length)

(* Find the encoding function. *)
let encoder_of_encoding = function
  | ASCIIHex -> encode_ASCIIHex
  | ASCII85 -> encode_ASCII85
  | RunLength -> encode_runlength
  | Flate -> encode_flate

(* \intf Encode a PDF stream with an encoding. *)
let encode_pdfstream pdf encoding stream =
  getstream stream;
  match stream with
  | Stream ({contents = d, Got s} as stream) ->
      let data = encoder_of_encoding encoding s in
        let d' = add_encoding (stream_size data) pdf encoding d in
          stream := d', Got data
  | _ -> raise (PDFError "Pdf.encode_pdfstream: malformed Stream")

