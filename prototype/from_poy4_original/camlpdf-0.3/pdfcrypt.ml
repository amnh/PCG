(* \chaptertitle{PDFCrypt}{Encryption and Decryption} *)
open Utility

(* \section{Hashes, passwords and keys} *)

(* Given an object number, generation number, input key and key length in bits,
apply Algorithm 3.1 from the PDF Reference manual to obtain the hash to be used
by the encryption function. *)
let find_hash r obj gen key keylength =
  let from_obj =
    [| i32toi (land32 obj 0x000000ffl);
       i32toi (lsr32 (land32 obj 0x0000ff00l) 8);
       i32toi (lsr32 (land32 obj 0x00ff0000l) 16) |]
  and from_gen =
    [| i32toi (land32 gen 0x000000ffl);
       i32toi (lsr32 (land32 gen 0x0000ff00l) 8) |]
  and extra =
    if r = 4 then [| 0x73; 0x41; 0x6C; 0x54 |] else [| |]
  in
    let digest_input = [key; from_obj; from_gen; extra] in
      let digest = Digest.string (string_of_int_arrays digest_input) in
        int_array_of_string
          (String.sub digest 0 (min 16 (keylength / 8 + 5)))

(* Find a key, given a password, O entry, P entry, id entry, and key length in
bits. *)
let padding =
  [| 0x28; 0xbf; 0x4e; 0x5e; 0x4e; 0x75; 0x8a; 0x41;
     0x64; 0x00; 0x4e; 0x56; 0xff; 0xfa; 0x01; 0x08;
     0x2e; 0x2e; 0x00; 0xb6; 0xd0; 0x68; 0x3e; 0x80;
     0x2f; 0x0c; 0xa9; 0xfe; 0x64; 0x53; 0x69; 0x7a |]

let pad_password password =
  let pw = Array.make 32 0 in
    Array.iteri (fun i v -> if i < 32 then pw.(i) <- v) password;
    let n = Array.length password in
      if n < 32 then
        for x = n to 31 do
          pw.(x) <- padding.(x - n)
        done;
  pw

let find_key ?(no_encrypt_metadata=false) password r o p id keylength =
  let password = int_array_of_string password
  and o = int_array_of_string o
  and id = int_array_of_string id in
    let pw = pad_password password in
      let from_p =
        [| i32toi (land32 p 0x000000ffl);
           i32toi (lsr32 (land32 p 0x0000ff00l) 8);
           i32toi (lsr32 (land32 p 0x00ff0000l) 16);
           i32toi (lsr32 (land32 p 0xff000000l) 24) |]
      and rev4_no_metadata =
        if r >= 4 && no_encrypt_metadata then [|255; 255; 255; 255|] else [||]
      in
        let todigest = [pw; o; from_p; id; rev4_no_metadata] in
          let hash_input = string_of_int_arrays todigest in
            let hashed = Digest.string hash_input in
              let hashed' =
                if r >= 3 then
                  let h = ref hashed in
                    for x = 1 to 50 do
                      let hashed = Digest.string !h in
                        h :=
                          string_of_int_array
                            (Array.sub (int_array_of_string hashed) 0 (keylength / 8))
                    done;
                    !h
                else
                  hashed
              in
                Array.sub (int_array_of_string hashed') 0 (keylength / 8)


(* \section{40bit / 128bit Encryption/Decryption Primitives} *)
(* Encryption / Decryption given a key. *)
let ksa s key =
  let keylength = Array.length key in
    for i = 0 to 255 do s.(i) <- i done;
    let j = ref 0 in
      for i = 0 to 255 do
        j := (!j + s.(i) + key.(i mod keylength)) mod 256;
        swap s i !j
      done

let prga s pi pj =
  pi := (!pi + 1) mod 256;
  pj := (!pj + s.(!pi)) mod 256;
  swap s !pi !pj;
  s.((s.(!pi) + s.(!pj)) mod 256)

let crypt key data =
  let s = Array.make 256 0
  and pi = ref 0
  and pj = ref 0
  and out = mkstream (stream_size data) in
    ksa s key;
    for x = 0 to stream_size data - 1 do
      out.{x} <- data.{x} lxor prga s pi pj
    done;
    out

(* \section{AES Encryption and Decryption Primitives} *)

(* The state, an array of four length 4 arrays. state.(row).(column) *)
let st = 
  Array.make_matrix 4 4 0

(* Finite field addition *)
let ( ++ ) = ( lxor )

(* Finite field multiplication modulo the irreducible polynomial. *)
let ( ** ) a b =
  let aa = ref a
  and bb = ref b
  and r = ref 0
  and t = ref 0 in
    while !aa <> 0 do
      if !aa land 1 <> 0 then r := !r lxor !bb;
      t := !bb land 0x80;
      bb := !bb lsl 1;
      if !t <> 0 then bb := !bb lxor 0x1b;
      aa := !aa lsr 1
    done;
    !r land 0xff

(* Multiplication of a finite field by x *)
let xtime f =
  (f lsl 1) lxor 0x1b

let input_to_state d =
  st.(0).(0) <- d.(0); st.(1).(0) <- d.(1);
  st.(2).(0) <- d.(2); st.(3).(0) <- d.(3);
  st.(0).(1) <- d.(4); st.(1).(1) <- d.(5);
  st.(2).(1) <- d.(6); st.(3).(1) <- d.(7);
  st.(0).(2) <- d.(8); st.(1).(2) <- d.(9);
  st.(2).(2) <- d.(10); st.(3).(2) <- d.(11);
  st.(0).(3) <- d.(12); st.(1).(3) <- d.(13);
  st.(2).(3) <- d.(14); st.(3).(3) <- d.(15)

let sbox =
[|
0x63; 0x7c; 0x77; 0x7b; 0xf2; 0x6b; 0x6f; 0xc5; 0x30; 0x01; 0x67; 0x2b; 0xfe; 0xd7; 0xab; 0x76;
0xca; 0x82; 0xc9; 0x7d; 0xfa; 0x59; 0x47; 0xf0; 0xad; 0xd4; 0xa2; 0xaf; 0x9c; 0xa4; 0x72; 0xc0;
0xb7; 0xfd; 0x93; 0x26; 0x36; 0x3f; 0xf7; 0xcc; 0x34; 0xa5; 0xe5; 0xf1; 0x71; 0xd8; 0x31; 0x15;
0x04; 0xc7; 0x23; 0xc3; 0x18; 0x96; 0x05; 0x9a; 0x07; 0x12; 0x80; 0xe2; 0xeb; 0x27; 0xb2; 0x75;
0x09; 0x83; 0x2c; 0x1a; 0x1b; 0x6e; 0x5a; 0xa0; 0x52; 0x3b; 0xd6; 0xb3; 0x29; 0xe3; 0x2f; 0x84;
0x53; 0xd1; 0x00; 0xed; 0x20; 0xfc; 0xb1; 0x5b; 0x6a; 0xcb; 0xbe; 0x39; 0x4a; 0x4c; 0x58; 0xcf;
0xd0; 0xef; 0xaa; 0xfb; 0x43; 0x4d; 0x33; 0x85; 0x45; 0xf9; 0x02; 0x7f; 0x50; 0x3c; 0x9f; 0xa8;
0x51; 0xa3; 0x40; 0x8f; 0x92; 0x9d; 0x38; 0xf5; 0xbc; 0xb6; 0xda; 0x21; 0x10; 0xff; 0xf3; 0xd2;
0xcd; 0x0c; 0x13; 0xec; 0x5f; 0x97; 0x44; 0x17; 0xc4; 0xa7; 0x7e; 0x3d; 0x64; 0x5d; 0x19; 0x73;
0x60; 0x81; 0x4f; 0xdc; 0x22; 0x2a; 0x90; 0x88; 0x46; 0xee; 0xb8; 0x14; 0xde; 0x5e; 0x0b; 0xdb;
0xe0; 0x32; 0x3a; 0x0a; 0x49; 0x06; 0x24; 0x5c; 0xc2; 0xd3; 0xac; 0x62; 0x91; 0x95; 0xe4; 0x79;
0xe7; 0xc8; 0x37; 0x6d; 0x8d; 0xd5; 0x4e; 0xa9; 0x6c; 0x56; 0xf4; 0xea; 0x65; 0x7a; 0xae; 0x08;
0xba; 0x78; 0x25; 0x2e; 0x1c; 0xa6; 0xb4; 0xc6; 0xe8; 0xdd; 0x74; 0x1f; 0x4b; 0xbd; 0x8b; 0x8a;
0x70; 0x3e; 0xb5; 0x66; 0x48; 0x03; 0xf6; 0x0e; 0x61; 0x35; 0x57; 0xb9; 0x86; 0xc1; 0x1d; 0x9e;
0xe1; 0xf8; 0x98; 0x11; 0x69; 0xd9; 0x8e; 0x94; 0x9b; 0x1e; 0x87; 0xe9; 0xce; 0x55; 0x28; 0xdf;
0x8c; 0xa1; 0x89; 0x0d; 0xbf; 0xe6; 0x42; 0x68; 0x41; 0x99; 0x2d; 0x0f; 0xb0; 0x54; 0xbb; 0x16
|]

let inv_sbox =
[|
0x52; 0x09; 0x6a; 0xd5; 0x30; 0x36; 0xa5; 0x38; 0xbf; 0x40; 0xa3; 0x9e; 0x81; 0xf3; 0xd7; 0xfb;
0x7c; 0xe3; 0x39; 0x82; 0x9b; 0x2f; 0xff; 0x87; 0x34; 0x8e; 0x43; 0x44; 0xc4; 0xde; 0xe9; 0xcb;
0x54; 0x7b; 0x94; 0x32; 0xa6; 0xc2; 0x23; 0x3d; 0xee; 0x4c; 0x95; 0x0b; 0x42; 0xfa; 0xc3; 0x4e;
0x08; 0x2e; 0xa1; 0x66; 0x28; 0xd9; 0x24; 0xb2; 0x76; 0x5b; 0xa2; 0x49; 0x6d; 0x8b; 0xd1; 0x25;
0x72; 0xf8; 0xf6; 0x64; 0x86; 0x68; 0x98; 0x16; 0xd4; 0xa4; 0x5c; 0xcc; 0x5d; 0x65; 0xb6; 0x92;
0x6c; 0x70; 0x48; 0x50; 0xfd; 0xed; 0xb9; 0xda; 0x5e; 0x15; 0x46; 0x57; 0xa7; 0x8d; 0x9d; 0x84;
0x90; 0xd8; 0xab; 0x00; 0x8c; 0xbc; 0xd3; 0x0a; 0xf7; 0xe4; 0x58; 0x05; 0xb8; 0xb3; 0x45; 0x06;
0xd0; 0x2c; 0x1e; 0x8f; 0xca; 0x3f; 0x0f; 0x02; 0xc1; 0xaf; 0xbd; 0x03; 0x01; 0x13; 0x8a; 0x6b;
0x3a; 0x91; 0x11; 0x41; 0x4f; 0x67; 0xdc; 0xea; 0x97; 0xf2; 0xcf; 0xce; 0xf0; 0xb4; 0xe6; 0x73;
0x96; 0xac; 0x74; 0x22; 0xe7; 0xad; 0x35; 0x85; 0xe2; 0xf9; 0x37; 0xe8; 0x1c; 0x75; 0xdf; 0x6e;
0x47; 0xf1; 0x1a; 0x71; 0x1d; 0x29; 0xc5; 0x89; 0x6f; 0xb7; 0x62; 0x0e; 0xaa; 0x18; 0xbe; 0x1b;
0xfc; 0x56; 0x3e; 0x4b; 0xc6; 0xd2; 0x79; 0x20; 0x9a; 0xdb; 0xc0; 0xfe; 0x78; 0xcd; 0x5a; 0xf4;
0x1f; 0xdd; 0xa8; 0x33; 0x88; 0x07; 0xc7; 0x31; 0xb1; 0x12; 0x10; 0x59; 0x27; 0x80; 0xec; 0x5f;
0x60; 0x51; 0x7f; 0xa9; 0x19; 0xb5; 0x4a; 0x0d; 0x2d; 0xe5; 0x7a; 0x9f; 0x93; 0xc9; 0x9c; 0xef;
0xa0; 0xe0; 0x3b; 0x4d; 0xae; 0x2a; 0xf5; 0xb0; 0xc8; 0xeb; 0xbb; 0x3c; 0x83; 0x53; 0x99; 0x61;
0x17; 0x2b; 0x04; 0x7e; 0xba; 0x77; 0xd6; 0x26; 0xe1; 0x69; 0x14; 0x63; 0x55; 0x21; 0x0c; 0x7d
|]

let subbyte b =
  sbox.(b)

let sub_bytes () =
  for r = 0 to 3 do
    for c = 0 to 3 do
      st.(r).(c) <- sbox.(st.(r).(c))
    done
  done

let inv_sub_bytes () =
  for r = 0 to 3 do
    for c = 0 to 3 do
      st.(r).(c) <- inv_sbox.(st.(r).(c))
    done
  done

(* Key schedule *)
let keys =
  Array.create 44 0l

let word_of_bytes a b c d =
  let a, b, c, d =
    (lsl32 (i32ofi a) 24),
    (lsl32 (i32ofi b) 16),
    (lsl32 (i32ofi c) 8),
    (i32ofi d)
  in
    lor32 (lor32 a b) (lor32 c d)

let bytes_of_word w =
  i32toi (lsr32 w 24),
  i32toi (land32 (lsr32 w 16) 0xFFl),
  i32toi (land32 (lsr32 w 8) 0xFFl),
  i32toi (land32 w 0xFFl)

let subword w =
  let a, b, c, d = bytes_of_word w in
    word_of_bytes (subbyte a) (subbyte b) (subbyte c) (subbyte d)

let rotword w =
  let a, b, c, d = bytes_of_word w in
    word_of_bytes b c d a

(* Round Constants (0..10) *)
let rcon =
  [| 0l;
     lsl32 0x01l 24; lsl32 0x02l 24; lsl32 0x04l 24; lsl32 0x08l 24;  
     lsl32 0x10l 24; lsl32 0x20l 24; lsl32 0x40l 24; lsl32 0x80l 24;  
     lsl32 0x1bl 24; lsl32 0x36l 24; |]

(* Key expansion *)
let key_expansion key =
  let temp = ref 0l
  and i = ref 0 in
    while (!i < 4) do
      keys.(!i) <-
        word_of_bytes
          key.(4 * !i) key.(4 * !i + 1) key.(4 * !i + 2) key.(4 * !i + 3);
      incr i
    done;
    i := 4;
    while (!i < 44) do
      temp := keys.(!i - 1);
      if !i mod 4 = 0 then
        temp := lxor32 (subword (rotword !temp)) rcon.(!i / 4);
      keys.(!i) <- lxor32 keys.(!i - 4) !temp;
      incr i 
    done

let shift_rows () =
  let a, b, c, d =
    st.(1).(0), st.(1).(1), st.(1).(2), st.(1).(3)
  in
    st.(1).(0) <- b; st.(1).(1) <- c;
    st.(1).(2) <- d; st.(1).(3) <- a;
  let a, b, c, d =
    st.(2).(0), st.(2).(1), st.(2).(2), st.(2).(3)
  in
    st.(2).(0) <- c; st.(2).(1) <- d;
    st.(2).(2) <- a; st.(2).(3) <- b;
  let a, b, c, d =
    st.(3).(0), st.(3).(1), st.(3).(2), st.(3).(3)
  in
    st.(3).(0) <- d; st.(3).(1) <- a;
    st.(3).(2) <- b; st.(3).(3) <- c

let inv_shift_rows () =
  let a, b, c, d =
    st.(1).(0), st.(1).(1), st.(1).(2), st.(1).(3)
  in
    st.(1).(0) <- d; st.(1).(1) <- a;
    st.(1).(2) <- b; st.(1).(3) <- c;
  let a, b, c, d =
    st.(2).(0), st.(2).(1), st.(2).(2), st.(2).(3)
  in
    st.(2).(0) <- c; st.(2).(1) <- d;
    st.(2).(2) <- a; st.(2).(3) <- b;
  let a, b, c, d =
    st.(3).(0), st.(3).(1), st.(3).(2), st.(3).(3)
  in
    st.(3).(0) <- b; st.(3).(1) <- c;
    st.(3).(2) <- d; st.(3).(3) <- a

let mix_columns () =
  for c = 0 to 3 do
    let s'0 =
      (0x02 ** st.(0).(c)) ++ (0x03 ** st.(1).(c)) ++ st.(2).(c) ++ st.(3).(c)
    and s'1 =
      st.(0).(c) ++ (0x02 ** st.(1).(c)) ++ (0x03 ** st.(2).(c)) ++ st.(3).(c)
    and s'2 =
      st.(0).(c) ++ st.(1).(c) ++ (0x02 ** st.(2).(c)) ++ (0x03 ** st.(3).(c))
    and s'3 =
      (0x03 ** st.(0).(c)) ++ st.(1).(c) ++ st.(2).(c) ++ (0x02 ** st.(3).(c))
    in
      st.(0).(c) <- s'0;
      st.(1).(c) <- s'1;
      st.(2).(c) <- s'2;
      st.(3).(c) <- s'3
  done

let inv_mix_columns () =
  for c = 0 to 3 do
    let s'0 =
      (0x0e ** st.(0).(c)) ++ (0x0b ** st.(1).(c)) ++
      (0x0d ** st.(2).(c)) ++ (0x09 ** st.(3).(c))
    and s'1 =
      (0x09 ** st.(0).(c)) ++ (0x0e ** st.(1).(c)) ++
      (0x0b ** st.(2).(c)) ++ (0x0d ** st.(3).(c))
    and s'2 =
      (0x0d ** st.(0).(c)) ++ (0x09 ** st.(1).(c)) ++
      (0x0e ** st.(2).(c)) ++ (0x0b ** st.(3).(c))
    and s'3 =
      (0x0b ** st.(0).(c)) ++ (0x0d ** st.(1).(c)) ++
      (0x09 ** st.(2).(c)) ++ (0x0e ** st.(3).(c))
    in
      st.(0).(c) <- s'0;
      st.(1).(c) <- s'1;
      st.(2).(c) <- s'2;
      st.(3).(c) <- s'3
  done

(* Add a round key to the state. *)
let add_round_key keypos =
  let a1, a2, a3, a4 = bytes_of_word keys.(keypos)
  and b1, b2, b3, b4 = bytes_of_word keys.(keypos + 1)
  and c1, c2, c3, c4 = bytes_of_word keys.(keypos + 2)
  and d1, d2, d3, d4 = bytes_of_word keys.(keypos + 3) in
    st.(0).(0) <- st.(0).(0) ++ a1; st.(1).(0) <- st.(1).(0) ++ a2;
    st.(2).(0) <- st.(2).(0) ++ a3; st.(3).(0) <- st.(3).(0) ++ a4;
    st.(0).(1) <- st.(0).(1) ++ b1; st.(1).(1) <- st.(1).(1) ++ b2;
    st.(2).(1) <- st.(2).(1) ++ b3; st.(3).(1) <- st.(3).(1) ++ b4;
    st.(0).(2) <- st.(0).(2) ++ c1; st.(1).(2) <- st.(1).(2) ++ c2;
    st.(2).(2) <- st.(2).(2) ++ c3; st.(3).(2) <- st.(3).(2) ++ c4;
    st.(0).(3) <- st.(0).(3) ++ d1; st.(1).(3) <- st.(1).(3) ++ d2;
    st.(2).(3) <- st.(2).(3) ++ d3; st.(3).(3) <- st.(3).(3) ++ d4

let output_from_state () =
  [| st.(0).(0); st.(1).(0); st.(2).(0); st.(3).(0);
     st.(0).(1); st.(1).(1); st.(2).(1); st.(3).(1);
     st.(0).(2); st.(1).(2); st.(2).(2); st.(3).(2);
     st.(0).(3); st.(1).(3); st.(2).(3); st.(3).(3) |]
  
(* Encryption cipher. Assumes key already expanded. *)
let cipher data_in =
  input_to_state data_in;
  add_round_key 0;
  for round = 1 to 9 do
    sub_bytes ();
    shift_rows ();
    mix_columns ();
    add_round_key (round * 4)
  done;
  sub_bytes ();
  shift_rows ();
  add_round_key 40;
  output_from_state ()

(* Decryption cipher. Assumes key already expanded. *)
let inv_cipher data_in =
  input_to_state data_in;
  add_round_key 40;
  for round = 9 downto 1 do
    inv_shift_rows ();
    inv_sub_bytes ();
    add_round_key (round * 4);
    inv_mix_columns ();
  done;
  inv_shift_rows ();
  inv_sub_bytes ();
  add_round_key 0;
  output_from_state ()

(* Pad the input data (RFC2898, PKCS \#5), then encrypt using a 16 byte AES
cipher in cipher block chaining mode, with a random initialisation vector, which
is stored as the first 16 bytes of the result. *)
let mkiv () =
  Random.self_init ();
  let r () = Random.int 255 in
    [| r (); r (); r (); r ();
       r (); r (); r (); r ();
       r (); r (); r (); r ();
       r (); r (); r (); r () |]

(* Debug function to print a block as characters. *)
let print_block arr =
  Array.iter (fun i -> Printf.printf "%C" (char_of_int i)) arr;
  flprint "\n\n"

(* Build blocks for encryption, including padding. *)
let get_blocks data =
  let l = stream_size data in
    let fullblocks =
      if l < 16 then [] else
        let blocks = ref [] in
          for x = 0 to l / 16 - 1 do
            blocks =::
              Array.of_list
                (map (fun x -> data.{x}) (ilist (x * 16) (x * 16 + 15)))
          done;
          rev !blocks
    and lastblock =
      let getlast n =
        if n = 0 then [] else
          let bytes = ref [] in
            for x = 0 to n - 1 do
              bytes =:: data.{l - 1 - x}
            done;
            !bytes
      and pad n =
        many n n
      in
        let overflow = l mod 16 in
          Array.of_list (getlast overflow @ pad (16 - overflow))
    in
      fullblocks @ [lastblock]

(* Flatten a list of blocks into a bytestream *)
let stream_of_blocks blocks =
  let len = 16 * length blocks in
    let s = mkstream len
    and p = ref 0 in
      iter
        (fun a ->
          Array.iter (fun v -> s.{!p} <- v; incr p) a)
        blocks;
      s

(* These two functions strip the padding from a stream once it's been decoded.*)
let get_padding s =
  let l = stream_size s in
    assert (l >= 16);
    let potential = s.{l - 1} in
      if potential > 0x10 || potential < 0x01 then None else
        let rec elts_equal p f t =
          if f = t then p = s.{t} else
            p = s.{f} && elts_equal p (f + 1) t
        in
          if elts_equal potential (l - potential - 1) (l - 1)
            then Some potential
            else None

let cutshort s =
  if stream_size s = 0 then mkstream 0 else
    if stream_size s <= 16 then s else
      match get_padding s with
      | None -> s
      | Some padding ->
          let s' = mkstream (stream_size s - padding) in
            for x = 0 to stream_size s' - 1 do
              s'.{x} <- s.{x}
            done;
            s'

(* Get blocks for decryption *) 
let get_plain_blocks d =
  if stream_size d mod 16 <> 0 then raise (Pdf.PDFError "Bad AES data") else
    map
      (fun n -> Array.init 16 (fun x -> d.{16 * n + x}))
      (ilist 0 (stream_size d / 16 - 1))

(* Decrypt data *)
let aes_decrypt_data key data =
  key_expansion key;
  match get_plain_blocks data with
  | [] | [_] -> mkstream 0
  | iv::codeblocks ->
      let prev_ciphertext = ref iv
      and outblocks = ref [] in
        iter
          (fun block ->
             let plaintext =
               (array_map2 (lxor)) (inv_cipher block) !prev_ciphertext
             in 
               prev_ciphertext := block;
               outblocks =:: plaintext)
          codeblocks;
          cutshort (stream_of_blocks (rev !outblocks))

(* Encrypt data *)
let aes_encrypt_data key data =
  key_expansion key;
  let outblocks = ref []
  and firstblock = mkiv () in
    let prev_ciphertext = ref firstblock in
      iter
        (fun block ->
          let ciphertext =
            cipher ((array_map2 (lxor)) block !prev_ciphertext)
          in
            prev_ciphertext := ciphertext;
            outblocks =:: ciphertext)
        (get_blocks data);
        stream_of_blocks (firstblock::rev !outblocks)

(* \section{Encryption and decryption of PDF files} *)

(* Authenticate the user password, given the password string and U, O, P, id
and key length entry. *)
let authenticate_user ?(no_encrypt_metadata=false) password r u o p id keylength =
  let u = int_array_of_string u in
    let key = find_key ~no_encrypt_metadata password r o p id keylength in
      if r >= 3 then
        let id = int_array_of_string id in
          let todigest = [padding; id] in
            let hash_input = string_of_int_arrays todigest in
              let hashed = Digest.string hash_input in
                let encrypted_hashed =
                  int_array_of_stream (crypt key (bytestream_of_string hashed))
                in
                  let u' = ref [||] in
                    u' := encrypted_hashed;
                    for x = 1 to 19 do
                      let key' = Array.make (keylength / 8) 0 in
                        for k = 0 to (keylength / 8) - 1 do
                          key'.(k) <- key.(k) lxor x 
                        done;
                        u' :=
                          int_array_of_stream
                            (crypt key' (stream_of_int_array !u'))
                    done;
                    Array.sub u 0 16 = !u'
      else
        u = int_array_of_stream (crypt key (stream_of_int_array padding))

(* Decrypt a PDF file, given the user password. *)
let rec
  decrypt pdf ?(no_encrypt_metadata = false) ?(encrypt = false) obj gen key keylength r
= function
  | Pdf.String s ->
      let f =
        (if r = 4 then
          (if encrypt then aes_encrypt_data else aes_decrypt_data)
         else
          crypt)
      in
        let s_ints = bytestream_of_string s in
          let hash = find_hash r (i32ofi obj) (i32ofi gen) key keylength in
            Pdf.String (string_of_stream (f hash s_ints))
  | (Pdf.Stream _) as stream ->
      decrypt_stream pdf ~no_encrypt_metadata ~encrypt obj gen key keylength r stream
  | Pdf.Array a ->
      Pdf.recurse_array (decrypt pdf ~no_encrypt_metadata ~encrypt obj gen key keylength r) a
  | Pdf.Dictionary d ->
      Pdf.recurse_dict (decrypt pdf ~no_encrypt_metadata ~encrypt obj gen key keylength r) d
  | x -> x

and decrypt_stream
  pdf ?(no_encrypt_metadata = false) ?(encrypt = false) obj gen key keylength r stream
=
  Pdf.getstream stream;
  begin match stream with
  | (Pdf.Stream {contents = (Pdf.Dictionary dict as d, Pdf.Got data)}) as stream ->
      if
        let identity_crypt_filter_present =
          match Pdf.lookup_direct pdf "/Filter" d with
          | Some (Pdf.Name "/Crypt")
          | Some (Pdf.Array (Pdf.Name "/Crypt"::_)) ->
              begin match Pdf.lookup_direct pdf "/DecodeParms" d with
              | Some (Pdf.Dictionary decodeparmsdict)
              | Some (Pdf.Array (Pdf.Dictionary decodeparmsdict::_)) ->
                  begin match
                    Pdf.lookup_direct pdf "/Name" (Pdf.Dictionary decodeparmsdict)
                  with
                  | Some (Pdf.Name "/Identity") | None -> true
                  | _ -> false
                  end
              | _ -> true
              end
          | _ -> false
        in
          (no_encrypt_metadata &&
             Pdf.lookup_direct pdf "/Type" d = Some (Pdf.Name "/Metadata"))
          || identity_crypt_filter_present
      then
        stream
      else
        let data' =
          let f =
            (if r = 4 then
              (if encrypt then aes_encrypt_data else aes_decrypt_data)
            else
              crypt)
          in
            let hash = find_hash r (i32ofi obj) (i32ofi gen) key keylength in
              f hash data
        and dict' =
          Pdf.recurse_dict
            (decrypt pdf ~no_encrypt_metadata ~encrypt obj gen key keylength r) dict
        in
          let dict'' =
            if stream_size data <> stream_size data' then
              Pdf.replace_dict_entry
                dict' "/Length" (Pdf.Integer (stream_size data'))
            else
              dict'
          in
            Pdf.Stream {contents = (dict'', Pdf.Got data')}
  | _ -> assert false 
  end

let process_cryption
  ?(no_encrypt_metadata = false) ?(encrypt = false) pdf crypt_type user_pw r u o p id keylength
=
  if authenticate_user ~no_encrypt_metadata user_pw r u o p id keylength then
    begin
      let key = find_key ~no_encrypt_metadata user_pw r o p id keylength
      and newdoc = ref Pdf.empty in
        Pdf.objiter_gen
          (fun objnum gennum obj ->
             newdoc :=
               Pdf.addobj_given_num
                 !newdoc
                 (objnum,
                    decrypt pdf ~no_encrypt_metadata ~encrypt objnum gennum key keylength r obj)
          )
          pdf;
        let trailerdict' = Pdf.remove_dict_entry pdf.Pdf.trailerdict "/Encrypt" in
          Some
            {pdf with
                 Pdf.objects = !newdoc.Pdf.objects;
                 Pdf.trailerdict = trailerdict'}
    end
  else
    None

(* ARC4 = old style or crypt filter with V2. AESV2 = Crypt filter with AESV2. We
don't need to distinguish between old and new ARC4 since support for different
crypts for different filter works anyway. *)
type encryption =
  | ARC4 of int * int (* keylength, r (= 2 or 3 or 4) *)
  | AESV2 (* v = 4, r = 4 *)

let get_encryption_values pdf =
  match Pdf.lookup_direct pdf "/Encrypt" pdf.Pdf.trailerdict with
  | None -> assert false (* Never called on an unencrypted PDF *)
  | Some encryptdict ->
      let crypt_type =
        match
          Pdf.lookup_direct pdf "/Filter" encryptdict,
          Pdf.lookup_direct pdf "/V" encryptdict,
          Pdf.lookup_direct pdf "/Length" encryptdict,
          Pdf.lookup_direct pdf "/R" encryptdict
        with
        | Some (Pdf.Name "/Standard"), Some (Pdf.Integer 1), _, Some (Pdf.Integer r)
        | Some (Pdf.Name "/Standard"), Some (Pdf.Integer 2), None, Some (Pdf.Integer r) ->
            Some (ARC4 (40, r))
        | Some (Pdf.Name "/Standard"), Some (Pdf.Integer 2), Some (Pdf.Integer n), _
            when n mod 8 = 0 && n >= 40 && n <= 128 ->
              Some (ARC4 (n, 3))
        | Some (Pdf.Name "/Standard"), Some (Pdf.Integer 4), length, _ ->
            begin match Pdf.lookup_direct pdf "/CF" encryptdict with
            | Some cfdict ->
                begin match Pdf.lookup_direct pdf "/StdCF" cfdict with
                | Some stdcfdict ->
                    begin match Pdf.lookup_direct pdf "/CFM" stdcfdict with
                    | Some (Pdf.Name "/V2") ->
                        begin match length with
                        | Some (Pdf.Integer i) -> Some (ARC4 (i, 4))
                        | _ ->
                            begin match Pdf.lookup_direct pdf "/Length" cfdict with
                            | Some (Pdf.Integer i) -> Some (ARC4 (i, 4))
                            | _ -> None
                            end
                        end
                    | Some (Pdf.Name "/AESV2") -> Some AESV2
                    | _ -> None
                    end
                | _ -> None
                end
            | _ -> None
            end
        | _ -> None
      in
        match crypt_type with
        | None -> raise (Pdf.PDFError "No encryption method")
        | Some crypt_type ->
            let o =
              match Pdf.lookup_direct pdf "/O" encryptdict with
              | Some (Pdf.String o) -> o
              | _ -> raise (Pdf.PDFError "Bad or missing /O entry")
            and u =
              match Pdf.lookup_direct pdf "/U" encryptdict with
              | Some (Pdf.String u) -> u
              | _ -> raise (Pdf.PDFError "Bad or missing /U entry")
            and p =
              match Pdf.lookup_direct pdf "/P" encryptdict with
              | Some (Pdf.Integer flags) -> i32ofi flags
              | _ -> raise (Pdf.PDFError "Bad or missing /P entry")
            and id =
              match Pdf.lookup_direct pdf "/ID" pdf.Pdf.trailerdict with
              | Some (Pdf.Array [Pdf.String s; _]) -> s
              | _ -> raise (Pdf.PDFError "Bad or missing /ID element")
            in
              crypt_type, u, o, p, id

(* Permissions *)
type permission =
  | NoEdit (*r R2, Bit 4 *)
  | NoPrint (*r R2, Bit 3 *)
  | NoCopy (*r R2, Bit 5 *)
  | NoAnnot (*r R2, Bit 6 *)
  | NoForms (*r R3 only, Bit 9 *)
  | NoExtract (*r R3 only, Bit 10 *)
  | NoAssemble (*r R3 only, Bit 11 *)
  | NoHqPrint (*r R3 only, Bit 12 *)

let string_of_permission = function
  | NoEdit -> "NoEdit"
  | NoPrint -> "NoPrint"
  | NoCopy -> "NoCopy"
  | NoAnnot -> "NoAnnot"
  | NoForms -> "NoForms"
  | NoExtract -> "NoExtract"
  | NoAssemble -> "NoAssemble"
  | NoHqPrint -> "NoHqPrint"

let string_of_bans bans =
  fold_left ( ^ ) "" (interleave " " (map string_of_permission bans))
  
let p_of_banlist toban =
  let p = ref 0l in
    let setbit n b =
      if b then p := Int32.logor !p (Int32.shift_left 1l (n - 1))
    and notin =
      notpred (member' toban)
    in
      setbit 3 (notin NoPrint);
      setbit 4 (notin NoEdit);
      setbit 5 (notin NoCopy);
      setbit 6 (notin NoAnnot);
      setbit 7 true;
      setbit 8 true;
      setbit 9 (notin NoForms);
      setbit 10 (notin NoExtract);
      setbit 11 (notin NoAssemble);
      setbit 12 (notin NoHqPrint);
      iter (fun x -> setbit x true) (ilist 13 32);
      !p

let banlist_of_p p =
  let l = ref []
  and bitset n =
    Int32.logand (Int32.shift_right p (n - 1)) 1l = 0l
  in
    if bitset 3 then l =:: NoPrint;
    if bitset 4 then l =:: NoEdit;
    if bitset 5 then l =:: NoCopy;
    if bitset 6 then l =:: NoAnnot;
    if bitset 9 then l =:: NoForms;
    if bitset 10 then l =:: NoExtract;
    if bitset 11 then l =:: NoAssemble;
    if bitset 12 then l =:: NoHqPrint;
    !l

(* Main function for decryption. *)
let decrypt_pdf user_pw pdf =
  match Pdf.lookup_direct pdf "/Encrypt" pdf.Pdf.trailerdict with
  | None -> Some pdf, []
  | Some encrypt_dict ->
     let crypt_type, u, o, p, id = get_encryption_values pdf in
       let r, keylength =
         match crypt_type with
         | AESV2 -> 4, 128
         | ARC4 (k, r) -> r, k
       and encrypt_metadata =
         match Pdf.lookup_direct pdf "/EncryptMetadata" encrypt_dict with
         | Some (Pdf.Boolean false) -> false
         | _ -> true
       in
         process_cryption ~no_encrypt_metadata:(not encrypt_metadata)
         pdf crypt_type user_pw r u o p id keylength,
         banlist_of_p p

(* Just decrypt a single stream, given the user password, and pdf. This is used
to decrypt cross-reference streams during the reading of a file -- the PDF is
only partially formed at this stage. *)
let decrypt_single_stream user_pw pdf obj gen stream =
  match Pdf.lookup_direct pdf "/Encrypt" pdf.Pdf.trailerdict with
  | None -> stream 
  | Some encrypt_dict ->
     let crypt_type, u, o, p, id = get_encryption_values pdf in
       let r, keylength =
         match crypt_type with
         | AESV2 -> 4, 128
         | ARC4 (k, r) -> r, k
       and no_encrypt_metadata =
         match Pdf.lookup_direct pdf "/EncryptMetadata" encrypt_dict with
         | Some (Pdf.Boolean false) -> true
         | _ -> false
       in
         if
           authenticate_user ~no_encrypt_metadata user_pw r u o p id keylength
         then
           let key = find_key ~no_encrypt_metadata user_pw r o p id keylength in
             decrypt_stream pdf ~no_encrypt_metadata obj gen key keylength r stream
         else
           raise (Pdf.PDFError "Bad password when decrypting stream")

(* Calculate the owner key from the padded owner password (as calculated by
[pad_password] *)
let owner_key padded_owner keylength r =
  let digest = Digest.string (string_of_int_array padded_owner) in
    let digest' =
      if r >= 3 then
        let d = ref digest in
          for x = 1 to 50 do
            d := Digest.string !d
          done;
          !d
        else
          digest 
    in
      int_array_of_string (String.sub digest' 0 (keylength / 8))

(* Calculate XOR keys *)
let mkkey key x =
  let key' = Array.copy key in
    for k = 0 to Array.length key - 1 do
      key'.(k) <- key.(k) lxor x 
    done;
    key'

(* Decrypt with the owner password. *)
let decrypt_pdf_owner owner_pw pdf =
  match Pdf.lookup_direct pdf "/Encrypt" pdf.Pdf.trailerdict with
  | None -> Some pdf
  | _ ->
    let padded_owner = pad_password (int_array_of_string owner_pw) in
      let crypt_type, _, o, _, _ = get_encryption_values pdf in
        let r, keylength =
          match crypt_type with
          | AESV2 -> 4, 128
          | ARC4 (k, r) -> r, k
        in
          let user_pw =
            let key = owner_key padded_owner keylength r in
              if r = 2 then
                string_of_stream (crypt key (bytestream_of_string o))
              else (* r >= 3 *)
                begin
                  let acc = ref (bytestream_of_string o) in
                    for x = 19 downto 0 do
                      acc := crypt (mkkey key x) !acc
                    done;
                    string_of_stream !acc 
                end
          in
            fst (decrypt_pdf user_pw pdf)

(* Make an owner password *)
let mk_owner r owner_pw user_pw keylength =
  let padded_owner =
    let source =
      if owner_pw = "" then user_pw else owner_pw
    in
     pad_password (int_array_of_string source)
  in
    let key = owner_key padded_owner keylength r in
      let padded_user = pad_password (int_array_of_string user_pw) in
        if r = 2 then
          string_of_stream (crypt key (stream_of_int_array padded_user))
        else (* r >= 3 *)
          let acc = ref (crypt key (stream_of_int_array padded_user)) in
            for x = 1 to 19 do
              acc := crypt (mkkey key x) !acc
            done;
            string_of_stream !acc 
            
(* Make a user password *)
let mk_user ?(no_encrypt_metadata = false) user_pw o p id r keylength =
  let key = find_key ~no_encrypt_metadata user_pw r o p id keylength in
    if r = 2 then
      string_of_stream (crypt key (stream_of_int_array padding))
    else (* r >= 3 *)
      let digest_input = [padding; int_array_of_string id] in
        let digest = Digest.string (string_of_int_arrays digest_input) in
          let acc = ref (crypt key (bytestream_of_string digest)) in
            for x = 1 to 19 do
              acc := crypt (mkkey key x) !acc
            done;
            string_of_stream !acc ^ (implode (many '\000' 16))

(* Get the ID, or add one if there's not one there. Return the updated pdf and
the ID *)
let get_or_add_id pdf =    
  match Pdf.lookup_direct pdf "/ID" pdf.Pdf.trailerdict with
  | Some (Pdf.Array [Pdf.String s; _]) ->
      s, pdf
  | _ ->
      let idobj = Pdf.generate_id pdf "" in
        let pdf' =
          {pdf with
            Pdf.trailerdict =
              Pdf.add_dict_entry pdf.Pdf.trailerdict "/ID" idobj}
        in
          match idobj with
          | Pdf.Array [Pdf.String s; _] -> s, pdf'
          | _ -> assert false

(* 40bit encryption *)
let encrypt_pdf_40bit user_pw owner_pw banlist pdf =
  let p = p_of_banlist banlist
  and owner = mk_owner 2 owner_pw user_pw 40
  and id, pdf = get_or_add_id pdf in
    let user = mk_user user_pw owner p id 2 40 in
      let crypt_dict =
        Pdf.Dictionary
          ["/Filter", Pdf.Name "/Standard";
           "/V", Pdf.Integer 1;
           "/R", Pdf.Integer 2;
           "/O", Pdf.String owner;
           "/U", Pdf.String user;
           "/P", Pdf.Integer (i32toi p)]
      in
        match process_cryption pdf (ARC4 (40, 2)) user_pw 2 user owner p id 40 with
        | Some pdf ->
            {pdf with
              Pdf.trailerdict =
                Pdf.add_dict_entry pdf.Pdf.trailerdict "/Encrypt" crypt_dict}
        | None -> raise (Pdf.PDFError "Encryption failed")

(* 128bit encryption *)
let encrypt_pdf_128bit user_pw owner_pw banlist pdf =
  let p = p_of_banlist banlist
  and owner = mk_owner 3 owner_pw user_pw 128
  and id, pdf = get_or_add_id pdf in
    let user = mk_user user_pw owner p id 3 128 in
      let crypt_dict =
        Pdf.Dictionary
          ["/Filter", Pdf.Name "/Standard";
           "/V", Pdf.Integer 2;
           "/R", Pdf.Integer 3;
           "/O", Pdf.String owner;
           "/U", Pdf.String user;
           "/Length", Pdf.Integer 128;
           "/P", Pdf.Integer (i32toi p)]
      in
        match process_cryption pdf (ARC4 (128, 3)) user_pw 3 user owner p id 128 with
        | Some pdf ->
            {pdf with
              Pdf.trailerdict =
                Pdf.add_dict_entry pdf.Pdf.trailerdict "/Encrypt" crypt_dict}
        | None -> raise (Pdf.PDFError "Encryption failed")

(* AES Encryption. *)
let encrypt_pdf_AES encrypt_metadata user_pw owner_pw banlist pdf =
  let p = p_of_banlist banlist
  and owner = mk_owner 4 owner_pw user_pw 128
  and id, pdf = get_or_add_id pdf in
    let user =
      mk_user ~no_encrypt_metadata:(not encrypt_metadata) user_pw owner p id 4 128
    in
      let crypt_dict =
        Pdf.Dictionary
          ["/Filter", Pdf.Name "/Standard";
           "/V", Pdf.Integer 4;
           "/CF",
              Pdf.Dictionary
                ["/StdCF",
                  Pdf.Dictionary
                    ["/Length", Pdf.Integer 16;
                     "/AuthEvent", Pdf.Name "/DocOpen";
                     "/CFM", Pdf.Name "/AESV2"]];
           "/EncryptMetadata", Pdf.Boolean encrypt_metadata;
           "/Length", Pdf.Integer 128;
           "/R", Pdf.Integer 4;
           "/O", Pdf.String owner;
           "/U", Pdf.String user;
           "/P", Pdf.Integer (i32toi p);
           "/StrF", Pdf.Name "/StdCF";
           "/StmF", Pdf.Name "/StdCF"]
      in
        match 
          process_cryption ~no_encrypt_metadata:(not encrypt_metadata) ~encrypt:true
          pdf AESV2 user_pw 4 user owner p id 128
        with
        | Some pdf ->
            {pdf with
              Pdf.trailerdict =
                Pdf.add_dict_entry pdf.Pdf.trailerdict "/Encrypt" crypt_dict}
        | None -> raise (Pdf.PDFError "Encryption failed")

(* \section{Utility functions} *)

(* Is a file encrypted? *)
let is_encrypted pdf =
  match Pdf.lookup_direct pdf "/Encrypt" pdf.Pdf.trailerdict with
  | Some _ -> true
  | None -> false

