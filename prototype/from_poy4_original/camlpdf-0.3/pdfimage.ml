(* \chaptertitle{PDFImage}{PDF Images} *)
open Utility

(* What's supported and needs supporting\\
   Unsupported CODECs: CCITTDecode, DCTDecode, JBIG2Decode, JPXDecode\\\\ 
   RGB, 8bpc\\
   CMYK, 8bpc\\
   Gray, 8bpc\\
   Black and white, 1bpp. The only one that /Decode works for\\
   Indexed, RGB and CMYK, 8bpp\\
   Indexed, RGB and CMYK, 4bpp\\
   Separation, CMYK\\
   ICCBased, knows how to find alternate colorspace\\
*)

type pixel_layout =
  | BPP1
  | BPP8
  | BPP24
  | BPP48

(* FIXME: We need to deal with decode and other things for JPEG, if we're not
going to decode them. *)
type image =
  | JPEG of bytestream
  | JPEG2000 of bytestream
  | JBIG2 of bytestream
  | Raw of int * int * pixel_layout * bytestream

let string_of_layout = function
  | BPP1 -> "BPP1"
  | BPP8 -> "BPP8"
  | BPP24 -> "BPP24"
  | BPP48 -> "BPP48"

let string_of_image = function
  | JPEG _ -> "JPEG"
  | JPEG2000 _ -> "JPEG2000"
  | JBIG2 _ -> "JBIG2"
  | Raw (w, h, layout, data) ->
      "RAW: " ^ string_of_int w ^ " " ^ string_of_int h
      ^ " " ^ string_of_layout layout ^ " bytes of data = "
      ^ string_of_int (stream_size data)

(* FIXME: Only copes with [1 0] for now, and only 8BPP *)
let decode entry image =
  match entry, image with
  | Some (Pdf.Array [Pdf.Integer 1; Pdf.Integer 0]), Raw (w, h, BPP24, s) ->
      for x = 0 to (stream_size s / 3) - 1 do
        s.{x * 3} <- 255 - s.{x * 3};
        s.{x * 3 + 1} <- 255 - s.{x * 3 + 1};
        s.{x * 3 + 2} <- 255 - s.{x * 3 + 2}
      done
  | _ -> ()

(* Decode until it is either plain or a type of decoding we can't deal with
natively. *) 
let rec decode_to_image pdf = function
  | Pdf.Stream {contents = Pdf.Dictionary d, s} as stream ->
      begin match lookup "/Filter" d with
      | None
      | Some (Pdf.Array [])
      | Some (Pdf.Name ("/DCTDecode" | "/DCT" | "/JBIG2Decode" | "/JPXDecode"))
      | Some (Pdf.Array [Pdf.Name ("/DCTDecode" | "/DCT" | "/JBIG2Decode" | "/JPXDecode")]) -> ()
      | _ ->
          Pdfcodec.decode_pdfstream_onestage pdf stream;
          decode_to_image pdf stream 
      end
  | _ -> raise (Pdf.PDFError "decode_to_image: bad stream")

(* Basic CMYK to RGB conversion *)
let rgb_of_cmyk c m y k =
  let c = float c and m = float m and y = float y and k = float k in
  let r = 255. -. fmin 255. ((c /.  255.) *. (255. -. k) +. k) 
  and g = 255. -. fmin 255. ((m /.  255.) *. (255. -. k) +. k)
  and b = 255. -. fmin 255. ((y /.  255.) *. (255. -. k) +. k) in
    int r, int g,  int b

let read_cmyk_8bpp_as_rgb24 width height data =
  let data' = mkstream (width * height * 3) in
    for p = 0 to width * height - 1 do
      let c = data.{p * 4}
      and m = data.{p * 4 + 1}
      and y = data.{p * 4 + 2}
      and k = data.{p * 4 + 3} in
        let r, g, b = rgb_of_cmyk c m y k in
          data'.{p * 3} <- r;
          data'.{p * 3 + 1} <- g;
          data'.{p * 3 + 2} <- b
    done;
    data'

let read_gray_8bpp_as_rgb24 width height data =
  let data' = mkstream (width * height * 3) in
    for pout = 0 to width * height - 1 do
      data'.{pout * 3} <- data.{pout};
      data'.{pout * 3 + 1} <- data.{pout};
      data'.{pout * 3 + 2} <- data.{pout};
    done;
    data'

(* Input is 1bpp, rows padded to bytes. *)
let read_1bpp_as_rgb24 width height s =
  let s' = mkstream (width * height * 3)
  and s_bits = Io.bitstream_of_input (Io.input_of_bytestream s) in
    let pout = ref 0 in
      for row = 0 to height - 1 do
        let bits_to_do = ref width in
          while !bits_to_do > 0 do
            let bit = if Io.getbit s_bits then 255 else 0 in
              s'.{!pout} <- bit;
              s'.{!pout + 1} <- bit;
              s'.{!pout + 2} <- bit;
              decr bits_to_do;
              pout += 3
          done;
          Io.align s_bits 
      done;
      s'

(* 4bpp, rows padded to bytes. *)
let read_4bpp_gray_as_rgb24 width height s =
  let s' = mkstream (width * height * 3)
  and s_bits = Io.bitstream_of_input (Io.input_of_bytestream s) in
    let pout = ref 0 in
      for row = 0 to height - 1 do
        let pix_to_do = ref width in
          while !pix_to_do > 0 do
            let a = if Io.getbit s_bits then 1 else 0 in
            let b = if Io.getbit s_bits then 1 else 0 in
            let c = if Io.getbit s_bits then 1 else 0 in
            let d = if Io.getbit s_bits then 1 else 0 in
              let col = (a * 8 + b * 4 + c * 2 + d) * (16 + 1) in
                s'.{!pout} <- col;
                s'.{!pout + 1} <- col;
                s'.{!pout + 2} <- col;
                decr pix_to_do;
                pout += 3
          done;
          Io.align s_bits 
      done;
      s'

(* Return a basic colorspace, together with any transform function. This is used
when we want to know, for instance, in [table_of_indexed_colorspace]. not clear
it's the right thing.  *)
let rec get_colorspace pdf resources = function
  | Pdf.Name ("/CalGray" | "/DeviceGray" | "/G") -> "/DeviceGray", None
  | Pdf.Name ("/CalRGB" | "/DeviceRGB" | "/RGB") -> "/DeviceRGB", None
  | Pdf.Name ("/CalCMYK" | "/DeviceCMYK" | "/CMYK") -> "/DeviceCMYK", None
  | Pdf.Name "/Pattern" -> "/Pattern", None
  | Pdf.Name space ->
    begin match Pdf.lookup_direct pdf "/ColorSpace" resources with
      | Some csdict ->
          begin match Pdf.lookup_direct pdf space csdict with
          | Some space' -> get_colorspace pdf resources space'
          | None -> space, None
          end
      | None -> space, None
    end
  | Pdf.Array elts ->
      begin match elts with
        | [Pdf.Name "/ICCBased"; iccstream] ->
             begin match
               Pdf.lookup_direct pdf "/Alternate" iccstream
             with
             | Some space' -> get_colorspace pdf resources space'
             | None ->
                 begin match Pdf.lookup_direct pdf "/N" iccstream with
                 | Some (Pdf.Integer 1) -> "/DeviceGray", None
                 | Some (Pdf.Integer 3) -> "/DeviceRGB", None
                 | Some (Pdf.Integer 4) -> "/DeviceCMYK", None
                 | _ -> "Bad ICCBased Alternate", None
                 end
             end
        | [Pdf.Name "/Separation"; _; alternate; tint_transform] ->
            let alternate', _ =
              get_colorspace pdf resources (Pdf.direct pdf alternate)
            in
              alternate', Some tint_transform
        | Pdf.Name "/DeviceN"::_::alternate::tt::_ -> 
            let space =
              fst (get_colorspace pdf resources (Pdf.direct pdf alternate))
            in
              space, Some tt
        | [Pdf.Name "/CalRGB"; _] ->
            get_colorspace pdf resources (Pdf.Name "/CalRGB")
        | [Pdf.Name "/CalCMYK"; _] ->
            get_colorspace pdf resources (Pdf.Name "/CalCMYK")
        | [Pdf.Name "/CalGray"; _] ->
            get_colorspace pdf resources (Pdf.Name "/CalGray")
        | _ -> raise (Pdf.PDFError "Unknown colorspace")
      end
  | Pdf.Indirect _ as indirect ->
      get_colorspace pdf resources (Pdf.direct pdf indirect)
  | _ ->
      raise (Pdf.PDFError "Unknown colorspace")

(* Read an indexed colourspace table. Does this cover all possibilities? *)
let table_of_indexed_colourspace pdf resources = function
  | Pdf.Array [Pdf.Name ("/Indexed" | "/I"); base; hival; lookup_data] ->
      let hival =
        match hival with
        | Pdf.Integer h -> h
        | _ -> raise (Pdf.PDFError "Bad /Hival")
      and base, _ =
        get_colorspace pdf resources base
      in
        let mktable_rgb data =
          try
            let table = Hashtbl.create (hival + 1)
            and i = Io.input_of_bytestream data in
              for x = 0 to hival do
                let r = i.Io.input_byte () in
                let g = i.Io.input_byte () in
                let b = i.Io.input_byte () in
                  Hashtbl.add table x (r, g, b)
              done;
              Some table
          with Io.EndOfInput -> None
        and mktable_cmyk data =
          try
            let table = Hashtbl.create (hival + 1)
            and i = Io.input_of_bytestream data in
              for x = 0 to hival do
                let c = i.Io.input_byte () in
                let m = i.Io.input_byte () in
                let y = i.Io.input_byte () in
                let k = i.Io.input_byte () in
                  Hashtbl.add table x (rgb_of_cmyk c m y k)
              done;
              Some table
          with Io.EndOfInput -> None
        in
          begin match Pdf.direct pdf lookup_data with
          | (Pdf.Stream _) as stream ->
              Pdfcodec.decode_pdfstream pdf stream;
              begin match stream with
              | (Pdf.Stream {contents = (_, Pdf.Got data)}) ->
                  begin match base with
                  | "/DeviceRGB" -> mktable_rgb data
                  | "/DeviceCMYK" -> mktable_cmyk data
                  | a -> None
                  end
              | _ -> raise (Pdf.PDFError "Indexed/Inconsistent")
              end
          | Pdf.String s ->
              let data = mkstream (String.length s) in
                for x = 0 to stream_size data - 1 do
                  data.{x} <- int_of_char s.[x]
                done;
                begin match base with
                | "/DeviceRGB" -> mktable_rgb data
                | "/DeviceCMYK" -> mktable_cmyk data
                | a -> None
                end
          | _ -> None 
          end
  | _ -> None

let read_8bpp_indexed_as_rgb24 table width height s =
  let s' = mkstream (width * height * 3) in
    for x = 0 to width * height - 1 do
      let r, g, b = Hashtbl.find table s.{x} in
        s'.{x * 3} <- r;
        s'.{x * 3 + 1} <- g;
        s'.{x * 3 + 2} <- b
    done;
    s'

let read_4bpp_indexed_as_rgb24 table width height s =
  let s' = mkstream (width * height * 3) in
    let posin = ref 0
    and posout = ref 0 in
      for row = 0 to height - 1 do
        for byte = 0 to (width + 1) / 2 - 1 do
          let p1 = s.{!posin} lsr 4
          and p2 = s.{!posin} land 15 in
            let r1, g1, b1 = Hashtbl.find table p1 in
              s'.{!posout} <- r1; incr posout;
              s'.{!posout} <- g1; incr posout;
              s'.{!posout} <- b1; incr posout;
            if not (odd width && byte = (width + 1) / 2 - 1) then
              let r2, g2, b2 = Hashtbl.find table p2 in
               s'.{!posout} <- r2; incr posout;
               s'.{!posout} <- g2; incr posout;
               s'.{!posout} <- b2; incr posout;
            incr posin
        done
      done;
      s'

(* Separation, CMYK alternate, tint transform function. *)
let read_separation_cmyk_as_rgb24 f width height s = 
  let s' = mkstream (width * height * 3) in
    for p = 0 to width * height - 1 do
      let v = s.{p} in
        match Pdffun.eval_function f [float v /. 255.] with
        | [c; y; m; k] ->
            let c = int (c *. 255.)
            and m = int (m *. 255.)
            and y = int (y *. 255.)
            and k = int (k *. 255.) in
            let r, g, b = rgb_of_cmyk c m y k in
              s'.{p * 3} <- r;
              s'.{p * 3 + 1} <- g;
              s'.{p * 3 + 2} <- b;
        | _ ->
            raise (Pdf.PDFError "Bad tint transform function")
    done;
    s'

let rec read_raw_image size colspace bpc pdf resources width height dict data =
  match size, colspace, bpc with
  | size, Some (Pdf.Name ("/DeviceRGB" | "/CalRGB" | "/RGB")), Some (Pdf.Integer 8)
      when size >= width * height * 3 ->
        Raw (width, height, BPP24, data)
  | size, Some (Pdf.Name ("/DeviceCMYK" | "/CalCMYK" | "/CMYK")), Some (Pdf.Integer 8)
      when size >= width * height * 4 ->
        Raw (width, height, BPP24, read_cmyk_8bpp_as_rgb24 width height data)
  | size, Some (Pdf.Name ("/DeviceGray" | "/G")), Some (Pdf.Integer 8)
      when size >= width * height ->
        Raw (width, height, BPP24, read_gray_8bpp_as_rgb24 width height data)
  | size, _, Some (Pdf.Integer 1)
      when size >= width * height / 8 ->
        Raw (width, height, BPP24, read_1bpp_as_rgb24 width height data)
  | size, Some (Pdf.Name ("/DeviceGray" | "/G")), Some (Pdf.Integer 4)
      when size >= width * height / 2 ->
        Raw (width, height, BPP24, read_4bpp_gray_as_rgb24 width height data)
  | size, Some (Pdf.Array (Pdf.Name ("/Indexed" | "/I")::_) as space), Some (Pdf.Integer 8)
      when size >= width * height ->
        begin match table_of_indexed_colourspace pdf resources space with
        | None -> raise (Pdf.PDFError "Bad indexed colourspace")
        | Some table ->
            Raw (width, height, BPP24, read_8bpp_indexed_as_rgb24 table width height data)
        end
  | size, Some (Pdf.Array (Pdf.Name ("/Indexed" | "/I")::_) as space), Some (Pdf.Integer 4)
      when size >= width * height / 2 ->
        begin match table_of_indexed_colourspace pdf resources space with
           | None -> raise (Pdf.PDFError "Bad indexed colorspace")
           | Some table ->
               Raw (width, height, BPP24, read_4bpp_indexed_as_rgb24 table width height data)
        end
  | size,
      (Some (Pdf.Array [Pdf.Name "/Separation"; _; Pdf.Name ("/DeviceCMYK" | "/CalCMYK" | "/CMYK"); f])),
       Some (Pdf.Integer 8)
    when size >= width * height ->
      let fn = Pdffun.parse_function pdf f in
        Raw (width, height, BPP24, read_separation_cmyk_as_rgb24 fn width height data)
  | size, Some ((Pdf.Array [Pdf.Name "/ICCBased"; _]) as cs), _ ->
      let alternate_space =
        Some (Pdf.Name (fst (get_colorspace pdf resources cs)))
      in
        read_raw_image size alternate_space bpc pdf resources width height dict data
  | size, cs, bpc ->
     (*i Printf.printf "NO IMAGE:\n size:%i\n cspace\n%s\n bpc\n%s\n width %i\n
     height %i\n" size
     (match cs with None -> "NONE" | Some cs -> Pdfwrite.string_of_pdf cs)
     (match bpc with None -> "NONE" | Some bpc -> Pdfwrite.string_of_pdf bpc)
     width
     height; i*)
     raise (Pdf.PDFError "No image\n")

let rec get_raw_image pdf resources width height dict data =
  let size =
    stream_size data
  and colspace =
    let colspace =
      Pdf.lookup_direct_orelse pdf "/ColorSpace" "/CS" dict
    in
      match Pdf.lookup_direct pdf "/ColorSpace" resources, colspace with
      | Some (Pdf.Dictionary _ as d), Some (Pdf.Name c) ->
          begin match Pdf.lookup_direct pdf c d with
          | Some colspace -> Some colspace
          | _ -> colspace
          end
      | _ -> colspace
  and bpc =
    Pdf.lookup_direct_orelse pdf "/BitsPerComponent" "/BPC" dict
  in
    read_raw_image size colspace bpc pdf resources width height dict data

let get_image_24bpp pdf resources stream =
  let streamdict, data =
    Pdf.getstream stream;
    match stream with
    | Pdf.Stream {contents = (s, Pdf.Got d)} ->
        s, d
    | _ -> assert false (*r [Pdf.getstream] would have failed *)
  in
    let width = 
      match (Pdf.lookup_direct_orelse pdf "/Width" "/W" streamdict) with
      | Some (Pdf.Integer x) -> x
      | _ -> raise (Pdfread.PDFSemanticError "Malformed /Image width")
    and height =
      match (Pdf.lookup_direct_orelse pdf "/Height" "/H" streamdict) with
      | Some (Pdf.Integer x) -> x
      | _ -> raise (Pdfread.PDFSemanticError "Malformed /Image height")
    in
      decode_to_image pdf stream;
      match stream with
      | Pdf.Stream {contents = (Pdf.Dictionary d) as dict, Pdf.Got s} ->
          begin match Pdf.lookup_direct_orelse pdf "/Filter" "/F" dict with
          | None | Some (Pdf.Array []) -> 
              let raw = get_raw_image pdf resources width height dict s
              and decode_entry = Pdf.lookup_direct_orelse pdf "/Decode" "/D" dict in
                decode decode_entry raw;
                raw
          | Some (Pdf.Name ("/DCTDecode" | "/DCT"))
          | Some (Pdf.Array [Pdf.Name ("/DCTDecode" | "/DCT")]) -> JPEG s
          | Some (Pdf.Name "/JBIG2Decode")
          | Some (Pdf.Array [Pdf.Name "/JBIG2Decode"]) -> JBIG2 s
          | Some (Pdf.Name "/JPXDecode")
          | Some (Pdf.Array [Pdf.Name "/JPXDecode"]) -> JPEG2000 s
          | _ -> raise (Pdf.PDFError "decode_to_image")
          end
      | _ -> assert false

