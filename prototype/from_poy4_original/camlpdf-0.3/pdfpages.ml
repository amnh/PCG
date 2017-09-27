(* \chaptertitle{PDFPages}{High level PDF operations} *)
open Utility
open Io
open Pdf
open Pdfread

(* \section{Types} *)

(* \REF{PDF Manual, Table 4.1}Graphics operators. Exported in interface. *)
type operator =
  | Op_w of float (*r \NOTE{General graphics state}Set line width *)
  | Op_J of int (*r Set line cap *)
  | Op_j of int (*r Set line join *)
  | Op_M of float (*r Set mitre limit *)
  | Op_d of float list * float (*r Set dash pattern (dash, phase) *)
  | Op_ri of string (*r Set rendering intent. *)
  | Op_i of int (*r Set flatness. *)
  | Op_gs of string (*r Set graphics state from dictionary *)
  | Op_q (*r \NOTE{Special graphics state}Save graphics state to stack *)
  | Op_Q (*r Restore graphics state from stack *)
  | Op_cm of Transform.transform_matrix (*r Modify CTM by concatenation *)
  | Op_m of float * float (*r \NOTE{Path construction}Begin a new subpath *)
  | Op_l of float * float (*r Append a straight segment *)
  | Op_c of float * float * float * float * float * float (*r Cubic bezier *)
  | Op_v of float * float * float * float (*r Similar. *)
  | Op_y of float * float * float * float (*r Similar. *)
  | Op_h (*r Close subpath *)
  | Op_re of float * float * float * float (*r Append rectangle *)
  | Op_S (*r Stroke a path\NOTE{Path painting} *)
  | Op_s (*r Close and stroke path *) 
  | Op_f (*r Fill path, non-zero *)
  | Op_F (*r Same. *)
  | Op_f' (*r f* operator. Fill path, even-odd. *)
  | Op_B (*r Fill and stroke path, non-zero *)
  | Op_B' (*r B* operator. Fill and stroke path, even-odd *)
  | Op_b (*r Close fill and stroke, non-zero *)
  | Op_b' (*r b* operator. Close fill and stroke, even-odd *)
  | Op_n (*r Path no-op *)
  | Op_W (*r Clipping path, even-odd \NOTE{Clipping paths} *)
  | Op_W' (*r Clipping path, non-zero *)
  | Op_BT (*r \NOTE{Text objects}Begin a text object *)
  | Op_ET (*r End a text object *)
  | Op_Tc of float (*r Set character spacing \NOTE{Text state} *)
  | Op_Tw of float (*r Set word spacing *)
  | Op_Tz of float (*r Set horizontal scaling *)
  | Op_TL of float (*r Set leading *)
  | Op_Tf of string * float (*r Set font size *)
  | Op_Tr of int (*r Set text rendering mode *)
  | Op_Ts of float (*r Set text rise *)
  | Op_Td of float * float (*r Move to next line \NOTE{Text positioning} *)
  | Op_TD of float * float (*r Ditto, but set leading *)
  | Op_Tm of Transform.transform_matrix (*r Set text and line matrices *)
  | Op_T' (*r T* operator. Move text to the next line *)
  | Op_Tj of string (*r Show text string \NOTE{Text showing} *)
  | Op_TJ of pdfobject (*r Show many text strings *)
  | Op_' of string (*r Move to next line and show text *)
  | Op_'' of float * float * string (*r Ditto, extra parameters *)
  | Op_d0 of float * float (*r Set glpyh width info \NOTE{Type 3 fonts} *)
  | Op_d1 of float * float * float * float * float * float (*r Similar *)
  | Op_CS of string (*r Set colour space. \NOTE{Colour} *)
  | Op_cs of string (*r Same for nonstroking operations *)
  | Op_SC of float list (*r Set colour in current colour space. *)
  | Op_sc of float list (*r Same for nonstroking operations *)
  | Op_SCN of float list (*r Set colour in current colour space. *)
  | Op_scn of float list (*r Same for nonstroking operations *)
  | Op_SCNName of string * float list (*r A named [Op_SCN] *)
  | Op_scnName of string * float list (*r Same for [Op_scn] *)
  | Op_G of float (*r set gray *)
  | Op_g of float (*r set gray nonstroking *)
  | Op_RG of float * float * float (*r Set stroking colour *)
  | Op_rg of float * float * float (*r Set painting colour *)
  | Op_K of float * float * float * float (*r Set CMYK stroking *)
  | Op_k of float * float * float * float (*r Set CMYK nonstroking *)
  | Op_sh of string (*r Shading pattern \NOTE{Shading patterns} *)
  | InlineImage of (pdfobject * bytestream) (*r Inline image dictionary/data *)
  | Op_Do of string (*r Introduce an XObject \NOTE{XObjects} *)
  | Op_MP of string (*r Marked content point \NOTE{Marked Content} *)
  | Op_DP of string * pdfobject (*r same with property list *)
  | Op_BMC of string (*r begin marked content sequence *)
  | Op_BDC of string * pdfobject (*r same with property list *)
  | Op_EMC (*r end of marked content sequence *)
  | Op_BX (*r Start compatibility mode  \NOTE{Compatibility markers} *)
  | Op_EX (*r End compatibility mode *)
  | Op_Unknown of string (*r Unknown operand / operator sequence *)

type lexeme =
  | Op of string
  | Obj of Pdfread.lexeme
  | PdfObj of pdfobject
  | LexInlineImage of (pdfobject * bytestream)
  | LexComment
  
(* \section{Lexing} *)
let lexemes_of_op = function
  | Op_w w -> [Obj (LexReal w); Op "w"]
  | Op_J j -> [Obj (LexInt j); Op "J"]
  | Op_j j -> [Obj (LexInt j); Op "j"]
  | Op_M m -> [Obj (LexReal m); Op "M"]
  | Op_d (fl, f) ->
      [Obj LexLeftSquare] @
      (map (fun x -> Obj (LexReal x)) fl) @
      [Obj LexRightSquare; Obj (LexReal f); Op "d"]
  | Op_ri s -> [Obj (LexName s); Op "ri"]
  | Op_i i -> [Obj (LexInt i); Op "i"]
  | Op_gs s -> [Obj (LexName s); Op "gs"]
  | Op_q -> [Op "q"] 
  | Op_Q -> [Op "Q"] 
  | Op_cm t ->
      [Obj (LexReal t.Transform.a); Obj (LexReal t.Transform.b);
       Obj (LexReal t.Transform.c); Obj (LexReal t.Transform.d);
       Obj (LexReal t.Transform.e); Obj (LexReal t.Transform.f);
       Op "cm"]
  | Op_m (a, b) ->
      [Obj (LexReal a); Obj (LexReal b); Op "m"]
  | Op_l (a, b) ->
      [Obj (LexReal a); Obj (LexReal b); Op "l"]
  | Op_c (a, b, c, d, e, f) ->
      [Obj (LexReal a); Obj (LexReal b);
       Obj (LexReal c); Obj (LexReal d);
       Obj (LexReal e); Obj (LexReal f); Op "c"]
  | Op_v (a, b, c, d) ->
      [Obj (LexReal a); Obj (LexReal b);
       Obj (LexReal c); Obj (LexReal d); Op "v"]
  | Op_y (a, b, c, d) -> 
      [Obj (LexReal a); Obj (LexReal b);
       Obj (LexReal c); Obj (LexReal d); Op "y"]
  | Op_h -> [Op "h"]
  | Op_re (a, b, c, d) -> 
      [Obj (LexReal a); Obj (LexReal b);
       Obj (LexReal c); Obj (LexReal d); Op "re"]
  | Op_S -> [Op "S"] 
  | Op_s -> [Op "s"]
  | Op_f -> [Op "f"]
  | Op_F -> [Op "F"]
  | Op_f' -> [Op "f*"]
  | Op_B -> [Op "B"]
  | Op_B' -> [Op "B*"]
  | Op_b -> [Op "b"]
  | Op_b' -> [Op "b*"]
  | Op_n -> [Op "n"]
  | Op_W -> [Op "W"] 
  | Op_W' -> [Op "W*"]
  | Op_BT -> [Op "BT"]
  | Op_ET -> [Op "ET"]
  | Op_Tc c -> [Obj (LexReal c); Op "Tc"] 
  | Op_Tw w -> [Obj (LexReal w); Op "Tw"] 
  | Op_Tz z -> [Obj (LexReal z); Op "Tz"] 
  | Op_TL l -> [Obj (LexReal l); Op "TL"] 
  | Op_Tf (f, s) -> [Obj (LexName f); Obj (LexReal s); Op "Tf"]
  | Op_Tr i -> [Obj (LexInt i); Op "Tr"]
  | Op_Ts f -> [Obj (LexReal f); Op "Ts"]
  | Op_Td (f, f') -> [Obj (LexReal f); Obj (LexReal f'); Op "Td"]
  | Op_TD (f, f') -> [Obj (LexReal f); Obj (LexReal f'); Op "TD"]
  | Op_Tm t ->
      [Obj (LexReal t.Transform.a); Obj (LexReal t.Transform.b);
       Obj (LexReal t.Transform.c); Obj (LexReal t.Transform.d);
       Obj (LexReal t.Transform.e); Obj (LexReal t.Transform.f);
       Op "Tm"]
  | Op_T' -> [Op "T*"]
  | Op_Tj s -> [Obj (LexString s); Op "Tj"] 
  | Op_TJ pdfobject -> [PdfObj pdfobject; Op "TJ"]
  | Op_' s -> [Obj (LexString s); Op "'"]
  | Op_'' (f, f', s) -> 
      [Obj (LexReal f); Obj (LexReal f'); Obj (LexString s); Op "''"]
  | Op_d0 (f, f') -> [Obj (LexReal f); Obj (LexReal f'); Op "d0"]
  | Op_d1 (a, b, c, d, e, f) ->
      [Obj (LexReal a); Obj (LexReal b);
       Obj (LexReal c); Obj (LexReal d);
       Obj (LexReal e); Obj (LexReal f); Op "d1"]
  | Op_CS s -> [Obj (LexName s); Op "CS"] 
  | Op_cs s -> [Obj (LexName s); Op "cs"] 
  | Op_SC fs -> map (fun f -> Obj (LexReal f)) fs @ [Op "SC"]
  | Op_sc fs -> map (fun f -> Obj (LexReal f)) fs @ [Op "sc"]
  | Op_SCN fs -> map (fun f -> Obj (LexReal f)) fs @ [Op "SCN"]
  | Op_scn fs -> map (fun f -> Obj (LexReal f)) fs @ [Op "scn"]
  | Op_SCNName (s, fs) ->
      [Obj (LexName s)] @ map (fun x -> Obj (LexReal x)) fs @ [Op "SCN"]
  | Op_scnName (s, fs) ->
      [Obj (LexName s)] @ map (fun x -> Obj (LexReal x)) fs @ [Op "scn"]
  | Op_G f -> [Obj (LexReal f); Op "G"] 
  | Op_g f -> [Obj (LexReal f); Op "g"] 
  | Op_RG (r, g, b) ->
      [Obj (LexReal r); Obj (LexReal g); Obj (LexReal b); Op "RG"]
  | Op_rg (r, g, b) ->
      [Obj (LexReal r); Obj (LexReal g); Obj (LexReal b); Op "rg"]
  | Op_K (c, m, y, k) ->
      [Obj (LexReal c); Obj (LexReal m); Obj (LexReal y); Obj (LexReal k); Op "K"]
  | Op_k (c, m, y, k) ->
      [Obj (LexReal c); Obj (LexReal m); Obj (LexReal y); Obj (LexReal k); Op "k"]
  | Op_sh s -> [Obj (LexName s); Op "sh"]
  | InlineImage (dict, data) -> [LexInlineImage (dict, data)]
  | Op_Do s -> [Obj (LexName s); Op "Do"]
  | Op_MP s -> [Obj (LexName s); Op "MP"]
  | Op_DP (s, obj) -> [Obj (LexName s); PdfObj obj; Op "DP"] 
  | Op_BMC s -> [Obj (LexName s); Op "BMC"]
  | Op_BDC (s, obj) -> [Obj (LexName s); PdfObj obj; Op "BDC"]
  | Op_EMC -> [Op "EMC"]
  | Op_BX -> [Op "BX"] 
  | Op_EX -> [Op "EX"] 
  | Op_Unknown _ -> []

(* Find a string representing some lexemes *)
let string_of_lexemes lexemes =
  let string_of_lexeme = function
    | LexComment -> ""
    | Obj o -> Pdfread.string_of_lexeme o 
    | Op op -> op
    | PdfObj obj -> Pdfwrite.string_of_pdf obj
    | LexInlineImage (dict, data) ->
        let dict_string = Pdfwrite.string_of_pdf dict in
          let dict_string' =
            (* Remove the dictionary markers. *)
            implode & rev & (drop' 2) & rev & (drop' 2) & explode dict_string
          and data_string =
            string_of_bytestream data
          and space =
            let filters =
              match lookup_direct_orelse Pdf.empty "/F" "/Filter" dict with
              | Some (Array filters) -> filters
              | Some (Name f) -> [Name f]
              | _ -> []
            in
              if
                (member (Name "/ASCIIHexDecode") filters
                || member (Name "/ASCII85Decode") filters
                || member (Name "/AHx") filters
                || member (Name "/A85") filters)
              then ""
              else " "
          in
          "BI\n" ^ dict_string' ^ "ID" ^ space ^ data_string ^ "\nEI\n"
  in
    let strings = map string_of_lexeme lexemes in 
      fold_left ( ^ ) "" (interleave " " strings)

(* \intf Make a string of an operation, for debug purposes only. *)
let string_of_op = function
  | Op_Unknown s -> "UNKNOWN: " ^ s
  | op -> string_of_lexemes (lexemes_of_op op)

let string_of_ops ops =
  fold_left ( ^ ) "" (interleave " " (map string_of_op ops))

(* Lex a name. *)
let lex_name i =
  nudge i;
  Some (Obj (LexName (implode ('/'::getuntil_white_or_delimiter i))))

(* Lex a number *)
let lex_number i =
  match Pdfread.lex_number i with
  | LexReal r -> Some (Obj (LexReal r))
  | LexInt i -> Some (Obj (LexInt i))
  | _ -> None

(* Lex and parse a dictionary to a [Pdf.pdfobject]. This constitutes a single
lexeme in terms of this module. *)
let get_dictionary i =
  Some (PdfObj (snd (Pdfread.parse (Pdfread.lex_dictionary i))))

(* This is raised when something which is a legitimate part of the PDF
standard but which we don't understand is found. For now, this is just inline
images. Eventually, it will be nothing. *)
exception Couldn'tHandleContent

(* Given a colourspace and the number of bits per component, give the number of
bits per pixel in the stored image data. *)
let rec components pdf resources t =
  match t with
  | Name ("/CalGray" | "/DeviceGray" | "/G") -> 1
  | Name ("/CalRGB" | "/DeviceRGB" | "/RGB") -> 3
  | Name ("/CalCMYK" | "/DeviceCMYK" | "/CMYK") -> 4
  | Name "/Pattern" ->
      raise (PDFSemanticError "Can't use /Pattern here")
  | Name space ->
      begin match lookup_direct pdf "/ColorSpace" resources with
      | Some csdict ->
          begin match lookup_direct pdf space csdict with
          | Some space' -> components pdf resources space'
          | None -> raise (PDFSemanticError "ColorSpace not found")
          end
      | None -> raise (PDFSemanticError "ColorSpace dict not found")
      end
  | Array [Name "/ICCBased"; iccstream] ->
      begin match lookup_direct pdf "/N" iccstream with
      | Some (Integer n) -> n
      | _ -> raise (PDFSemanticError "Bad iccstream")
      end
  | Array (Name "/DeviceN"::_::alternate::_) ->
      components pdf resources (direct pdf alternate)
  | Array [Name "/Separation"; _; _; _]
  | Array (Name "/Indexed"::_::_) -> 1
  | Array [Name "/CalRGB"; _] -> 3
  | Array [Name "/CalCMYK"; _] -> 4
  | Array [Name "/CalGray"; _] -> 1
  | Array [Name "/Pattern"; alternate] ->
      components pdf resources (direct pdf alternate)
  | _ -> raise (PDFSemanticError "Unknown colourspace")

(* Lex an inline image. We read the dictionary, and then the stream. *)
let lex_inline_image pdf resources i =
  (*i flprint "lex_inline_image\n"; i*)
  let fail () =
    raise Couldn'tHandleContent
  and dict =
    let lexemes = Pdfread.lex_dictionary i in
      snd (Pdfread.parse ([Pdfread.LexLeftDict] @ lexemes @ [Pdfread.LexRightDict]))
  in
    (* Read ID token *)
    dropwhite i;
    let c = i.input_char () in
      let c' = i.input_char () in
        match c, c' with
        | 'I', 'D' ->
          (* Skip a byte if not ASCII85 / ASCIIHex as one of the filters. *)
          let toskip =
            let filters =
              match lookup_direct_orelse pdf "/F" "/Filter" dict with
              | Some (Array filters) -> filters
              | Some (Name f) -> [Name f]
              | _ -> []
            in
              not
                (member (Name "/ASCIIHexDecode") filters
                || member (Name "/ASCII85Decode") filters
                || member (Name "/AHx") filters
                || member (Name "/A85") filters)
          in
            if toskip then ignore (i.input_byte ());
            let bytes =
              let bpc =
                match lookup_direct_orelse pdf "/BPC" "/BitsPerComponent" dict with
                | Some (Integer bpc) -> bpc
                | _ -> fail ()
              in
              let cspace =
                match lookup_direct_orelse pdf "/CS" "/ColorSpace" dict with
                | Some (Name ("/DeviceGray" | "/DeviceRGB" | "/DeviceCMYK") as n) ->
                    n
                | Some (Name ("/G" | "/RGB" | "/CMYK") as n) -> n
                | Some ((Array _) as n) -> n
                | Some (Name cspace) ->
                    begin match lookup_direct pdf "/ColorSpace" resources with
                    | Some (Dictionary _ as d) ->
                        begin match lookup_direct pdf cspace d with
                        | Some c -> c
                        | _ -> fail ()
                        end
                    | _ -> fail ()
                    end
                | None ->
                    (* Could it be an image mask? *)
                    begin match lookup_direct_orelse pdf "/IM" "/ImageMask" dict with
                    | Some (Pdf.Boolean true) -> Name "/DeviceGray"
                    | _ -> fail ()
                    end
                | _ -> fail ()
              and width =
                match lookup_direct_orelse pdf "/W" "/Width" dict with
                | Some (Integer w) -> w
                | _ -> fail () 
              and height =
                match lookup_direct_orelse pdf "/H" "/Height" dict with
                | Some (Integer h) -> h
                | _ -> fail ()
              in
                let bitwidth =
                  components pdf resources cspace * bpc * width
                in
                  let bytewidth =
                    if bitwidth mod 8 = 0 then bitwidth / 8 else bitwidth / 8 + 1
                  in
                    bytewidth * height
            in
              let data =
                match lookup_direct_orelse empty "/F" "/Filter" dict with
                | None | Some (Array []) ->
                    begin try let data = mkstream bytes in
                      if bytes > 0 then
                        for x = 0 to stream_size data - 1 do
                          data.{x} <- i.input_byte ();
                        done;
                      data
                    with
                    | e -> print_string (Printexc.to_string e); raise e
                    end
                | Some _ ->
                    try
                      match Pdfcodec.decode_from_input i dict with
                      | None -> raise Couldn'tHandleContent
                      | Some data -> data 
                    with
                      | Pdfcodec.DecodeNotSupported ->
                          raise Couldn'tHandleContent
                      | Pdfcodec.Couldn'tDecodeStream r ->
                          raise (PDFError ("Inline image, bad data: " ^ r))
                      | e -> raise e
              in
                (* Read EI token *)
                dropwhite i;
                let c = i.input_char () in
                  let c' = i.input_char () in
                    begin match c, c' with
                    | 'E', 'I' ->
                        (* Remove filter, predictor. *)
                        let dict' =
                          fold_left
                            remove_dict_entry
                            dict
                            ["/Filter"; "/F"; "/DecodeParms"; "/DP"] 
                        in
                          (*i flprint "successful end of lex_inline_image\n"; i*)
                          dict', data
                    | _ ->
                        fail ()
                    end
        | _ -> fail ()

(* Lex a keyword. *)
let lex_keyword pdf resources i =
  match implode (getuntil_white_or_delimiter i) with
  | "true" -> Some (Obj (LexBool true))
  | "false" -> Some (Obj (LexBool false))
  | "BI" -> Some (LexInlineImage (lex_inline_image pdf resources i))
  | "ID" | "EI" -> None (*r [lex_inline_image] should consume these *)
  | ( "w" | "J" | "j" | "M" | "d" | "ri" | "i" | "gs"
    | "q" | "Q" | "cm" | "m" | "l" | "c" | "v" | "y"
    | "h" | "re" | "S" | "s" | "f" | "F" | "f*" | "B"
    | "B*" | "b" | "b*" | "n" | "W" | "W*" | "BT" | "ET"
    | "Tc" | "Tw" | "Tz" | "TL" | "Tf" | "Tr" | "Ts"
    | "Td" | "TD" | "Tm" | "T*" | "Tj" | "TJ" | "\'"
    | "\'\'" | "d0" | "d1" | "CS" | "cs" | "SC" | "SCN"
    | "sc" | "scn" | "G" | "g" | "RG" | "rg" | "K" | "k"
    | "sh" | "Do" | "MP" | "DP" | "BMC"
    | "BDC" | "EMC" | "BX" | "EX" ) as opstring ->
        Some (Op opstring)
  | _ -> None

(* Lex a string. *)
let lex_string i =
  match Pdfread.lex_string i with
  | LexString str -> Some (Obj (LexString str))
  | _ -> None

(* Lex a hexadecimal string. *)
let lex_hexstring i =
  match Pdfread.lex_hexstring i with
  | LexString str -> Some (Obj (LexString str))
  | _ -> None

(* Lex one token *)
let lex_next pdf resources i =
  try
    dropwhite i;
    match peek_char i with
    | '/' -> lex_name i
    | '0'..'9' | '+' | '-' | '.' -> lex_number i
    | 'A'..'Z' | 'a'..'z' | '\'' -> lex_keyword pdf resources i
    | '(' -> lex_string i
    | '[' -> nudge i; Some (Obj (LexLeftSquare))
    | ']' -> nudge i; Some (Obj (LexRightSquare))
    | '<' -> 
        begin match nudge i; let c = peek_char i in rewind i; c with
        | '<' -> get_dictionary i
        | _ -> lex_hexstring i
        end
    | '%' -> ignore (Pdfread.lex_comment i); Some (LexComment)
    | _ ->
        (*i flprint "\nLexing failure: chars: ";
        for x = 0 to 60 do
          print_char (i.input_char ())
        done; i*)
        raise (PDFSemanticError "Lexing failure in content stream")
  with
    | PDFReadError r -> 
        raise (PDFReadError ("Pdfpages.lex_next => " ^ r))
    | EndOfInput -> None 
    | Couldn'tHandleContent -> None

let print_lexeme = function
  | Obj p -> print_lexeme p
  | Op s -> print_string s; print_newline ()
  | PdfObj p -> print_string "PDF OBJECT\n"
  | LexInlineImage _ -> print_string "INLINE IMAGE\n"
  | LexComment -> print_string "COMMENT\n"

(* Lex a graphics stream *)
let rec lex_stream pdf resources i lexemes =
  match lex_next pdf resources i with
  | None -> rev lexemes
  | Some LexComment ->
      lex_stream pdf resources i lexemes
  | Some lexeme ->
      lex_stream pdf resources i (lexeme::lexemes)

(* \section{Parsing} *)
  
(* Parse a single operator and its operands, provided as a lexeme list. The
string from which these lexemes were extracted is provided so that [Op_Unknown]
instances can be generated. The compatibility level is also provided, and may be
updated. *)
let parse_operator compatibility string = function
  | [Obj (LexReal tx); Obj (LexReal ty); Op "Td"] -> Op_Td (tx, ty)
  | [Obj (LexReal tx); Obj (LexReal ty); Op "TD"] -> Op_TD (tx, ty)
  | [Obj (LexReal width); Op "w"] -> Op_w width
  | [Obj (LexReal cap); Op "J"] -> Op_J (int_of_float cap)
  | [Obj (LexReal join); Op "j"] -> Op_j (int_of_float join)
  | [Op "W"] -> Op_W
  | [Op "W*"] -> Op_W'
  | [Op "q"] -> Op_q
  | [Op "Q"] -> Op_Q
  | [Op "h"] -> Op_h
  | [Op "n"] -> Op_n
  | [Obj (LexReal x); Obj (LexReal y); Op "m"] -> Op_m (x, y)
  | [Obj (LexReal x); Obj (LexReal y); Op "l"] -> Op_l (x, y)
  | [Op "f*"] -> Op_f'
  | [Op "f"] -> Op_f
  | [Op "F"] -> Op_F
  | [Op "BT"] -> Op_BT
  | [Op "ET"] -> Op_ET
  | [Obj (LexReal leading); Op "TL"] -> Op_TL leading
  | [Obj (LexName n); Obj (LexReal s); Op "Tf"] -> Op_Tf (n, s)
  | [Op "T*"] -> Op_T'
  | [Obj (LexString s); Op "Tj"] -> Op_Tj s
  | [Obj (LexReal r); Obj (LexReal g); Obj (LexReal b); Op "RG"] ->
      Op_RG (r, g, b)
  | [Obj (LexReal r); Obj (LexReal g); Obj (LexReal b); Op "rg"] ->
      Op_rg (r, g, b)
  | [Obj (LexReal g); Op "G"] -> Op_G g
  | [Obj (LexReal g); Op "g"] -> Op_g g
  | [Obj (LexReal c); Obj (LexReal m);
     Obj (LexReal y); Obj (LexReal k); Op "k"] -> Op_k (c, m, y, k)
  | [Obj (LexReal c); Obj (LexReal m);
     Obj (LexReal y); Obj (LexReal k); Op "K"] -> Op_K (c, m, y, k)
  | [Obj (LexReal a); Obj (LexReal b); Obj (LexReal c); 
     Obj (LexReal d); Obj (LexReal e); Obj (LexReal f); Op "cm"] ->
       Op_cm
         {Transform.a = a; Transform.b = b; Transform.c = c;
          Transform.d = d; Transform.e = e; Transform.f = f}
  | [Obj (LexReal a); Obj (LexReal b); Obj (LexReal c); 
     Obj (LexReal d); Obj (LexReal e); Obj (LexReal f); Op "Tm"] ->
       Op_Tm
         {Transform.a = a; Transform.b = b; Transform.c = c;
          Transform.d = d; Transform.e = e; Transform.f = f}
  | [Obj (LexName n); Op "MP"] -> Op_MP n
  | [Obj (LexName n); PdfObj p; Op "DP"] -> Op_DP (n, p)
  | [Obj (LexName n); Obj o; Op "DP"] ->
      let p = snd (Pdfread.parse [o]) in Op_DP (n, p)
  | [Obj (LexName n); Op "BMC"] -> Op_BMC n
  | [Obj (LexName n); PdfObj p; Op "BDC"] -> Op_BDC (n, p)
  | [Obj (LexName n); Obj o; Op "BDC"] ->
      let p = snd (Pdfread.parse [o]) in Op_BDC (n, p)
  | [Op "EMC"] -> Op_EMC
  | [Obj (LexName n); Op "gs"] -> Op_gs n
  | [Obj (LexName n); Op "Do"] -> Op_Do n
  | [Obj (LexName n); Op "CS"] -> Op_CS n
  | [Obj (LexName n); Op "cs"] -> Op_cs n
  | [Obj (LexReal x1); Obj (LexReal y1); Obj (LexReal x2);
     Obj (LexReal y2); Obj (LexReal x3); Obj (LexReal y3);
     Op "c"] -> Op_c (x1, y1, x2, y2, x3, y3)
  | [Obj (LexReal x2); Obj (LexReal y2);
     Obj (LexReal x3); Obj (LexReal y3);
     Op "v"] -> Op_v (x2, y2, x3, y3)
  | [Obj (LexReal x1); Obj (LexReal y1);
     Obj (LexReal x3); Obj (LexReal y3);
     Op "y"] -> Op_y (x1, y1, x3, y3)
  | [Op "B"] -> Op_B
  | [Op "B*"] -> Op_B'
  | [Op "b"] -> Op_b
  | [Op "b*"] -> Op_b'
  | [Op "S"] -> Op_S
  | [Op "s"] -> Op_s
  | [Obj (LexReal x); Obj (LexReal y);
     Obj (LexReal w); Obj (LexReal h);
     Op "re"] -> Op_re (x, y, w, h)
  | [Obj (LexName n); Op "ri"] -> Op_ri n
  | [Obj (LexReal i); Op "i"] -> Op_i (int_of_float i)
  | [Op "BX"] -> incr compatibility; Op_BX
  | [Op "EX"] -> decr compatibility; Op_EX
  | [Obj (LexReal m); Op "M"] -> Op_M m
  | [Obj (LexString s); Op "\'"] -> Op_' s
  | [Obj (LexReal aw); Obj (LexReal ac); Obj (LexString s); Op "\'\'"] ->
      Op_'' (aw, ac, s)
  | [Obj (LexReal wx); Obj (LexReal wy); Op "d0"] ->
      Op_d0 (wx, wy)
  | [Obj (LexReal wx); Obj (LexReal wy);
     Obj (LexReal llx); Obj (LexReal lly);
     Obj (LexReal urx); Obj (LexReal ury); Op "d1"] ->
      Op_d1 (wx, wy, llx, lly, urx, ury)
  | [Obj (LexName n); Op "sh"] -> Op_sh n
  | [Obj (LexReal tc); Op "Tc"] -> Op_Tc tc
  | [Obj (LexReal tw); Op "Tw"] -> Op_Tw tw
  | [Obj (LexReal tz); Op "Tz"] -> Op_Tz tz
  | [Obj (LexReal tr); Op "Tr"] -> Op_Tr (int tr)
  | [Obj (LexReal ts); Op "Ts"] -> Op_Ts ts
  | [LexInlineImage d] -> InlineImage d
  | ls ->
      (* More complicated things are parsed by reversing the lexemes so we may
      inspect the operator. *)
      let reals_of_real_lexemes errtext lexemes =
        let real_of_real_lexeme errtext = function
          | Obj (LexReal n) -> n
          | _ -> raise (PDFSemanticError errtext)
        in
          map (real_of_real_lexeme errtext) lexemes
      in
        match rev ls with
        | Op "sc"::nums ->
            Op_sc (rev (reals_of_real_lexemes "Malformed 'sc'" nums))
        | Op "SC"::nums ->
            Op_SC (rev (reals_of_real_lexemes "Malformed 'SC'" nums))
        | Op "scn"::Obj (LexName n)::rest ->
            Op_scnName (n, rev (reals_of_real_lexemes "scn" rest))
        | Op "SCN"::Obj (LexName n)::rest ->
            Op_SCNName (n, rev (reals_of_real_lexemes "SCN" rest))
        | Op "scn"::nums ->
            Op_scn (rev (reals_of_real_lexemes "Malformed 'scn'" nums))
        | Op "SCN"::nums ->
            Op_SCN (rev (reals_of_real_lexemes "Malformed 'SCN'" nums))
        | Op "d"::Obj (LexReal phase)::Obj LexRightSquare::r ->
            begin match rev r with
            | Obj LexLeftSquare::t ->
                let reals =
                  map
                    (function
                     | (Obj (LexReal i)) -> i
                     | _ ->
                       raise (PDFSemanticError "malformed 'd' op"))
                    t
                in
                  Op_d (reals, phase) 
            | _ -> raise (PDFSemanticError "malformed 'd' op")
            end
        | Op "TJ"::Obj LexRightSquare::r ->
            begin match rev r with
            | Obj LexLeftSquare::t ->
                let elements =
                  map
                    (function
                     | (Obj (LexReal i)) -> Real i
                     | (Obj (LexString s)) -> String s
                     | _ -> raise (PDFSemanticError "malformed TJ elt"))
                    t
                in
                  Op_TJ (Array elements)
            | _ -> raise (PDFSemanticError "malformed TJ op")
            end
        | Op _::_ -> Op_Unknown string
        | _ -> failwith "Empty or malformed graphics operation."

(* Split the lexemes into sections (zero or more operands followed by an
operator) and parse each. *)
let split s =
  let h, t =
    cleavewhile (function Op _ | LexInlineImage _ -> false | _ -> true) s
  in
    match t with
    | [] -> raise (PDFSemanticError "premature graphics stream end")
    | f::l -> append h [f], l

let rec parse_lexemes compatibility ls ops =
  let make_real = function
    | Obj (LexInt i) -> Obj (LexReal (float_of_int i))
    | x -> x
  in
    match ls with
    | [] -> rev ops
    | _ ->
      let section, remaining = split ls in
        let op =
          parse_operator
            compatibility
            (string_of_lexemes section)
            (map make_real section)
        in
          parse_lexemes compatibility remaining (op::ops)


(* \intf Parse, given a list of streams. The contents of a single PDF page can be
split over several streams, which must be concatenated at the lexeme level.
*)

(* Concatenate bytestreams, padding with whitespace *)
let concat_bytestreams ss =
  let total_length = fold_left ( + ) 0 (map stream_size ss) in
    let s' = mkstream (total_length + length ss) in
      let p = ref 0 in
        iter
          (fun s ->
             for x = 0 to stream_size s - 1 do
               s'.{!p} <- s.{x};
               incr p
             done;
             s'.{!p} <- int_of_char ' ';
             incr p)
          ss;
        s'

let parse_stream pdf resources streams =
  let stream = match streams with [s] -> s | _ -> concat_bytestreams streams in
  let i = input_of_bytestream stream in
    let lexemes = lex_stream pdf resources i [] in
      parse_lexemes (ref 0) lexemes []

(* \intf Parse the operators in a list of streams. *)
let parse_operators pdf resources streams =
  let rawstreams =
    map
      (fun c ->
        Pdfcodec.decode_pdfstream pdf c;
        bigarray_of_stream c)
      streams
  in
    parse_stream pdf resources rawstreams
    
(* \section{Flattening} *)
          
(* Give a bigarray representing a list of graphics operators. *)
let stream_of_lexemes ls =
  let oplists = ref []
  and ls = ref ls in
    while !ls <> [] do
      let l, l' = split !ls in
        ls := l';
        oplists =:: l
    done;
    let strings =
      rev_map
        (fun ls ->
           let s = string_of_lexemes ls in
             if lastchar s <> Some ' ' then s ^ " " else s)
        !oplists
    in
      let total_length =
        fold_left ( + ) 0 (map String.length strings)
      in
        let s = mkstream total_length
        and strings = ref strings
        and pos = ref 0 in
          while !strings <> [] do
            let str = hd !strings in
              let l = String.length str in
                if l > 0 then
                  for n = 0 to l - 1 do
                    s.{!pos} <- int_of_char str.[n];
                    incr pos
                  done;
                strings := tl !strings
          done;
          s

let print_stream s =
  if stream_size s > 0 then 
    for x = 0 to stream_size s - 1 do
      Printf.printf "%c" (char_of_int s.{x})
    done;
  print_newline ()
    
(* \intf Make a stream from a list of operators. *)
let stream_of_ops ops =
  let data = stream_of_lexemes (flatten (map lexemes_of_op ops)) in
    Pdf.Stream
      (ref (Pdf.Dictionary [("/Length", Pdf.Integer (stream_size data))], Pdf.Got data))


