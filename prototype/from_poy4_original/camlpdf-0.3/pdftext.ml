(* \chaptertitle{PDFText}{Reading and writing text} *)
open Utility

(* \section{Data type for fonts} *)

(* Type 3 Specific Glyph Data *)
type type3_glpyhs =
  {fontbbox : float * float * float * float;
   fontmatrix : Transform.transform_matrix;
   charprocs : (string * Pdf.pdfobject) list;
   type3_resources : Pdf.pdfobject}

(* A font is either one of the standard 14 fonts, a simple font, or.. *)
type simple_fonttype =
  | Type1
  | MMType1
  | Type3 of type3_glpyhs
  | Truetype

type fontmetrics = int array (*r widths of glyphs 0..255 *)

(* The fontfile is an indirect reference into the document, rather than a
PDFobject itself. This preserves polymorphic equality (a pdfobject can contain
functional values *)
type fontdescriptor =
  {ascent : float;
   descent : float;
   leading : float;
   avgwidth : float;
   maxwidth : float;
   fontfile : int option}

type differences = (string * int) list

type encoding =
  | ImplicitInFontFile
  | StandardEncoding
  | MacRomanEncoding
  | WinAnsiEncoding
  | MacExpertEncoding
  | CustomEncoding of encoding * differences
  | FillUndefinedWithStandard of encoding

type simple_font =
  {fonttype : simple_fonttype;
   basefont : string;
   fontmetrics : fontmetrics option;
   fontdescriptor : fontdescriptor option;
   encoding : encoding}

type standard_font =
  | TimesRoman
  | TimesBold
  | TimesItalic
  | TimesBoldItalic
  | Helvetica
  | HelveticaBold
  | HelveticaOblique
  | HelveticaBoldOblique
  | Courier
  | CourierBold
  | CourierOblique
  | CourierBoldOblique
  | Symbol
  | ZapfDingbats

type cid_system_info =
  {registry : string;
   ordering : string;
   supplement : int}

type composite_CIDfont =
  {cid_system_info : cid_system_info;
   cid_basefont : string;
   cid_fontdescriptor : fontdescriptor;
   cid_widths : (int * int) list;
   cid_default_width : int}

type cmap_encoding =
  | Predefined of string
  | CMap of int (* indirect reference to CMap stream *)

type font =
  | StandardFont of standard_font * encoding
  | SimpleFont of simple_font
  | CIDKeyedFont of string * composite_CIDfont * cmap_encoding (* string is top-level basefont *)

let read_type3_data pdf font =
  {fontbbox =
     (let obj = Pdf.lookup_fail "No fontbbox" pdf "/FontBBox" font in
       Pdf.parse_rectangle obj);
   fontmatrix =
     Pdf.parse_matrix pdf "/FontMatrix" font;
   charprocs = 
     (match Pdf.lookup_fail "Bad Charprocs" pdf "/CharProcs" font with
      | Pdf.Dictionary l -> l
      | _ -> raise (Pdfread.PDFSemanticError "Bad charprocs")); 
   type3_resources =
     (match Pdf.lookup_direct pdf "/Resources" font with
      | None -> Pdf.Dictionary []
      | Some d -> d)}

let simple_fonttype_of_string pdf font = function
  | "/Type1" -> Some Type1
  | "/MMType1" -> Some MMType1
  | "/Type3" ->
      Some (Type3 (read_type3_data pdf font))
  | "/TrueType" -> Some Truetype
  | _ -> None

let read_basefont pdf font =
  match Pdf.lookup_direct pdf "/BaseFont" font with
  | Some (Pdf.Name n) -> n
  | _ -> ""

let read_fontdescriptor pdf font =
  match Pdf.lookup_direct pdf "/FontDescriptor" font with
  | None -> None
  | Some fontdescriptor ->
      let ascent =
        match Pdf.lookup_direct pdf "/Ascent" fontdescriptor with
        | Some x -> Pdf.getnum x
        | None -> 0.
      and descent =
        match Pdf.lookup_direct pdf "/Descent" fontdescriptor with
        | Some x -> Pdf.getnum x
        | None -> 0.
      and leading =
        match Pdf.lookup_direct pdf "/Leading" fontdescriptor with
        | Some x -> Pdf.getnum x
        | None -> 0.
      and avgwidth =
        match Pdf.lookup_direct pdf "/AvgWidth" fontdescriptor with
        | Some x -> Pdf.getnum x
        | None -> 0.
      and maxwidth =
        match Pdf.lookup_direct pdf "/MaxWidth" fontdescriptor with
        | Some x -> Pdf.getnum x
        | None -> 0.
      and fontfile =
        match Pdf.find_indirect "/FontFile" fontdescriptor with
        | Some i -> Some i
        | None ->
            match Pdf.find_indirect "/FontFile2" fontdescriptor with
            | Some i -> Some i
            | None ->
                Pdf.find_indirect "/FontFile3" fontdescriptor
      in
        Some
          {ascent = ascent;
           descent = descent;
           leading = leading;
           avgwidth = avgwidth;
           maxwidth = maxwidth;
           fontfile = fontfile}

(* Read the widths from a font. Normally in the font descriptor, but in Type3
fonts at the top level. *)
let read_metrics pdf font =
  let fontdescriptor =
    match Pdf.lookup_direct pdf "/Subtype" font with
    | Some (Pdf.Name "/Type3") -> Some font
    | _ -> Pdf.lookup_direct pdf "/FontDescriptor" font
  in
    match fontdescriptor with
    | None -> None
    | Some fontdescriptor ->
        let firstchar =
          match Pdf.lookup_direct pdf "/FirstChar" font with
          | Some (Pdf.Integer i) ->
              if i <= 255 && i >= 0 then i else
                raise (Pdf.PDFError "Bad /Firstchar")
          | _ -> raise (Pdf.PDFError "No /FirstChar")
        and lastchar =
          match Pdf.lookup_direct pdf "/LastChar" font with
          | Some (Pdf.Integer i) ->
              if i <= 255 && i >= 0 then i else
                raise (Pdf.PDFError "Bad /Lastchar")
          | _ -> raise (Pdf.PDFError "No /LastChar")
        and missingwidth =
          match Pdf.lookup_direct pdf "/MissingWidth" fontdescriptor with
          | Some (Pdf.Integer w) -> w
          | _ -> 0
        in
          let elts =
            match Pdf.lookup_direct pdf "/Widths" font with
            | Some (Pdf.Array elts) -> elts
            | _ -> raise (Pdf.PDFError "No /Widths")
          in
            if length elts <> lastchar - firstchar + 1
              then raise (Pdf.PDFError "Bad /Widths")
              else
                let before =
                  many missingwidth firstchar
                and given =
                  map
                    (fun elt ->
                       match Pdf.direct pdf elt with
                       | Pdf.Integer i -> i
                       | _ -> raise (Pdf.PDFError "Bad /Width entry"))
                    elts
                and after =
                  many missingwidth (255 - lastchar)
                in
                  Some (Array.of_list (before @ given @ after))

(* Parse a /Differences entry to get a list of (name, number) pairs *)
let pairs_of_differences pdf differences =
  let rec groups_of_differences prev elts =
    match elts with
    | [] -> prev
    | Pdf.Integer n::rest ->
        let stripname = function Pdf.Name n -> n | _ -> assert false in
          let names, more =
            cleavewhile (function Pdf.Name _ -> true | _ -> false) rest
          in
            groups_of_differences ((n, map stripname names)::prev) more
    | _ -> raise (Pdf.PDFError "Malformed /Differences")
  and mappings_of_group (x, es) =
    let additions = ilist 0 (length es - 1) in
      map2 (fun e a -> (x + a, e)) es additions
  in
    match differences with
    | Pdf.Array elts ->
        let direct_elements = map (Pdf.direct pdf) elts in
          let groups = groups_of_differences [] direct_elements in
            map
             (fun (k, v) -> (v, k))
             (flatten (map mappings_of_group groups))
    | _ -> raise (Pdf.PDFError "Bad /Differences")

let standard_font_of_name = function
  | "/Times-Roman" | "/TimesNewRoman" ->
      Some TimesRoman
  | "/Times-Bold" | "/TimesNewRoman,Bold" ->
      Some TimesBold
  | "/Times-Italic" | "/TimesNewRoman,Italic" ->
      Some TimesItalic
  | "/Times-BoldItalic" | "/TimesNewRoman,BoldItalic" ->
      Some TimesBoldItalic
  | "/Helvetica" | "/Arial" ->
      Some Helvetica
  | "/Helvetica-Bold" | "/Arial,Bold" ->
      Some HelveticaBold
  | "/Helvetica-Oblique" | "/Arial,Italic" ->
      Some HelveticaOblique
  | "/Helvetica-BoldOblique" | "/Arial,BoldItalic" ->
      Some HelveticaBoldOblique
  | "/Courier" | "/CourierNew" ->
      Some Courier
  | "/CourierBold" | "/CourierNew,Bold" ->
      Some CourierBold
  | "/Courier-Oblique" | "/CourierNew,Italic" ->
      Some CourierOblique
  | "/Courier-BoldOblique" | "/CourierNew,BoldItalic" ->
      Some CourierBoldOblique
  | "/Symbol" ->
      Some Symbol
  | "/ZapfDingbats" ->
      Some ZapfDingbats
  | _ ->
      None

(* Predicate: is it a standard 14 font? If it's been overriden (contains widths
etc, we treat it as a simple font. *)
let is_standard14font pdf font =
  match Pdf.lookup_direct pdf "/Subtype" font with
  | Some (Pdf.Name "/Type1") ->
      begin match Pdf.lookup_direct pdf "/BaseFont" font with
      | Some (Pdf.Name name) ->
          begin match standard_font_of_name name with
          | None -> false
          | Some _ ->
              (* Check to see if it's been overriden *)
              match Pdf.lookup_direct pdf "/Widths" font with
              | None -> true
              | _ -> false
          end
      | _ -> false
      end
  | _ -> false

(* Is a font embedded in the document? *)
let is_embedded pdf font =
  match Pdf.lookup_direct pdf "/FontDescriptor" font with
  | None -> false
  | Some fontdescriptor ->
      match
        Pdf.lookup_direct_orelse pdf "/FontFile" "/FontFile2" fontdescriptor
      with
      | Some _ -> true
      | None ->
          match Pdf.lookup_direct pdf "/Fontfile3" fontdescriptor with
          | Some _ -> true
          | None -> false

(* Is a font symbolic? (Doesn't deal with standard 14 Zapf and Symbol) *)
let is_symbolic pdf font =
  match Pdf.lookup_direct pdf "/FontDescriptor" font with
  | None -> false
  | Some fontdescriptor ->
      match Pdf.lookup_direct pdf "/Flags" fontdescriptor with
      | Some (Pdf.Integer flags) -> flags land (1 lsl 3) > 0
      | _ -> raise (Pdf.PDFError "No /Flags in font descriptor")

(* For now, not for truetype fonts: add pg 399-401 later. Need to clarify what
happens if a standard-14 font is overriden. *)
let read_encoding pdf font =
  match Pdf.lookup_direct pdf "/Encoding" font with
  | Some (Pdf.Name "/MacRomanEncoding") -> MacRomanEncoding
  | Some (Pdf.Name "/MacExpertEncoding") -> MacExpertEncoding
  | Some (Pdf.Name "/WinAnsiEncoding") -> WinAnsiEncoding
  | Some (Pdf.Dictionary _ as encdict) ->
      begin match Pdf.lookup_direct pdf "/Subtype" font with
      | Some
          (Pdf.Name (("/Type1" | "/MMType1" | "/Type3" | "/TrueType") as fonttype))
        ->
          let encoding =
            let base_encoding =
              match Pdf.lookup_direct pdf "/BaseEncoding" encdict with
              | Some (Pdf.Name "/MacRomanEncoding") -> MacRomanEncoding
              | Some (Pdf.Name "/MacExpertEncoding") -> MacExpertEncoding
              | Some (Pdf.Name "/WinAnsiEncoding") -> WinAnsiEncoding
              | None ->
                  if is_embedded pdf font
                  then (ImplicitInFontFile)
                    else if is_symbolic pdf font
                      then ImplicitInFontFile
                      else StandardEncoding
              | _ -> raise (Pdf.PDFError "unknown /BaseEncoding")
            in
              begin match Pdf.lookup_direct pdf "/Differences" encdict with
              | Some differences ->
                  CustomEncoding
                    (base_encoding, pairs_of_differences pdf differences)
              | _ -> base_encoding
              end
          in
            if fonttype = "/Truetype"
              then FillUndefinedWithStandard encoding
              else encoding
      | _ -> raise (Pdf.PDFError "Bad font /Subtype")
      end
  | _ -> ImplicitInFontFile

let read_simple_font pdf font =
  match Pdf.lookup_direct pdf "/Subtype" font with
  | Some (Pdf.Name n) ->
      begin match simple_fonttype_of_string pdf font n with
      | Some fonttype ->
          let fontdescriptor = read_fontdescriptor pdf font in
            SimpleFont
              {fonttype = fonttype;
               basefont = read_basefont pdf font;
               fontmetrics = read_metrics pdf font;
               fontdescriptor = fontdescriptor;
               encoding = read_encoding pdf font}
      | None -> raise (Pdf.PDFError "Not a simple font")
      end
  | _ -> raise (Pdf.PDFError "No font /Subtype")

(* Read a base 14 font *)
let read_standard14font pdf font =
  match Pdf.lookup_direct pdf "/BaseFont" font with
  | Some (Pdf.Name name) ->
      begin match standard_font_of_name name with
      | None -> raise (Pdf.PDFError "Not a base 14 font")
      | Some f -> StandardFont (f, read_encoding pdf font)
      end
  | _ -> raise (Pdf.PDFError "Bad base 14 font")

(* Predicate: is it a simple font, assuming it's not a standard 14 font. *)
let is_simple_font pdf font =
  match Pdf.lookup_direct pdf "/Subtype" font with
  | Some (Pdf.Name ("/Type1" | "/MMType1" | "/Type3" | "/TrueType")) -> true
  | _ -> false

(* Predicate: is it a CIDKeyed font? *)
let is_cidkeyed_font pdf font =
  match Pdf.lookup_direct pdf "/Subtype" font with
  | Some (Pdf.Name "/Type0") -> true
  | _ -> false

(* Read a CID system info dictionary *)
let read_cid_system_info pdf dict =
  {registry =
     begin match Pdf.lookup_direct pdf "/Registry" dict with
     | Some (Pdf.String s) -> s
     | _ -> raise (Pdf.PDFError "No /Registry")
     end;
   ordering =
     begin match Pdf.lookup_direct pdf "/Ordering" dict with
     | Some (Pdf.String s) -> s
     | _ -> raise (Pdf.PDFError "No /Ordering")
     end;
   supplement =
     begin match Pdf.lookup_direct pdf "/Supplement" dict with
     | Some (Pdf.Integer i) -> i
     | _ -> raise (Pdf.PDFError "No /Supplement")
     end}

(* This returns the explicit pairs, which need to be combined
with the default value to look a width up. *)
let rec read_cid_widths = function
  | Pdf.Integer c::Pdf.Array ws::more ->
      let nums =
        map
          (function
           | Pdf.Integer i -> i
           | _ -> raise (Pdf.PDFError "Bad /W array"))
        ws
      in
        combine (indxn c nums) nums @ read_cid_widths more
  | Pdf.Integer c_first::Pdf.Integer c_last::Pdf.Integer w::more ->
      if c_last <= c_first
        then raise (Pdf.PDFError "Bad /W array")
        else
          let pairs =
            combine
              (ilist c_first c_last)
              (many w (c_last - c_first + 1))
          in
            pairs @ read_cid_widths more
  | [] -> []
  | _ -> raise (Pdf.PDFError "Malformed /W in CIDfont")

(* Read a composite CID font *)
(* FIXME: Doesn't support vertical modes (DW2 / W2) *)
let read_descendant pdf dict =
  let cid_system_info =
    match Pdf.lookup_direct pdf "/CIDSystemInfo" dict with
    | Some cid_dict -> read_cid_system_info pdf cid_dict
    | None -> raise (Pdf.PDFError "No CIDSystemInfo")
  and cid_basefont =
    match Pdf.lookup_direct pdf "/BaseFont" dict with
    | Some (Pdf.Name n) -> n
    | _ -> raise (Pdf.PDFError "No /BaseFont")
  and cid_fontdescriptor =
    match read_fontdescriptor pdf dict with
    | Some f -> f
    | None -> raise (Pdf.PDFError "No FontDescriptor in CIDkeyed font")
  and cid_widths =
    match Pdf.lookup_direct pdf "/W" dict with
    | Some (Pdf.Array ws) -> read_cid_widths ws
    | _ -> []
  and default_width =
    match Pdf.lookup_direct pdf "/DW" dict with
    | Some (Pdf.Integer d) -> d
    | _ -> 1000
  in
    {cid_system_info = cid_system_info;
     cid_basefont = cid_basefont;
     cid_fontdescriptor = cid_fontdescriptor;
     cid_widths = cid_widths;
     cid_default_width = default_width}

(* Read a CIDKeyed (Type 0) font *)
let read_cidkeyed_font pdf font =
  let basefont =
    match Pdf.lookup_direct pdf "/BaseFont" font with
    | Some (Pdf.Name b) -> b
    | _ -> raise (Pdf.PDFError "Bad /BaseFont")
  and composite_CIDfont =
    match Pdf.lookup_direct pdf "/DescendantFonts" font with
    | Some (Pdf.Array [e]) ->
        read_descendant pdf (Pdf.direct pdf e)
    | _ -> raise (Pdf.PDFError "Bad descendant font")
  and encoding =
    match Pdf.lookup_direct pdf "/Encoding" font with
    | Some (Pdf.Name e) -> Predefined e
    | Some (Pdf.Stream _) ->
        begin match Pdf.find_indirect "/Encoding" font with
        | Some n -> CMap n
        | None -> raise (Pdf.PDFError "malformed /Encoding")
        end
    | _ -> raise (Pdf.PDFError "malformed or missing /Encoding")
  in
    CIDKeyedFont (basefont, composite_CIDfont, encoding)

(* Reads a font *)
let read_font pdf font =
  if is_standard14font pdf font
    then read_standard14font pdf font
    else if is_simple_font pdf font
      then read_simple_font pdf font
      else if is_cidkeyed_font pdf font
        then read_cidkeyed_font pdf font
        else raise (Pdf.PDFError "Unknown font type")

(* \section{Font encodings} *)

(* Standard encoding *)
let name_to_standard =
  ["/A", 0o101; "/AE", 0o341; "/B", 0o102; "/C", 0o103; "/D", 0o104; "/E",
  0o105; "/F", 0o106; "/G", 0o107; "/H", 0o110; "/I", 0o111; "/J", 0o112; "/K",
  0o113; "/L", 0o114; "/Lslash", 0o350; "/M", 0o115; "/N", 0o116; "/O", 0o117;
  "/OE", 0o352; "/Oslash", 0o351; "/P", 0o120; "/Q", 0o121; "/R", 0o122; "/S",
  0o123; "/T", 0o124; "/U", 0o125; "/V", 0o126; "/W", 0o127; "/X", 0o130; "/Y",
  0o131; "/Z", 0o132; "/a", 0o140; "/acute", 0o302; "/ae", 0o361; "/ampersand",
  0o046; "/asciicircum", 0o136; "/asciitilde", 0o176; "/asterisk", 0o052; "/at",
  0o100; "/b", 0o142; "/backslash", 0o134; "/bar", 0o174; "/braceleft", 0o173;
  "/braceright", 0o175; "/bracketleft", 0o133; "/bracketright", 0o135; "/breve",
  0o306; "/bullet", 0o267; "/c", 0o143; "/caron", 0o317; "/cedilla", 0o313;
  "/cent", 0o242; "/circumflex", 0o303; "/colon", 0o072; "/comma", 0o054;
  "/currency", 0o250; "/d", 0o144; "/dagger", 0o262; "/daggerdbl", 0o263;
  "/dieresis", 0o310; "/dollar", 0o044; "/dotaccent", 0o307; "/dottlessi",
  0o365; "/e", 0o145; "/eight", 0o070; "/ellipsis", 0o274; "/emdash", 0o320;
  "/endash", 0o261; "/equal", 0o075; "/exclam", 0o041; "/exclamdown", 0o241;
  "/f", 0o146; "/fi", 0o256; "/five", 0o065; "/fl", 0o257; "/florin", 0o246;
  "/four", 0o064; "/fraction", 0o244; "/g", 0o147; "/germandbls", 0o373;
  "/grave", 0o301; "/greater", 0o076; "/guillemotleft", 0o253;
  "/guillemotright", 0o273; "/guilsinglleft", 0o254; "/guilsinglright", 0o255;
  "/h", 0o150; "/hungarumlaut", 0o315; "/hyphen", 0o055; "/i", 0o151; "/j",
  0o152; "/k", 0o153; "/l", 0o154; "/less", 0o074; "/lslash", 0o370; "/m",
  0o155; "/macron", 0o305; "/n", 0o156; "/nine", 0o071; "/numbersign", 0o043;
  "/o", 0o157; "/oe", 0o372; "/ogonek", 0o316; "/one", 0o316; "/ordfeminine",
  0o343; "/ordmasculine", 0o353; "/oslash", 0o361; "/p", 0o160; "/paragraph",
  0o266; "/parenleft", 0o050; "/parenright", 0o051; "/percent", 0o045;
  "/period", 0o056; "/periodcentered", 0o264; "/perthousand", 0o275; "/plus",
  0o053; "/q", 0o161; "/question", 0o077; "/questiondown", 0o277; "/quotedbl",
  0o042; "/quotedblbase", 0o271; "/quotedblleft", 0o252; "/quotedblright",
  0o272; "/quoteleft", 0o140; "/quoteright", 0o047; "/quotesinglbase", 0o270;
  "/quotesingle", 0o251; "/r", 0o162; "/ring", 0o312; "/s", 0o163; "/section",
  0o247; "/semicolon", 0o073; "/seven", 0o067; "/six", 0o066; "/slash", 0o057;
  "/space", 0o040; "/sterling", 0o243; "/t", 0o164; "/three", 0o063; "/tilde",
  0o304; "/two", 0o062; "/u", 0o165; "/underscore", 0o137; "/v", 0o166; "/w",
  0o167; "/x", 0o170; "/y", 0o171; "/yen", 0o245; "/z", 0o172; "/zero", 0o060]
 
(* Mac Roman Encoding *)
let name_to_macroman =
  ["/A", 0o101; "/AE", 0o256; "/Aacute", 0o347; "/Acircumflex", 0o345;
  "/Adieresis", 0o200; "/Agrave", 0o313; "/Aring", 0o201; "/Atilde", 0o314;
  "/B", 0o102; "/C", 0o103; "/Ccedilla", 0o202; "/D", 0o104; "/E", 0o105;
  "/Eacute", 0o203; "/Ecircumflex", 0o346; "/Edieresis", 0o350; "/Egrave",
  0o351; "/F", 0o106; "/G", 0o107; "/H", 0o110; "/I", 0o111; "/Iacute", 0o352;
  "/Icircumflex", 0o353; "/Idieresis", 0o354; "/Igrave", 0o355; "/J", 0o112;
  "/K", 0o113; "/L", 0o114; "/M", 0o115; "/N", 0o116; "/Ntilde", 0o204; "/O",
  0o117; "/OE", 0o316; "/Oacute", 0o356; "/Ocircumflex", 0o357; "/Odieresis",
  0o205; "/Ograve", 0o361; "/Oslash", 0o257; "/Otilde", 0o315; "/P", 0o120;
  "/Q", 0o121; "/R", 0o122; "/S", 0o123; "/T", 0o124; "/U", 0o125; "/Uacute",
  0o362; "/Ucircumflex", 0o363; "/Udieresis", 0o206; "/Ugrave", 0o364; "/V",
  0o126; "/W", 0o127; "/X", 0o130; "/Y", 0o131; "/Ydieresis", 0o331; "/Z",
  0o132; "/a", 0o141; "/aacute", 0o207; "/acircumflex", 0o211; "/acute", 0o253;
  "/adieresis", 0o212; "/ae", 0o276; "/agrave", 0o210; "/ampersand", 0o046;
  "/aring", 0o214; "/asciicircum", 0o136; "/asciitilde", 0o176; "/asterisk",
  0o052; "/at", 0o100; "/atilde", 0o213; "/b", 0o142; "/backslash", 0o134;
  "/bar", 0o174; "/braceleft", 0o173; "/braceright", 0o175; "/bracketleft",
  0o133; "/bracketright", 0o135; "/breve", 0o371; "/bullet", 0o245; "/c", 0o143;
  "/caron", 0o377; "/ccedilla", 0o215; "/cedilla", 0o374; "/cent", 0o242;
  "/circumflex", 0o366; "/colon", 0o072; "/comma", 0o054; "/copyright", 0o251;
  "/currency", 0o333; "/d", 0o144; "/dagger", 0o240; "/daggerdbl", 0o340;
  "/degree", 0o241; "/dieresis", 0o254; "/divide", 0o326; "/dollar", 0o044;
  "/dotaccent", 0o372; "/dotlessi", 0o365; "/e", 0o145; "/eacute", 0o216;
  "/ecircumflex", 0o220; "/edieresis", 0o221; "/egrave", 0o217; "/eight", 0o070;
  "/ellipsis", 0o311; "/emdash", 0o321; "/endash", 0o320; "/equal", 0o075;
  "/exclam", 0o041; "/exclamdown", 0o301; "/f", 0o146; "/fi", 0o336; "/five",
  0o065; "/fl", 0o337; "/florin", 0o304; "/four", 0o064; "/fraction", 0o332;
  "/g", 0o147; "/germandbls", 0o247; "/grave", 0o140; "/greater", 0o076;
  "/guillemotleft", 0o307; "/guillemotright", 0o310; "/guilsinglleft", 0o334;
  "/guilsinglright", 0o335; "/h", 0o150; "/hungrumlaut", 0o375; "/hyphen",
  0o055; "/i", 0o151; "/iacute", 0o222; "/icircumflex", 0o224; "/idieresis",
  0o225; "/igrave", 0o223; "/j", 0o152; "/k", 0o153; "/l", 0o154; "/less",
  0o074; "/logicalnot", 0o302; "/m", 0o155; "/macron", 0o370; "/mu", 0o265;
  "/n", 0o156; "/nine", 0o071; "/ntilde", 0o226; "/numbersign", 0o043; "/o",
  0o157; "/oacute", 0o227; "/ocircumflex", 0o231; "/odieresis", 0o232; "/oe",
  0o317; "/ogonek", 0o376; "/one", 0o061; "/ordfeminine", 0o273;
  "/ordmasculine", 0o274; "/oslash", 0o277; "/otilde", 0o233; "/p", 0o160;
  "/paragraph", 0o246; "/parenleft", 0o050; "/parenright", 0o051; "/percent",
  0o045; "/period", 0o056; "/periodcentered", 0o341; "/perthousand", 0o344;
  "/plus", 0o053; "/plusminus", 0o261; "/q", 0o161; "/question", 0o077;
  "/questiondown", 0o300; "/quotedbl", 0o042; "/quotedblbase", 0o343;
  "/quotedblleft", 0o322; "/quotedblright", 0o325; "/quoteleft", 0o324;
  "/quoteright", 0o325; "/quotesinglbase", 0o342; "/quotesingle", 0o047; "/r",
  0o162; "/registered", 0o250; "/ring", 0o373; "/s", 0o163; "/section", 0o244;
  "/semicolon", 0o073; "/seven", 0o067; "/six", 0o066; "/slash", 0o057;
  "/space", 0o040; "/sterling", 0o243; "/t", 0o164; "/three", 0o063; "/tilde",
  0o367; "/trademark", 0o252; "/two", 0o062; "/u", 0o165; "/uacute", 0o234;
  "/ucircumflex", 0o236; "/udieresis", 0o237; "/ugrave", 0o235; "/underscore",
  0o137; "/v", 0o166; "/w", 0o167; "/x", 0o170; "/y", 0o171; "/ydieresis",
  0o330; "/yen", 0o264; "/z", 0o172; "/zero", 0o060]

(* Win Ansi Encoding *)
let name_to_win =
  ["/A", 0o101; "/AE", 0o306; "/Aacute", 0o301; "/Acircumflex", 0o302;
  "/Adieresis", 0o304; "/Agrave", 0o300; "/Aring", 0o305; "/Atilde", 0o303;
  "/B", 0o102; "/C", 0o103; "/Ccedilla", 0o307; "/D", 0o104; "/E", 0o105;
  "/Eacute", 0o311; "/Ecircumflex", 0o312; "/Edieresis", 0o313; "/Egrave",
  0o310; "/Eth", 0o320; "/Euro", 0o240; "/F", 0o106; "/G", 0o107; "/H", 0o110;
  "/I", 0o111; "/Iacute", 0o315; "/Icircumflex", 0o316; "/Idieresis", 0o317;
  "/Igrave", 0o314; "/J", 0o112; "/K", 0o113; "/L", 0o114; "/M", 0o115; "/N",
  0o116; "/Ntilde", 0o321; "/O", 0o117; "/OE", 0o214; "/Oacute", 0o323;
  "/Ocircumflex", 0o324; "/Odieresis", 0o326; "/Ograve", 0o322; "/Oslash",
  0o330; "/Otilde", 0o325; "/P", 0o120; "/Q", 0o121; "/R", 0o122; "/S", 0o123;
  "/Scaron", 0o212; "/T", 0o124; "/Thorn", 0o336; "/U", 0o125; "/Uacute", 0o332;
  "/Ucircumflex", 0o333; "/Udieresis", 0o334; "/Ugrave", 0o331; "/V", 0o126;
  "/W", 0o127; "/X", 0o130; "/Y", 0o131; "/Yacute", 0o335; "/Ydieresis", 0o237;
  "/Z", 0o132; "/Zcaron", 0o216; "/a", 0o141; "/aacute", 0o341; "/acircumflex",
  0o342; "/acute", 0o264; "/adieresis", 0o344; "/ae", 0o346; "/agrave", 0o340;
  "/ampersand", 0o046; "/aring", 0o345; "/asciicircum", 0o136; "/asciitilde",
  0o176; "/asterisk", 0o052; "/at", 0o100; "/atilde", 0o343; "/b", 0o142;
  "/backslash", 0o134; "/bar", 0o174; "/braceleft", 0o173; "/braceright", 0o175;
  "/bracketleft", 0o133; "/bracketright", 0o135; "/brokenbar", 0o246; "/bullet",
  0o225; "/c", 0o143; "/ccedilla", 0o347; "/cedilla", 0o270; "/cent", 0o240;
  "/circumflex", 0o210; "/colon", 0o072; "/comma", 0o054; "/copyright", 0o251;
  "/currency", 0o244; "/d", 0o144; "/dagger", 0o206; "/daggerdbl", 0o207;
  "/degree", 0o260; "/dieresis", 0o250; "/divide", 0o367; "/dollar",0o044; "/e",
  0o145; "/eacute", 0o351; "/ecircumflex", 0o352; "/edieresis", 0o353;
  "/egrave", 0o350; "/eight", 0o070; "/ellipsis", 0o205; "/emdash", 0o204;
  "/endash", 0o226; "/equal", 0o075; "/eth", 0o360; "/exclam", 0o041;
  "/exclamdown", 0o241; "/f", 0o146; "/five", 0o065; "/florin", 0o203; "/four",
  0o064; "/g", 0o147; "/germandbls", 0o337; "/grave", 0o140; "/greater", 0o076;
  "/guillemotleft", 0o253; "/guillemotright", 0o273; "/guilsinglleft", 0o213;
  "/guilsinglright", 0o233; "/h", 0o150; "/hyphen", 0o055; "/i", 0o151;
  "/iacute", 0o355; "/icircumflex", 0o356; "/idieresis", 0o357; "/igrave",
  0o354; "/j", 0o152; "/k", 0o153; "/l", 0o154; "/less", 0o074; "/logicalnot",
  0o254; "/m", 0o155; "/macron", 0o257; "/mu", 0o265; "/multiply", 0o327; "/n",
  0o156; "/nine", 0o071; "/ntilde", 0o361; "/numbersign", 0o043; "/o", 0o157;
  "/oacute", 0o363; "/ocircumflex", 0o364; "/odieresis", 0o366; "/oe", 0o234;
  "/ograve", 0o362; "/one", 0o061; "/onehalf", 0o275; "/onequarter", 0o274;
  "/onesuperior", 0o271; "/ordfeminine", 0o252; "/ordmasculine", 0o272;
  "/oslash", 0o370; "/otilde", 0o365; "/p", 0o160; "/paragraph", 0o266;
  "/parenleft", 0o050; "/parenright", 0o051; "/percent", 0o045; "/period",
  0o056; "/periodcentered", 0o267; "/perthousand", 0o211; "/plus", 0o053;
  "/plusminus", 0o261; "/q", 0o161; "/question", 0o077; "/questiondown", 0o277;
  "/quotedbl", 0o042; "/quotedblbase", 0o204; "/quotedblleft", 0o223;
  "/quotedblright", 0o224; "/quoteleft", 0o221; "/quoteright", 0o222;
  "/quotesinglbase", 0o202; "/quotesingle", 0o047; "/r", 0o162; "/registered",
  0o256; "/s", 0o163; "/scaron", 0o232; "/section", 0o247; "/semicolon", 0o073;
  "/seven", 0o067; "/six", 0o066; "/slash", 0o057; "/space", 0o040; "/sterling",
  0o243; "/t", 0o164; "/thorn", 0o376; "/three", 0o063; "/threequarters", 0o276;
  "/threesuperior", 0o263; "/tilde", 0o230; "/trademark", 0o231; "/two", 0o062;
  "/twosuperior", 0o262; "/u", 0o165; "/uacute", 0o372; "/ucircumflex", 0o373;
  "/udieresis", 0o374; "/ugrave", 0o371; "/underscore", 0o137; "/v", 0o166;
  "/w", 0o167; "/x", 0o170; "/y", 0o171; "/yacute", 0o375; "/ydieresis", 0o377;
  "/yen", 0o245; "/z", 0o172; "/zcaron", 0o236; "/zero", 0o060]

(* Mac Expert Encoding *)
let name_to_macexpert =
  ["/AEsmall", 0o276; "/Aacutesmall", 0o207; "/Acircumflexsmall", 0o211;
  "/Acutesmall", 0o047; "/Adieresissmall", 0o212; "/Agravesmall", 0o210;
  "/Aringsmall", 0o214; "/Asmall", 0o141; "/Atildesmall", 0o213; "/Brevesmall",
  0o363; "/Bsmall", 0o142; "/Caronsmall", 0o256; "/Ccedillasmall", 0o215;
  "/Cedillasmall", 0o311; "/Circumflexsmall", 0o136; "/Csmall", 0o143;
  "/Dieresissmall", 0o254; "/Dotaccentsmall", 0o372; "/Dsmall", 0o144;
  "/Eacutesmall", 0o216; "/Ecircumflexsmall", 0o220; "/Edieresissmall", 0o221;
  "/Egravesmall", 0o217; "/Esmall", 0o145; "/Ethsmall", 0o104; "/Fsmall", 0o146;
  "/Gravesmall", 0o140; "/Gsmall", 0o147; "/Hsmall", 0o150;
  "/Hungarumlautsmall", 0o042; "/Iacutesmall", 0o222; "/Icircumflexsmall",
  0o224; "/Idieresissmall", 0o225; "/Igravesmall", 0o223; "/Ismall", 0o151;
  "/Jsmall", 0o152; "/Ksmall", 0o153; "/Lslashsmall", 0o302; "/Lsmall", 0o154;
  "/Macronsmall", 0o364; "/Msmall", 0o155; "/Nsmall", 0o156; "/Ntildesmall",
  0o226; "/OEsmall", 0o317; "/Oacutesmall", 0o227; "/Ocircumflexsmall", 0o231;
  "/Odieresissmall", 0o232; "/Ogoneksmall", 0o362; "/Ogravesmall", 0o230;
  "/Oslashsmall", 0o277; "/Osmall", 0o157; "/Otildesmall", 0o233; "/Psmall",
  0o160; "/Qsmall", 0o161; "/Ringsmall", 0o373; "/Rsmall", 0o162;
  "/Scaronsmall", 0o247; "/Ssmall", 0o163; "/Thornsmall", 0o271; "/Tildesmall",
  0o176; "/Tsmall", 0o164; "/Uacutesmall", 0o234; "/Ucircumflexsmall", 0o236;
  "/Udieresissmall", 0o237; "/Ugravesmall", 0o235; "/Usmall", 0o165; "/Vsmall",
  0o166; "/Wsmall", 0o167; "/Xsmall", 0o170; "/Yacutesmall", 0o264;
  "/Ydieresissmall", 0o330; "/Ysmall", 0o171; "/Zcaronsmall", 0o275; "/Zsmall",
  0o172; "/ampersandsmall", 0o046; "/asuperior", 0o201; "/bsuperior", 0o365;
  "/centinferior", 0o251; "/centoldstyle", 0o043; "/centsuperior", 0o202;
  "/colon", 0o072; "/colonmonetary", 0o173; "/comma", 0o054; "/commainferior",
  0o262; "/commasuperior", 0o370; "/dollarinferior", 0o266; "/dollaroldstyle",
  0o044; "/dsuperior", 0o353; "/eightinferior", 0o245; "/eightoldstyle", 0o070;
  "/eightsuperior", 0o241; "/esuperior", 0o344; "/exclamdownsmall", 0o326;
  "/exclamsmall", 0o041; "/ff", 0o126; "/ffi", 0o131; "/ffl", 0o132; "/fi",
  0o127; "/figuredash", 0o320; "/fiveeighths", 0o114; "/fiveinferior", 0o260;
  "/fiveoldstyle", 0o065; "/fivesuperior", 0o336; "/fl", 0o130; "/fourinferior",
  0o242; "/fouroldstyle", 0o064; "/foursuperior", 0o335; "/fraction", 0o057;
  "/hyphen", 0o055; "/hypheninferior", 0o137; "/hyphensuperior", 0o137;
  "/isuperior", 0o351; "/lsuperior", 0o361; "/msuperior", 0o367;
  "/nineinferior", 0o273; "/nineoldstyle", 0o071; "/ninesuperior", 0o341;
  "/nsuperior", 0o366; "/onedotenleader", 0o053; "/oneeighth", 0o112;
  "/onefitted", 0o174; "/onehalf", 0o110; "/oneinferior", 0o301; "/oneoldstyle",
  0o061; "/onequarter", 0o107; "/onesuperior", 0o332; "/onethird", 0o116;
  "/osuperior", 0o257; "/parenleftinferior", 0o133; "/parenleftsuperior", 0o050;
  "/parenrightinferior", 0o135; "/parenrightsuperior", 0o051; "/period", 0o056;
  "/periodinferior", 0o263; "/periodsuperior", 0o371; "/questiondownsmall",
  0o300; "/questionsmall", 0o077; "/rsuperior", 0o345; "/rupiah", 0o175;
  "/semicolon", 0o073; "/seveneighths", 0o115; "/seveninferior", 0o246;
  "/sevenoldstyle", 0o067; "/sevensuperior", 0o340; "/sixinferior", 0o244;
  "/sixoldstyle", 0o066; "/sixsuperior", 0o337; "/space", 0o040; "/ssuperior",
  0o352; "/threeeighths", 0o113; "/threeinferior", 0o243; "/threeoldstyle",
  0o063; "/threequarters", 0o111; "/threequartersemdash", 0o075;
  "/threesuperior", 0o334; "/tsuperior", 0o346; "/twodotenleader", 0o052;
  "/twoinferior", 0o252; "/twooldstyle", 0o062; "/twosuperior", 0o333;
  "/twothirds", 0o117; "/zeroinferior", 0o274; "/zerooldstyle", 0o060;
  "/zerosuperior", 0o342]

(* Symbol Encoding *)
let name_to_symbol =
  ["/Alpha", 0o101; "/Beta", 0o102; "/Chi", 0o103; "/Delta", 0o104; "/Epsilon",
  0o105; "/Eta", 0o110; "/Euro", 0o240; "/Gamma", 0o107; "/Ifraktur", 0o301;
  "/Iota", 0o111; "/Kappa", 0o113; "/Lambda", 0o114; "/Mu", 0o115; "/Nu", 0o116;
  "/Omega", 0o127; "/Omicron", 0o117; "/Phi", 0o106; "/Pi", 0o120; "/Psi",
  0o131; "/Rfraktur", 0o302; "/Rho", 0o122; "/Sigma", 0o123; "/Tau", 0o124;
  "/Theta", 0o121; "/Upsilon", 0o125; "/Upsilon1", 0o241; "/Xi", 0o130; "/Zeta",
  0o132; "/aleph", 0o300; "/alpha", 0o141; "/ampersand", 0o046; "/angle", 0o320;
  "/angleleft", 0o341; "/angleright", 0o361; "/approxequal", 0o273;
  "/arrowboth", 0o253; "/arrowdblboth", 0o333; "/arrowdbldown", 0o337;
  "/arrowdblleft", 0o334; "/arrowdblright", 0o336; "/arrowhorizex", 0o276;
  "/arrowleft", 0o254; "/arrowright", 0o256; "/arrowup", 0o255; "/arrowvertex",
  0o275; "/asteriskmath", 0o052; "/bar", 0o174; "/beta", 0o142; "/braceleft",
  0o173; "/braceright", 0o175; "/bracelefttp", 0o354; "/braceleftmid", 0o355;
  "/braceleftbt", 0o376; "/bracerighttp", 0o374; "/bracerightmid", 0o375;
  "/bracerightbt", 0o376; "/braceex", 0o357; "/bracketleft", 0o133;
  "/bracketright", 0o135; "/bracketlefttp", 0o351; "/bracketleftex", 0o352;
  "/bracketleftbt", 0o353; "/bracketrighttp", 0o371; "/brackerrightex", 0o372;
  "/bracketrightbt", 0o373; "/bullet", 0o267; "/carriagereturn", 0o277; "/chi",
  0o143; "/circlemultiply", 0o304; "/circleplus", 0o305; "/club", 0o247;
  "/colon", 0o072; "/comma", 0o054; "/congruent", 0o100; "/copyrightsans",
  0o343; "/copyrightserif", 0o323; "/degree", 0o260; "/delta", 0o144;
  "/diamond", 0o250; "/divide", 0o270; "/dotmath", 0o327; "/eight", 0o070;
  "/element", 0o316; "/ellipsis", 0o274; "/emptyset", 0o306; "/epsilon", 0o145;
  "/equal", 0o075; "/equivalence", 0o272; "/eta", 0o150; "/exclam", 0o041;
  "/existential", 0o044; "/five", 0o065; "/florin", 0o246; "/four", 0o064;
  "/fraction", 0o244; "/gamma", 0o147; "/gradient", 0o321; "/greater", 0o076;
  "/greaterequal", 0o263; "/heart", 0o251; "/infinity", 0o245; "/integral",
  0o362; "/integraltp", 0o363; "/integralex", 0o364; "/integralbt", 0o365;
  "/intersection", 0o307; "/iota", 0o151; "/kappa", 0o153; "/lambda", 0o154;
  "/less", 0o074; "/lessequal", 0o243; "/logicaland", 0o331; "/logicalnot",
  0o330; "/logicalor", 0o332; "/lozenge", 0o340; "/minus", 0o055; "/minute",
  0o242; "/mu", 0o155; "/multiply", 0o264; "/nine", 0o071; "/notelement", 0o317;
  "/notequal", 0o271; "/notsubset", 0o313; "/nu", 0o156; "/numbersign", 0o043;
  "/omega", 0o167; "/omega1", 0o166; "/omicron", 0o157; "/one", 0o061;
  "/parenleft", 0o050; "/parenright", 0o051; "/parenlefttp", 0o346;
  "/parenleftex", 0o347; "/parenleftbt", 0o350; "/parenrighttp", 0o366;
  "/parenrightex", 0o367; "/parenrightbt", 0o370; "/partialdiff", 0o266;
  "/percent", 0o045; "/period", 0o056; "/perpendicular", 0o136; "/phi", 0o146;
  "/phi1", 0o152; "/pi", 0o160; "/plus", 0o153; "/plusminus", 0o261; "/product",
  0o325; "/propersubset", 0o314; "/propersuperset", 0o311; "/proportional",
  0o265; "/psi", 0o171; "/question", 0o077; "/radical", 0o326; "/radicalex",
  0o140; "/reflexsubset", 0o315; "/reflexsuperset", 0o312; "/registersans",
  0o342; "/registerserif", 0o322; "/rho", 0o162; "/second", 0o262; "/semicolon",
  0o073; "/seven", 0o067; "/sigma", 0o163; "/sigma1", 0o126; "/similar", 0o176;
  "/six", 0o066; "/slash", 0o157; "/space", 0o040; "/spade", 0o252; "/suchthat",
  0o047; "/summation", 0o345; "/tau", 0o164; "/therefore", 0o134; "/theta",
  0o161; "/theta1", 0o112; "/three", 0o063; "/trademarksans", 0o344;
  "/trademarkserif", 0o324; "/two", 0o062; "/underscore", 0o137; "/union",
  0o310; "/universal", 0o042; "/upsilon", 0o165; "/weierstrass", 0o303; "/xi",
  0o303; "/zero", 0o060; "/zeta", 0o172]

(* 6. Dingbats encoding *)
let name_to_dingbats = 
  ["/space", 0o040; "/a1", 0o041; "/a2", 0o042; "/a202", 0o043; "/a3", 0o044;
  "/a4", 0o045; "/a5", 0o046; "/a119", 0o047; "/a118", 0o050; "/a117", 0o051;
  "/a11", 0o052; "/a12", 0o053; "/a13", 0o054; "/a14", 0o055; "/a15", 0o056;
  "/a16", 0o057; "/a105", 0o060; "/a17", 0o061; "/a18", 0o062; "/a19", 0o063;
  "/a20", 0o064; "/a21", 0o065; "/a22", 0o066; "/a23", 0o067; "/a24", 0o070;
  "/a25", 0o071; "/a26", 0o072; "/a27", 0o073; "/a28", 0o074; "/a6", 0o075;
  "/a7", 0o076; "/a8", 0o077; "/a9", 0o100; "/a10", 0o101; "/a29", 0o102;
  "/a30", 0o103; "/a31", 0o104; "/a32", 0o105; "/a33", 0o106; "/a34", 0o107;
  "/a35", 0o110; "/a36", 0o111; "/a37", 0o112; "/a38", 0o113; "/a39", 0o114;
  "/a40", 0o115; "/a41", 0o116; "/a42", 0o117; "/a43", 0o120; "/a44", 0o121;
  "/a45", 0o122; "/a46", 0o123; "/a47", 0o124; "/a48", 0o125; "/a49", 0o126;
  "/a50", 0o127; "/a51", 0o130; "/a52", 0o131; "/a53", 0o132; "/a54", 0o133;
  "/a55", 0o134; "/a56", 0o135; "/a57", 0o136; "/a58", 0o137; "/a59", 0o140;
  "/a60", 0o141; "/a61", 0o142; "/a62", 0o143; "/a63", 0o144; "/a64", 0o145;
  "/a65", 0o146; "/a66", 0o147; "/a67", 0o150; "/a68", 0o151; "/a69", 0o152;
  "/a70", 0o153; "/a71", 0o154; "/a72", 0o155; "/a73", 0o156; "/a74", 0o157;
  "/a203", 0o160; "/a75", 0o161; "/a204", 0o162; "/a76", 0o163; "/a77", 0o164;
  "/a78", 0o165; "/a79", 0o166; "/a81", 0o167; "/a82", 0o170; "/a83", 0o171;
  "/a84", 0o172; "/a97", 0o173; "/a98", 0o174; "/a99", 0o175; "/a100", 0o176;
  "/a101", 0o241; "/a102", 0o242; "/a103", 0o243; "/a104", 0o244; "/a106",
  0o245; "/a107", 0o246; "/a108", 0o247; "/a112", 0o250; "/a111", 0o251;
  "/a110", 0o252; "/a109", 0o253; "/a120", 0o254; "/a121", 0o255; "/a122",
  0o256; "/a123", 0o257; "/a124", 0o260; "/a125", 0o261; "/a126", 0o262;
  "/a127", 0o263; "/a128", 0o264; "/a129", 0o265; "/a130", 0o266; "/a131",
  0o267; "/a132", 0o270; "/a133", 0o271; "/a134", 0o272; "/a135", 0o273;
  "/a136", 0o274; "/a137", 0o275; "/a138", 0o276; "/a139", 0o277; "/a140",
  0o300; "/a141", 0o301; "/a142", 0o302; "/a143", 0o303; "/a144", 0o304;
  "/a145", 0o305; "/a146", 0o306; "/a147", 0o307; "/a148", 0o310; "/a149",
  0o311; "/a150", 0o312; "/a151", 0o313; "/a152", 0o314; "/a153", 0O315;
  "/a154", 0o316; "/a155", 0o317; "/a156", 0o320; "/a157", 0o321; "/a158",
  0o322; "/a159", 0o323; "/a160", 0o324; "/a161", 0o325; "/a163", 0o326;
  "/a164", 0o327; "/a196", 0o330; "/a165", 0o331; "/a192", 0o332; "/a166",
  0o333; "/a167", 0o334; "/a168", 0o335; "/a169", 0o336; "/a170", 0o337;
  "/a171", 0o340; "/a172", 0o341; "/a173", 0o342; "/a162", 0o343; "/a174",
  0o344; "/a175", 0o345; "/a176", 0o346; "/a177", 0o347; "/a178", 0o350;
  "/a179", 0o351; "/a193", 0o352; "/a180", 0o353; "/a199", 0o354; "/a181",
  0o355; "/a200", 0o356; "/a182", 0o357; "/a201", 0o361; "/a183", 0o362;
  "/a184", 0o363; "/a197", 0o364; "/a185", 0o365; "/a194", 0o366; "/a198",
  0o367; "/a186", 0o370; "/a195", 0o371; "/a187", 0o372; "/a188", 0o373;
  "/a189", 0o374; "/a190", 0o375; "/a191", 0o376]

(* Parse a /ToUnicode CMap to extract font mapping. *)
type section =
  | BfChar of char list
  | BfRange of char list

let rec getuntilend prev = function
  | [] -> rev prev, []
  | 'e'::'n'::'d'::'b'::'f'::'c'::'h'::'a'::'r'::more -> rev prev, more
  | h::t -> getuntilend (h::prev) t

let rec getuntilend_range prev = function
  | [] -> rev prev, []
  | 'e'::'n'::'d'::'b'::'f'::'r'::'a'::'n'::'g'::'e'::more -> rev prev, more
  | h::t -> getuntilend_range (h::prev) t

let rec get_section = function
  | [] -> None
  | 'b'::'e'::'g'::'i'::'n'::'b'::'f'::'c'::'h'::'a'::'r'::more ->
      let numbers, rest = getuntilend [] more in
        Some (BfChar numbers, rest)
  | 'b'::'e'::'g'::'i'::'n'::'b'::'f'::'r'::'a'::'n'::'g'::'e'::more ->
      let numbers, rest = getuntilend_range [] more in
        Some (BfRange numbers, rest)
  | _::t -> get_section t

(* Read a character code. *)
let rec read_number = function
  | x::more when Pdf.is_whitespace x -> read_number more
  | '<'::a::'>'::more ->
      int_of_string (implode ['0'; 'x'; a]), more
  | '<'::a::b::'>'::more ->
      int_of_string (implode ['0'; 'x'; a; b]), more
  | '<'::a::b::c::'>'::more ->
      int_of_string (implode ['0'; 'x'; a; b; c]), more
  | '<'::a::b::c::d::'>'::more ->
      int_of_string (implode ['0'; 'x'; a; b; c; d]), more
  | [] -> raise Not_found
  | _ -> raise (Pdf.PDFError "Unknown number in /ToUnicode")

(* Read the bytes of the UTF-16BE unicode sequence as a string. *)
let fail () =
  raise (Pdf.PDFError "Bad unicode value")

let rec read_unicode = function
  | x::rest when Pdf.is_whitespace x -> read_unicode rest
  | '<'::rest ->
      let chars, rest  = cleavewhile (neq '>') rest in
        let is_hex_digit = function
          | '0'..'9' | 'a'..'f' | 'A'..'F' -> true
          | _ -> false
        in
          iter
            (fun x -> if not (is_hex_digit x) then fail ())
            chars;
          if length chars > 0 && even (length chars) then
            let bytes =
              map
                (function
                  | [x; y] -> char_of_int & int_of_string & implode ['0'; 'x'; x; y]
                  | _ -> assert false)
                (splitinto 2 chars)
            in
              let rest' =
                match rest with
                | [] -> []
                | _ -> tl rest
              in
                implode bytes, rest'
          else
            fail ()
  | _ -> fail ()

let print_bytestream s =
  flprint "\n";
  for x = 0 to stream_size s - 1 do
    print_char (char_of_int s.{x})
  done;
  flprint "\n"

let rec get_sections chars =
  match get_section chars with
  | None -> []
  | Some (sec, restchars) ->
      sec::get_sections restchars

let pairs_of_section = function
  | BfChar numbers ->
      let results = ref []
      and numbers = ref numbers in
        begin try
          while true do
            let number, rest = read_number !numbers in
              let str, rest = read_unicode rest in
                numbers := rest;
                results =:: (number, str)
          done;
          []
        with
          Not_found -> rev !results
        end
  | BfRange numbers ->
      let results = ref []
      and numbers = ref numbers in
        begin try
          while true do
            let src1, rest  = read_number !numbers in
              let src2, rest  = read_number rest in
                if src1 > src2 then raise (Pdf.PDFError "Bad /ToUnicode") else
                  match rest with
                  | '<'::_ ->
                      (* It's a single unicode string *)
                      let increment_final code d =
                        match code with
                        | "" -> ""
                        | s ->
                            let s = String.copy s in
                              s.[String.length s - 1] <-
                                char_of_int (int_of_char s.[String.length s - 1] + d);
                              s
                      in
                        let code, rest = read_unicode rest in
                          results =@
                            rev
                              (combine
                                (ilist src1 src2)
                                (map (increment_final code) (ilist 0 (src2 - src1))));
                          numbers := rest
                  | '['::rest ->
                      (* It's several. *)
                      let rest = ref rest in
                        results =@
                          combine
                            (ilist src1 src2)
                            (map
                              (fun _ ->
                                 let num, rest' = read_unicode !rest in
                                   rest := rest';
                                   num)
                              (ilist 0 (src2 - src1)));
                      rest := (match !rest with [] -> [] | x -> tl x);
                      numbers := !rest
                  | _ -> raise (Pdf.PDFError "Bad BfRange")
          done;
          []
        with
          Not_found -> rev !results
        end

let rec parse_tounicode pdf tounicode =
  match tounicode with
  | Pdf.Stream {contents = (dict, Pdf.Got data)} ->
      Pdfcodec.decode_pdfstream pdf tounicode;
      begin match tounicode with
      | Pdf.Stream {contents = (dict, Pdf.Got data)} ->
          begin try
            flatten &
            map pairs_of_section &
            get_sections &
            lose Pdf.is_whitespace &
            charlist_of_bytestream data
          with
            e -> Printf.eprintf "/ToUnicode Parse Error : %s\n" (Printexc.to_string e); []
          end
      | _ -> assert false
      end
  | Pdf.Stream {contents = (_, Pdf.ToGet (_, _, _))} ->
      Pdf.getstream tounicode;
      parse_tounicode pdf tounicode
  | e -> raise (Pdf.PDFError "Bad /ToUnicode")

(* Extracting of Text *)

(* A text extractor takes a character and returns a list of unicode codepoints.
This may have to be extended when we deal with composite fonts. *)
type text_extractor =
  {convert: int -> int list;
   font: font}

(* Encode utf16be *)
let utf16be_of_codepoint u =
  if u < 0 || u > 0x10FFFF then
    raise (Invalid_argument "utf16be_of_codepoints")
  else
    if u < 0x10000 then [u] else
      let u' = u - 0x10000
      and w1 = 0xD800
      and w2 = 0xDC00 in
        let w1 = w1 lor (u' lsr 10)
        and w2 = w2 lor (u' land 0b1111111111) in
          [w1; w2]

let utf16be_of_codepoints l =
  implode & map char_of_int & flatten & map utf16be_of_codepoint l

(* Return a list of codepoints from a UTF-16BE string. See RFC2871 *)
let fail () =
  raise (Invalid_argument "codepoints_of_utf16be")

let rec codepoints_of_utf16be_inner prev = function
  | [] -> rev prev
  | [w1] -> fail ()
  | [w1a; w1b] ->
      let w1 = (w1a lsl 8) lor w1b in
        if w1 < 0xD800 || w1 > 0xDFFF then
          codepoints_of_utf16be_inner (w1::prev) []
        else
          fail ()
  | [_; _; _] -> fail ()
  | w1a::w1b::w2a::w2b::more ->
      let w1 = (w1a lsl 8) lor w1b in
        if w1 < 0xD800 || w1 > 0xDFFF then
          codepoints_of_utf16be_inner (w1::prev) (w2a::w2b::more)
        else
          if w1 >= 0xD800 && w1 <= 0xDBFF then
            let w2 = (w2a lsl 8) lor w2b in
              if w2 >= 0xDC00 && w2 <= 0xDFFF then
                let ho = w1 land 0b1111111111
                and lo = w2 lsr 6 in
                  codepoints_of_utf16be_inner
                    ((((ho lsl 10) lor lo) + 0x10000)::prev) more
              else
                fail ()
          else
            fail ()

let codepoints_of_utf16be str =
  codepoints_of_utf16be_inner [] (map int_of_char (explode str))

let glyph_hashes =
  hashtable_of_dictionary (Glyphlist.glyphmap @ Glyphlist.dingbatmap)

(* Build a hashtable for lookups based on an encoding *)
let table_of_encoding encoding =
  let table = Hashtbl.create 203
  and swp = map (fun (k, v) -> (v, k)) in
    let addvals = iter (fun (k, v) -> Hashtbl.add table k v) in
      let rec add_encoding = function
        | ImplicitInFontFile -> ()
        | StandardEncoding ->
            addvals (swp name_to_standard)
        | MacRomanEncoding ->
            addvals (swp name_to_macroman)
        | WinAnsiEncoding ->
            addvals (swp name_to_win)
        | MacExpertEncoding ->
            addvals (swp name_to_macexpert)
        | CustomEncoding (e, ds) ->
            add_encoding e;
            addvals (swp ds)
        | FillUndefinedWithStandard e ->
            addvals (swp name_to_standard);
            add_encoding e
      in
        add_encoding encoding;
        table

(* Method:
    1. If there's a /ToUnicode CMap, use it.
    2. If it is a standard 14 or simple font, use the encoding to get a glyph
    name, then look up the character in the glyph list.
    3. If it's a CID font, which we don't understand, just return.
The font here is the PDF font structure, not our font data type. If we need to
parse it, we do. *)
let text_extractor_of_font pdf font =
  match Pdf.lookup_direct pdf "/ToUnicode" font with
  | Some tounicode ->
      let convert =
        begin try
          let table =
            hashtable_of_dictionary & parse_tounicode pdf tounicode
          in
            begin function i ->
              try
                codepoints_of_utf16be (Hashtbl.find table i)
              with
              Not_found -> [i]
            end
        with
          Pdf.PDFError ("Bad /ToUnicode") -> (function i -> [i])
        end
      in
        {convert = convert; font = read_font pdf font}
  | None ->
      let convert =
        if is_simple_font pdf font || is_standard14font pdf font then
          let encoding =
            match read_font pdf font with
            | StandardFont (_, e) -> e
            | SimpleFont {encoding = e} -> e
            | _ -> assert false
          in
           let table = table_of_encoding encoding in
          begin function i ->
            try
              (*i Printf.printf "Character input: %i\n" i; i*)
              let decoded = Hashtbl.find table i in
                (*i Printf.printf "Decoded character: %s\n" decoded; i*)
                let r = Hashtbl.find glyph_hashes decoded in
                  (*i flprint "Unicode result:"; iter (Printf.printf "%i") r; flprint "\n"; i*)
                  r
            with
              Not_found -> [i]
          end
        else
          (function i -> [i])
   in
     {convert = convert; font = read_font pdf font}

(* For now, the only composite font encoding scheme we understand is /Identity-H *)
let is_identity_h = function
  | CIDKeyedFont (_, _, Predefined "/Identity-H") -> true
  | _ -> false

let codepoints_of_text extractor text =
  if text = "" then [] else
    if is_identity_h extractor.font then
      let chars = map int_of_char & explode text in
        if odd (length chars)
          then raise (Pdf.PDFError "Bad Text")
          else
            let pairs = pairs_of_list chars in
              let cs = ref [] in
              iter
                (fun (h, l) ->
                  let codepoints = extractor.convert ((h lsl 8) lor l) in
                    cs =@ rev codepoints)
                pairs;
              rev !cs
    else
      begin
        let cs = ref [] in
        for x = 0 to String.length text - 1 do
          cs =@ rev (extractor.convert (int_of_char text.[x]))
        done;
        rev !cs
      end

(* Send each byte to the text extractor, and concatenate the unicode codepoint
lists which result. *)
let utf16be_of_text extractor text =
  utf16be_of_codepoints (codepoints_of_text extractor text)

(* Convert UTF16BE to Latin1. Chars > U+0255 are dropped silently. *)
let latin1_of_utf16be str =
  implode
  & map char_of_int
  & option_map
      (fun x ->
         if x < 0 then assert false else
         if x < 256 then Some x else None)
  & codepoints_of_utf16be str

(* Lossily convert to Latin1 string *)
let latin1_string_of_text extractor text =
  latin1_of_utf16be & utf16be_of_text extractor text

(* Decode a character according to an encoding *)
let decode_char encoding chr =
  try 
    let table = table_of_encoding encoding in
      let name = Hashtbl.find table (int_of_char chr) in
        let number = Hashtbl.find glyph_hashes name in
          match number with
          | [number] ->
            if number < 0 then assert false else
            if number > 255 then chr else
            char_of_int number
          | _ -> raise Not_found
  with
    Not_found -> chr

(* Return the glyph name from a char in a type3 font. Raises [Not_found] if
not found. *)
let decode_type3_char encoding chr =
  let table = table_of_encoding encoding in
    Hashtbl.find table (int_of_char chr)

