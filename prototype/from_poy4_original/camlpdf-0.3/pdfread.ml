(* \chaptertitle{PDFRead}{Reading PDF from File} *)

(* This module can read PDF files into the format given by the [Pdf] module. It
supports PDF versions 1.0--1.6. The commentary is not in itself sufficient for
full understanding: you must read this together with the Adobe PDF Reference
Manual. Section numbers are from the Fifth Edition. *)
open Utility
open Io

(* Bring Pdf data constructors and functions up to top level. *)
open Pdf

(* \intf Errors in low-level functions, errors in lexing, errors in parsing. *)
exception PDFReadError of string
exception PDFLexError of string
exception PDFParseError of string

(* \intf Errors in the structure of the PDF (i.e not in its basic syntax.) *)
exception PDFSemanticError of string
  
(* Predicate on newline characters (carriage return and linefeed). *)
let is_newline = function
  | '\010' | '\013' -> true
  | _ -> false

let input_line i =
  let goteol = ref false
  and chars = ref [] in
    try
      until_exception ""
        (fun () ->
          let c = i.input_char () in
            if is_newline c then set goteol else
              if !goteol
                then (rewind i; raise EndOfInput)
                else chars =:: c)
    with
      EndOfInput -> implode (rev !chars)

(* Read back until a predicate is fulfilled. *)
let rec read_back_until p i =
  if (notpred p) (read_char_back i)
    then read_back_until p i

(* Go back one line. In other words, find the second EOL character group
seeking back in the file, and seek to the character after it. A blank line
after a line with a single EOL character will be treated as being part of that
EOL. *)
let backline i =
  read_back_until is_newline i;
  read_back_until (notpred is_newline) i;
  read_back_until is_newline i;
  nudge i

(* Read the major and minor version numbers  from a PDF [1.x] file. Fail if
header invalid or major version number is not 1.  *)
let rec read_header_inner pos i =
  try
    if pos > 1024 then raise EndOfInput else
      i.seek_in (i64ofi pos);
      match explode (input_line i) with
      | '%'::'P'::'D'::'F'::'-'::'1'::'.'::minor ->
          let minorchars = takewhile isdigit minor in
            if minorchars = []
              then
                raise (PDFReadError "Malformed PDF header")
              else
                begin
                  i.set_offset (i64ofi pos);
                  1, int_of_string (implode minorchars)
                end
      | _ ->
          read_header_inner (pos + 1) i
  with
    EndOfInput | Failure "int_of_string" ->
      raise (PDFReadError "Could not read PDF header")

let read_header =
  read_header_inner 0

(* Find the EOF marker, and move position to its first character. We allow 1024
bytes from end-of-file for compatibility with Acrobat. *)
let find_eof i =
  let fail () = raise (PDFReadError "Could not find EOF marker")
  and pos = ref (i.in_channel_length ()) in
    try
      let notfound = ref true
      and tries = ref 1024 in
        while !notfound do
          pos := Int64.pred !pos;
          i.seek_in !pos;
          if !tries < 0 then fail () else decr tries;
          if input_line i = "%%EOF" then clear notfound
        done;
        i.seek_in !pos;
    with
      _ -> fail ()

(* Return a list of tokens which result from the lexing of a string with a
given token set. *)
let lexwith lexer s =
  try list_of_stream & lexer & Stream.of_string s with
    Stream.Error _ -> raise (PDFError "Syntax error in PDF file.")

(* The lexer with no keywords. *)
let null_lexer =
  Genlex.make_lexer []

(* Lexemes. *)
type lexeme =
  | LexNull
  | LexBool of bool
  | LexInt of int
  | LexReal of float
  | LexString of string
  | LexName of string
  | LexLeftSquare
  | LexRightSquare
  | LexLeftDict
  | LexRightDict
  | LexStream of stream
  | LexEndStream
  | LexObj
  | LexEndObj
  | LexR
  | LexComment
  | StopLexing
  | LexNone

(* String of lexeme. *)
let string_of_lexeme = function
  | LexNull -> "null"
  | LexBool b -> Pdfwrite.string_of_pdf (Boolean b)
  | LexInt i -> Pdfwrite.string_of_pdf (Integer i)
  | LexReal f -> Pdfwrite.string_of_pdf (Real f)
  | LexString s -> Pdfwrite.string_of_pdf (String s)
  | LexName s -> s
  | LexLeftSquare -> "["
  | LexRightSquare -> "]"
  | LexLeftDict -> "<<"
  | LexRightDict -> ">>"
  | LexStream _ -> "LexStream"
  | LexEndStream -> "EndStream"
  | LexObj -> "obj"
  | LexEndObj -> "endobj"
  | LexR -> "R"
  | LexComment -> "Comment"
  | StopLexing -> "StopLexing"
  | LexNone -> "LexNone"

let print_lexeme l =
  Printf.printf "%s\n" (string_of_lexeme l)

(* Predicate on whitespace and delimiters. *)
let is_whitespace_or_delimiter c =
  is_whitespace c || is_delimiter c

(* Return the list of characters between and including the current position and
before the next character satisfying a given predicate, leaving the position at
the character following the last one returned. Can raise [EndOfInput]. If [eoi]
is true, end of input is considered a delimiter, and the characters up to it are
returned if it is reached. *)
let getuntil eoi f i =
  let rec getuntil_inner r eoi f i =
    try
      let chr = i.input_char () in
        if f chr
          then (rewind i; rev r)
          else getuntil_inner (chr::r) eoi f i
    with
      EndOfInput -> if eoi then rev r else raise EndOfInput
  in
    getuntil_inner [] eoi f i

(* The same, but don't return anything. *)
let rec ignoreuntil eoi f i =
  try
    let chr = i.input_char () in
      if f chr then rewind i else ignoreuntil eoi f i
  with
    EndOfInput -> if eoi then () else raise EndOfInput

(* Ignore until the next whitespace *)
let ignoreuntilwhite =
  ignoreuntil true is_whitespace

(* Position on the next non-whitespace character. *)
let dropwhite i =
  ignoreuntil false (notpred is_whitespace) i

(* The same, but stop at array, dictionary endings etc. *)
let getuntil_white_or_delimiter =
  getuntil true is_whitespace_or_delimiter

(* \section {Lexing} *)

(* Each of the following functions lexes a particular object, leaving the
channel position at the character after the end of the lexeme. Upon entry, the
file position is on the first character of the potential lexeme. \smallgap*)

(* Lex a bool. *)
let lex_bool i =
  match implode (getuntil_white_or_delimiter i) with
  | "true" -> LexBool true
  | "false" -> LexBool false
  | _ -> LexNone

(* Lex an int or float. See PDF manual for details of policy. *)
let lex_number i =
  let number = implode (getuntil_white_or_delimiter i) in
    try
      match hd (lexwith null_lexer number) with
      | Genlex.Int i -> LexInt i
      | Genlex.Float f -> LexReal f
      | _ -> LexNone
    with
    | Failure "hd" -> LexNone
    | PDFError _ (* can't cope with floats where number has leading point. *)
    | Failure "int_of_string" ->
        LexReal (float_of_string number) (*r [float_of_string] never fails. *)

(* Lex a name. We need to nudge past the slash and then add it manually since it
is also a delimiter. Note that this correctly lexes the name consisting of just
the slash, which is valid. *)
let rec substitute_hex prev = function
  | [] -> rev prev
  | '#'::a::b::more ->
      let chr =
        char_of_int (Scanf.sscanf (implode [a;b]) "%x" ident)
      in
        substitute_hex (chr::prev) more
  | chr::more ->
      substitute_hex (chr::prev) more

let lex_name i =
  nudge i;
  let rawchars = "/" ^ (implode (getuntil_white_or_delimiter i)) in
    let substituted = implode & substitute_hex [] & explode rawchars in
      LexName substituted

(* Lex a comment. We throw away everything from here until a new line. In the
case of a CRLF, only the CR is consumed, but the LF will be consumed before the
next token is read anyway, so this is fine. *)
let lex_comment i =
  ignoreuntil false is_newline i;
  LexComment

(* Lex a string. A string is between parenthesis. Unbalanced parenthesis in the
string must be escaped, but balanced ones need not be. We convert escaped
characters to the characters themselves. A newline sequence following a
backslash represents a newline. The string is returned without its enclosing
parameters. \smallgap *)

(* The regular expressions for everything but escaped octal characters and
escaped backslashes, compiled statically here. *)
let regexes =
  map
    (fun (a, b) -> Str.regexp_string a, b)
    [("\\n", "\n"); ("\\r", "\r"); ("\\t", "\t"); ("\\b", "\b");
    ("\\f", "\012"); ("\\(", "("); ("\\)", ")");
    ("\\\n\r", ""); ("\\\n", ""); ("\\\r", "")]

(* PDF strings can contain characters as a backslash followed by up to three
octal characters. If there are fewer than three, the next character in the file
cannot be a digit (The format is ambiguous as to whether this means an
\emph{octal} digit --- we play safe and allow non-octal digits). This replaces
these sequences of characters by a single character as used by OCaml in its
native strings.

Beware malformed strings. For instance, Reader accepts ((\\(ISA)) \smallgap *)

(* Build a character from a list of octal digits. *)
let mkchar l =
  try
    char_of_int (Scanf.sscanf (implode l) "%o" ident)
  with
    Scanf.Scan_failure f ->
      raise (PDFError ("mkchar: " ^ f))

(* Main function. *)
let lex_string i =
  let str = Buffer.create 16 in
  let addchar = Buffer.add_char str
  and paren = ref 1
  and c = i.input_char () in
  assert (c = '(');
  while !paren > 0 do
    let c = i.input_char () in
    match c with
      | '(' ->
	  incr paren; addchar c;
      | ')' ->
	  decr paren; if !paren > 0 then addchar c;
      | '\\' ->
	  let c' = i.input_char () in
	  (match c' with
	    | 'n' -> addchar '\n'
	    | 'r' -> addchar '\r'
	    | 't' -> addchar '\t'
	    | 'b' -> addchar '\b'
	    | 'f' -> addchar '\012'
	    | '\r' ->
		if i.input_char () <> '\n' then
		  rewind i
	    | '\n' -> ()
	    | '0'..'7' ->
	      (* Replace octal character sequences with the real character. *)
		let o2 = i.input_char () in
		(match o2 with
		  | '0'..'7' ->
		      let o3 = i.input_char () in
		      (match o3 with
			| '0'..'7' ->
			    addchar (mkchar [c'; o2; o3])
			| _ ->
			    rewind i;
			    addchar (mkchar [c'; o2]))
		  | _ ->
		      rewind i;
		      addchar (mkchar [c']))
            | _ -> (* including ['('], [')'], ['\\'], and all the others *)
		addchar c' )
      | _ ->
	  addchar c
  done;
  LexString (Buffer.contents str)

(* Lex a hexadecimal string. *)
let lex_hexstring i =
  let mkchar a b =
    try
      char_of_int (Scanf.sscanf (implode [a; b]) "%x" ident)
    with
      Scanf.Scan_failure f ->
         raise (PDFError ("Lexing Hexstring: " ^ f))
  in
    let _ = i.input_char () (*r skip start marker *)
    and str = Buffer.create 16
    and finished = ref false in
      let addchar = Buffer.add_char str in
        let rec input_next_char () =
          let c = i.input_char () in
            if is_whitespace c then input_next_char () else c
        in
          while not !finished do
            let c = input_next_char () in
            let c' = input_next_char () in
              match c, c' with
              | '>', _ -> rewind i; set finished
              | a, '>' -> addchar (mkchar a '0')
              | a, b -> addchar (mkchar a b)
          done;
          LexString (Buffer.contents str)

(* Lex a keyword. *)
let lex_keyword i =
  match implode (getuntil_white_or_delimiter i) with
  | "obj" -> LexObj
  | "endobj" -> LexEndObj
  | "R" -> LexR
  | "null" -> LexNull
  | "endstream" -> LexEndStream
  | _ -> LexNone

(* Lex a stream, given its length (previously extracted by parsing the stream
dictionary). If [opt] is [true] the stream is actually read, if [false] a
[ToGet] tuple is created. The channel is positioned on the first character of
the stream keyword. *)
let lex_stream_data i l opt =
  ignoreuntilwhite i;
  (* Skip either CRLF or LF. (See PDF specification for why) *)
  begin match i.input_char () with
  | '\013' ->
      begin match i.input_char () with
      | '\010' -> () (* It was CRLF *)
      | _ -> rewind i (* No padding, happens to be CR *)
      end
  | '\010' -> () (* Just LF *)
  | _ -> rewind i (* No padding. *)
  end;
  if opt then
    let arr = mkstream l in
      if l > 0 then
        for k = 0 to l - 1 do
          arr.{k} <- i.input_byte ()
        done;
      LexStream (Got arr)
  else
    (* Advance past the stream data. *)
    let pos = i.pos_in ()
    and l = i64ofi l in
      i.seek_in (i64add pos l);
      LexStream (ToGet (i, pos, l))

(* Lex a stream. This involves \emph{parsing} the stream dictionary to get the
length. [i] is at the start of the stream data, suitable for input to
[lex_stream_data]. We extract the dictionary by going through
[previous_lexemes], the reverse-order list of the lexemes already read. *)
let lex_stream i p previous_lexemes lexobj opt =
  let fail () = raise (PDFLexError "Failure lexing stream dict.") in
    let dictlexemes =
      [LexInt 0; LexInt 0; LexObj] @
      rev
        (takewhile (fun x -> x <> LexObj) previous_lexemes) @
      [LexEndObj]
    in
      match p dictlexemes with
        | _, Dictionary a ->
          let rec findlength = function
            | Integer l -> Some l
            | Indirect k -> findlength & snd & p & lexobj k
            | _ -> None
          in
            begin match lookup "/Length" a with
              | None -> fail ()
              | Some v ->
                 match findlength v with
                 | None -> fail ()
                 | Some l -> lex_stream_data i l opt
            end
        | _ -> fail ()

(* Find the next lexeme in the channel and return it. The latest-first lexeme
list [previous_lexemes] contains all things thus-far lexed. [dictlevel] is a
number representing the dictionary and/or array nesting level. If [endonstream] is true,
lexing ends upon encountering a [LexStream] lexeme. *)
let lex_next
  ?(dictlevel = ref 0) ?(endonstream = false) i previous_lexemes p opt lexobj
=
  try
    dropwhite i;
    (* To avoid problems with lexing at the end of the input, produce whitespace
    when input ends. *)
    let chr1 = i.input_char () in
      rewind i;
      match chr1 with
      | '%' -> lex_comment i
      | 't' | 'f' -> lex_bool i
      | '/' -> lex_name i
      | '0'..'9' | '+' | '-' | '.' -> lex_number i
      | '[' -> nudge i; incr dictlevel; LexLeftSquare
      | ']' -> nudge i; decr dictlevel; LexRightSquare
      | '(' -> lex_string i
      | '<' ->
        let _ = i.input_char () in
          let chr2 = i.input_char () in
            rewind i; rewind i;
            begin match chr2 with
            | '<' -> nudge i; nudge i; incr dictlevel; LexLeftDict
            | _ -> lex_hexstring i
            end 
      | '>' ->
        let _ = i.input_char () in
          let chr2 = i.input_char () in
            rewind i; rewind i;
            begin match chr2 with
            | '>' -> nudge i; nudge i; decr dictlevel; LexRightDict
            | _ -> LexNone
            end
      | 'R' -> nudge i; LexR
      | 's' ->
          (* Disambiguate ``startxref'' and ``stream'' on the third character. *)
          let _ = i.input_char () in
            let _ = i.input_char () in
              let chr3 = i.input_char () in
                rewind i; rewind i; rewind i;
                begin match chr3 with
                | 'a' -> StopLexing (*r startxref *)
                | _ -> (*r stream *)
                   if endonstream
                     then StopLexing
                     else lex_stream i p previous_lexemes lexobj opt
                end
      | 'a'..'z' -> lex_keyword i
      | 'I' -> StopLexing (*r We've hit an ID marker in an inline image *)
      | _ -> LexNone
  with
    EndOfInput -> StopLexing

(* Lex just a dictionary, consuming only the tokens to the end of it. This is
used in the [PDFPages] module to read dictionaries in graphics streams. *)
let lex_dictionary i =
  let rec lex_dictionary_getlexemes i lexemes dictlevel =
    let lex_dictionary_next i dictlevel =
      let dummyparse = fun _ -> 0, Null
      and dummylexobj = fun _ -> [] in
        lex_next ~dictlevel:dictlevel i [] dummyparse false dummylexobj
    in
      match lex_dictionary_next i dictlevel with
      | LexRightDict when !dictlevel = 0 ->
          rev (LexRightDict::lexemes)
      | StopLexing ->
          rev lexemes
      | LexNone ->
          raise (PDFReadError "Could not read dictionary")
      | a ->
          lex_dictionary_getlexemes i (a::lexemes) dictlevel
  in
    lex_dictionary_getlexemes i [] (ref 0)
 
(* Calculate a list of lexemes from input [i], using parser [p] to lex
streams. Can raise [PDFReadError]. *)
let lex_object_at ?(oneonly = false) i opt p lexobj =
  let dictlevel = ref 0 in
    let rec lex_object_at i lexemes =
      let lexeme = lex_next ~dictlevel:dictlevel i lexemes p opt lexobj in
        (*i flprint "LEXEME:"; print_lexeme lexeme; i*)
        match lexeme with
        | LexEndObj -> rev (lexeme::lexemes) 
        | StopLexing -> rev lexemes
        | LexComment -> lex_object_at i (lexeme::lexemes)
        | LexRightDict | LexRightSquare ->
            if oneonly && !dictlevel = 0
              then rev (lexeme::lexemes)
              else lex_object_at i (lexeme::lexemes)
        | LexNone ->
            Printf.eprintf "\nStopped at %Li\n" (i.pos_in ());
            raise (PDFReadError "Could not read object")
        | a -> lex_object_at i (lexeme::lexemes)
    in
      lex_object_at i []

(* Type of sanitized cross-reference entries. They are either plain offsets, or
an object stream an index into it. *)
type xref =
  | XRefPlain of int64 * int (*r offset, generation. *)
  | XRefStream of int * int (*r object number of stream, index. *)

(* [p] is the parser. Since this will be called from within functions it also
calls, we must store and retrieve the current file position on entry and exit. *)
let rec lex_object i xrefs p opt n =
  let current_pos = i.pos_in () in
     let xref =
       try Hashtbl.find xrefs n with
       | Not_found -> raise (PDFReadError "Object not in xref table")
     in
       match xref with
       | XRefStream (objstm, index) ->
           assert false (*r lex object only used on XRefPlain entries *)
       | XRefPlain (o, _) ->
           i.seek_in o;
           let result = lex_object_at i opt p (lex_object i xrefs p opt) in
             i.seek_in current_pos;
             result

(* Given an object stream pdfobject and a list of object indexes to extract,
 return an [int * lexeme list list] representing those object number, lexeme
 pairs. *)
let lex_stream_object i xrefs parse opt obj indexes user_pw partial_pdf gen =
  let _, stmobj = parse (lex_object i xrefs parse opt obj) in
    match stmobj with
    | Stream {contents = Dictionary d, stream} ->
        (* We assume that these are direct entries. *)
        let n =
          match lookup "/N" d with
          | Some (Integer n) -> n
          | _ -> raise (PDFSemanticError "missing/malformed /N")
        and first =
          match lookup "/First" d with
          | Some (Integer n) -> n
          | _ -> raise (PDFSemanticError "missing/malformed /First")
        in
          (* Decrypt if necessary *)
          let stmobj =
            Pdfcrypt.decrypt_single_stream user_pw partial_pdf obj gen stmobj
          in
          Pdfcodec.decode_pdfstream Pdf.empty stmobj;
          begin match stmobj with
          | Stream {contents = _, Got raw} ->
            let i = input_of_bytestream raw in
              begin try
                (* Read index. *)
                let rawnums = ref [] in
                  for x = 1 to n * 2 do
                    dropwhite i;
                    rawnums =::
                      match lex_number i with
                      | LexInt i -> i
                      | k -> raise (PDFSemanticError "objstm offset")
                  done;
                  rawnums := rev !rawnums;
                  (* Read each object *)
                  let pairs = pairs_of_list !rawnums
                  and objects = ref []
                  and index = ref 0 in
                    iter
                      (fun (objnum, offset) ->
                         if member !index indexes then
                           begin
                             i.seek_in (i64ofi (offset + first));
                             let lexemes =
                               lex_object_at ~oneonly:true
                                 i opt parse (lex_object i xrefs parse opt)
                             in
                               objects =:: (objnum, lexemes);
                             incr index
                           end)
                      pairs;
                    rev !objects
              with
                EndOfInput ->
                  raise (PDFSemanticError "unexpected objstream end")
              end
          | _ -> raise (PDFSemanticError "couldn't decode objstream")
          end
    | _ -> raise (PDFSemanticError "lex_stream_object: not a stream")

(* \section{Parsing} *)

(* Parsing proceeds as a series of operations over lists of lexemes or parsed
objects. Parsing ends when the list is a singleton and its element is an
well-formed object. *)
type partial_parse_element =
  | Lexeme of lexeme
  | Parsed of pdfobject

(* Parse stage one --- parse basic lexemes. *)
let parse_initial =
  map
    (function
     | Lexeme LexNull -> Parsed Null
     | Lexeme (LexBool b) -> Parsed (Boolean b)
     | Lexeme (LexInt i) -> Parsed (Integer i)
     | Lexeme (LexReal r) -> Parsed (Real r)
     | Lexeme (LexString s) -> Parsed (String s)
     | Lexeme (LexName n) ->
         Parsed (Name n)
     | l -> l)

let print_partial = function
  | Lexeme l -> print_lexeme l
  | Parsed p -> Printf.printf "PARSED: %s\n" (Pdfwrite.string_of_pdf p)

(* Parse stage two. Parse indirect references. Also remove any dummy
[LexComment] tokens. *)
let parse_R ts =
  let rec parse_R_inner r = function
    | [] -> rev r
    | Parsed (Integer o)::Parsed (Integer _)::Lexeme LexR::rest ->
        parse_R_inner (Parsed (Indirect o)::r) rest
    | Lexeme LexComment::t -> parse_R_inner r t
    | h::t -> parse_R_inner (h::r) t
  in
    parse_R_inner [] ts

(* Parse stage three. Repeatedly parse dictionaries and arrays, bottom up. This
should leave everything parsed other than the object itself. *)
let rec get_lexemes_to_symbol l s = function
  | [] -> None
  | Lexeme s'::t when s = s' -> Some (rev l, t)
  | Lexeme (LexLeftDict | LexLeftSquare)::_ -> None
  | Parsed _ as h::t -> get_lexemes_to_symbol (h::l) s t
  | Lexeme h::t ->
      raise (PDFParseError "get_lexemes_to_symbol: Bad dict or array?")

let rec replace_dictarray prev = function
  | [] -> rev prev
  | Lexeme LexLeftDict::t ->
      begin match get_lexemes_to_symbol [] LexRightDict t with
      | None -> replace_dictarray (Lexeme LexLeftDict::prev) t
      | Some (lexemes, rest) ->
          if odd (length lexemes)
            then
              ((*i iter print_partial lexemes; i*)
              raise (PDFParseError "replace_dictarray 1"))
            else
              let pairs =
                map
                  (function
                   | Parsed (Name k), Parsed v -> k, v
                   | _ -> raise (PDFParseError "replace_dictarray 2"))
                  (pairs_of_list lexemes)
            in
              replace_dictarray (Parsed (Dictionary pairs)::prev) rest
      end
  | Lexeme LexLeftSquare::t ->
      begin match get_lexemes_to_symbol [] LexRightSquare t with
      | None -> replace_dictarray (Lexeme LexLeftSquare::prev )t
      | Some (lexemes, rest) ->
          let arry =
            map
              (function
               | Parsed x -> x
               | _ -> raise (PDFParseError "replace_dictarray 3"))
              lexemes
          in
            replace_dictarray (Parsed (Array arry)::prev) rest
      end
  | h::t -> replace_dictarray (h::prev) t

(* Debug printing of parsemes. *)
let print_parseme = function
  | Parsed p -> print_string (Pdfwrite.string_of_pdf p)
  | Lexeme l -> print_lexeme l

(* Call [replace_dictarray] repeatedly until no arrays or dictionaries to do,
then extract the object. Possible correct forms: (1)~Normal object (2)~Stream
object (3)~Trailer dictionary. This can be non-terminating on bad input, so
bail out after 5000 recursions. *)
let rec parse_reduce recs l =
  if recs = 5000 then raise (PDFReadError "Parse error") else
    let rec parse_finished = function
      | [] -> true
      | Lexeme (LexLeftSquare | LexLeftDict)::_ -> false
      | _::t -> parse_finished t
    in
      if parse_finished l then
        match l with
        | [Parsed (Integer o); Parsed (Integer g);
          Lexeme LexObj; Parsed obj; Lexeme LexEndObj] ->
            o, obj
        | [Parsed (Integer o); Parsed (Integer g);
          Lexeme LexObj; Parsed obj; Lexeme (LexStream s);
          Lexeme LexEndStream; Lexeme LexEndObj] ->
            o, Stream {contents = obj, s}
        | [Parsed d] ->
            0, d
        | l ->
            (*i flprint "PARSEMES:\n";
            iter print_parseme l;
            flprint "END OF PARSEMES\n"; i*)
            raise (PDFReadError "Could not extract object")
      else
        parse_reduce (recs + 1) (replace_dictarray [] l)

(* Parse some lexemes *)
let parse lexemes =
  parse_reduce 0 &
  parse_R &
  parse_initial (map (fun x -> Lexeme x) lexemes)

let parse ?objnum' lexemes =
  match objnum' with
  | None -> parse lexemes
  | Some o ->
      match parse lexemes with
        (_, obj) -> (o, obj) 

(* Advance to the first thing after the current pointer which is not a comment. *)
let rec ignore_comments i =
  let pos = i.pos_in () in
    if i.input_char () = '%'
      then (ignore (input_line i); ignore_comments i)
      else i.seek_in pos

(* \section{Cross-reference tables} *)

(* Read the cross-reference table. Supports the multiple sections created when
a PDF file is incrementally modified. *)
type xref_line =
  | Invalid
  | Section of int * int (*r Start, length. *)
  | Valid of int64 * int (*r byte offset, gen. *)
  | Free of int64 * int (*r free entry. *)
  | InObjectStream of int * int (*r Stream number, index. *)
  | StreamFree of int64 * int (*r free entry in an object stream. *)
  | XRefNull (*r is the null object. *)
  | Finished (*r end of a table. *)

(* Read and parse a single line of a cross-reference table. We use a
long-winded match pattern on the characters of cross-reference lines because a
byte offset can exceed the range for [Genlex.Int]. *)
let rec read_xref_line i =
  let pos = i.pos_in () in
    let line = input_line i in
      if line = "xref" then read_xref_line i else
        match explode line with
        | 't'::'r'::'a'::'i'::'l'::'e'::'r'::more ->
            (* Bad files may not put newlines after the trailer, so [input_line] may
            have taken too much, preventing us from reading the trailer
            dictionary, so we rewind. *)
            i.seek_in (i64add pos 7L);
            Finished
        | ('0'..'9' as a)::('0'..'9' as b)::('0'..'9' as c)::
          ('0'..'9' as d)::('0'..'9' as e)::('0'..'9' as f)::
          ('0'..'9' as g)::('0'..'9' as h)::('0'..'9' as i)::
          ('0'..'9' as j)::' '::('0'..'9' as k)::('0'..'9' as l)::
          ('0'..'9' as m)::('0'..'9' as n)::('0'..'9' as o)::' '::r ->
           let p, i =
             Int64.of_string (implode [a; b; c; d; e; f; g; h; i; j]),
             int_of_string (implode [k; l; m; n; o])
           in
             begin
               match r with
               | 'n'::_ -> Valid (p, i)
               | 'f'::_ -> Free (p, i)
               | _ -> Invalid
             end
        | _ ->
          (* Artworks produces bad PDF with lines like \texttt{xref 1 5} *)
          match lexwith (Genlex.make_lexer ["xref"]) line with
          | [Genlex.Kwd "xref"; Genlex.Int s; Genlex.Int l]
          | [Genlex.Int s; Genlex.Int l] -> Section (s, l)
          | _ -> Invalid 

(* Read the cross-reference table in [i] at the current position. Leaves [i] at
the first character of the trailer dictionary. *)
let read_xref i =
  let fail () = raise (PDFReadError "Could not read x-ref table")
  and xrefs = ref [] in
    begin try
      let finished = ref false
      and objnumber = ref 1 in
        while not !finished do
          match read_xref_line i with
          | Invalid -> fail ()
          | Valid (offset, gen) ->
              xrefs =:: (!objnumber, XRefPlain (offset, gen));
              incr objnumber
          | Finished -> set finished
          | Section (s, _) -> objnumber := s
          | Free _ -> incr objnumber
          | _ -> () (* Xref stream types won't have been generated. *)
        done
      with
        EndOfInput | Sys_error _ | Failure "int_of_string"-> fail ()
    end;
    !xrefs

(* PDF 1.5 cross-reference stream support. [i] is the input. The tuple describes
the lengths in bytes of each of the three fields. *)
let read_xref_line_stream i (w1, w2, w3) =
  assert (w1 >= 0 && w2 >= 0 && w3 >= 0);
  let rec mknum mul = function
    | [] -> 0L
    | h::t -> i64add (i64mul (i64ofi h) mul) (mknum (i64mul mul 256L) t)
  in
    let rec read_field bytes = function
      | 0 -> mknum 1L bytes (* Lower order byte first. *)
      | n -> read_field (i.input_byte ()::bytes) (n - 1)
    in
      let f1 = read_field [] w1 in
        let f2 = read_field [] w2 in
          let f3 = read_field [] w3 in
            match f1 with
            | 0L -> StreamFree (f2, i64toi f3)
            | 1L -> Valid (f2, i64toi f3)
            | 2L -> InObjectStream (i64toi f2, i64toi f3)
            | n -> XRefNull

(* The function to read a whole cross-reference stream, and return an [xref
list]. Leaves [i] at the first character of the stream dictionary, which
containes the trailer dictionary entries. *)
let read_xref_stream i =
  let original_pos = i.pos_in ()
  and err = PDFReadError "Bad xref stream" in
    let rec lex_untilstream i ls =
      let lexobj = lex_object i (null_hash ()) parse false in
        match lex_next ~endonstream:true i [] parse false lexobj with
        | StopLexing -> rev ls
        | l -> lex_untilstream i (l::ls)
    in
      let stream, obj, gen =
        match
          let lexobj = lex_object i (null_hash ()) parse true in
            let dictlex = lex_untilstream i [] in
              let obj =
                match hd dictlex with
                | LexInt i -> i
                | _ -> raise Not_found
              and gen =
                match (hd (tl dictlex)) with
                | LexInt i -> i
                | _ -> raise Not_found
              in
                match lex_stream i parse (rev dictlex) lexobj true with
                | LexNone -> raise err
                | stream ->
                    snd (parse (dictlex @ [stream] @ [LexEndStream; LexEndObj])),
                    obj,
                    gen
        with
        | Stream _ as stream, obj, gen -> stream, obj, gen
        | _ -> raise err
      in
        Pdfcodec.decode_pdfstream Pdf.empty stream;
        let ws =
          match lookup_direct Pdf.empty "/W" stream with
          | Some (Array [Integer w1; Integer w2; Integer w3]) -> w1, w2, w3
          | _ -> raise err
        and i' =
          match stream with
          | Stream {contents = _, Got s} -> input_of_bytestream s
          | _ -> raise err
        and xrefs = ref [] in
          begin try
            while true do
              xrefs =:: read_xref_line_stream i' ws
            done
          with
            EndOfInput -> ()
          end;
          xrefs := rev !xrefs;
          let starts_and_lens =
            match lookup_direct Pdf.empty "/Index" stream with
            | Some (Array elts) ->
                if odd (length elts) then raise (PDFReadError "Bad /Index");
                map
                  (function
                    | (Pdf.Integer s, Pdf.Integer l) -> s, l
                    | _ -> raise (PDFReadError "Bad /Index entry"))
                  (pairs_of_list elts)
            | Some _ -> raise (PDFSemanticError "Unknown /Index")
            | None ->
                let size =
                  match lookup_direct Pdf.empty "/Size" stream with
                  | Some (Integer s) -> s
                  | _ ->
                      raise (PDFSemanticError "Missing /Size in xref dict")
                in
                  [0, size]
          in
            let xrefs' = ref [] in
            iter
              (fun (start, len) ->
                let these_xrefs =
                  try take !xrefs len with
                    _ -> raise (PDFReadError "Bad xref stream\n")
                in
                  xrefs := drop !xrefs len;
                  let objnumber = ref start in
                    iter
                      (function
                       | Valid (offset, gen) ->
                           xrefs' =:: (!objnumber, XRefPlain (offset, gen));
                           incr objnumber
                       | InObjectStream (stream, index) ->
                           xrefs' =:: (!objnumber, XRefStream (stream, index));
                           incr objnumber
                       | _ -> incr objnumber)
                      these_xrefs)
                starts_and_lens;
              i.seek_in original_pos;
              rev !xrefs'

(* A suitable function for the Pdf module to use to lex and parse an object.
Assumes [i] has been set to the correct position. [n] is the object number. *)
let get_object i xrefs n =
  let lexemes = lex_object i xrefs parse false n in
    snd (parse ~objnum':n lexemes)

(* \section{Main functions} *)

(* Read a PDF from a channel. If [opt], streams are read immediately into
memory. *)
let read_pdf user_pw opt i =
  let xrefs = Hashtbl.create 1001 in
  let major, minor = read_header i
  and objects, root, trailerdict =
    let addref (n, x) =
      try ignore (Hashtbl.find xrefs n) with
        Not_found -> Hashtbl.add xrefs n x
    and got_all_xref_sections = ref false
    and trailerdict = ref []
    and xref = ref 0L
    and first = ref true in
      (* This function builds a partial pdf of the plain objects whose
      references have currently been seen. *)
      let mkpartial trailerdict =
        let objpairs = ref [] in
          (* 1. Build object number, offset pairs *)
          Hashtbl.iter
            (fun n x ->
               match x with
               | XRefPlain (offset, gen) -> objpairs =:: (n, (ref ToParse, gen))
               | _ -> ())
            xrefs;
            (* 2. Build the object map *)
            let objects =
              Pdf.objects_of_list (Some (get_object i xrefs)) !objpairs
            in
              (* 3. Build the Pdf putting the trailerdict in *)
              {Pdf.empty with
                 Pdf.objects = objects;
                 Pdf.trailerdict = trailerdict}
      in
      (* Move to the first xref section. *)
      find_eof i;
      backline i;
      dropwhite i;
      begin match takewhile isdigit (getuntil_white_or_delimiter i) with
      | [] -> raise (PDFReadError "Could not find xref pointer")
      | xrefchars -> xref := Int64.of_string (implode xrefchars);
      end;
      while not !got_all_xref_sections do
        i.seek_in !xref;
        (* Distinguish between xref table and xref stream. *)
        dropwhite i;
        let f_read_xref =
          if peek_char i = 'x'
            then read_xref
            else read_xref_stream
        in
          (* Read cross-reference table *)
          iter addref (f_read_xref i);
          (* It is now assumed that [i] is at the start of the trailer dictionary. *)
          let trailerdict_current =
            let lexemes =
              lex_object_at ~oneonly:true i opt parse (lex_object i xrefs parse opt)
            in
            match parse lexemes with
              | (_, Dictionary d)
              | (_, Stream {contents = Dictionary d, _}) -> d
              | _ -> raise (PDFReadError "Malformed trailer")
          in
            begin
              if !first then
                begin
                  trailerdict := mergedict trailerdict_current !trailerdict;
                  clear first
                end;
              (* Do we have a /XRefStm to follow? *)
              begin match lookup "/XRefStm" trailerdict_current with
              | Some (Integer n) ->
                  i.seek_in (i64ofi n);
                  iter addref (read_xref_stream i)
              | _ -> ()
              end;
              (* Is there another to do? *)
              match lookup "/Prev" trailerdict_current with
              | None -> set got_all_xref_sections
              | Some (Integer n) -> xref := i64ofi n
              | _ -> raise (PDFReadError "Malformed trailer")
            end
      done;
      let root =
        match lookup "/Root" !trailerdict with
        | Some (Indirect i) -> i
        | None -> raise (PDFReadError "No /Root entry")
        | _ -> raise (PDFReadError "Malformed /Root entry")
      in
        let getgen n =
          match Hashtbl.find xrefs n with
          | XRefPlain (_, g) -> g
          | XRefStream _ -> 0
        in
        let objects_nonstream =
          let objnumbers = ref [] in
            Hashtbl.iter
              (fun n x ->
                 match x with
                 | XRefPlain (offset, gen) -> objnumbers =:: n
                 | _ -> ())
              xrefs;
              map
                (if opt then
                   fun o ->
                     let num, parsed =
                       parse (lex_object i xrefs parse opt o)
                     in
                       num, (ref (Pdf.Parsed parsed), getgen o)
                   else
                     fun o -> o, (ref Pdf.ToParse, getgen o))
                !objnumbers
         and objects_stream =
           let streamones =
             map
               (function
                  | (n, XRefStream (s, i)) -> (n, s, i)
                  | _ -> assert false)
               (keep
                 (function (n, XRefStream _) -> true | _ -> false)
                 (list_of_hashtbl xrefs))
           in

             let cmp_objs (_, s, _) (_, s', _) = compare s s' in
               let sorted = List.sort cmp_objs streamones in
                 let collated = collate cmp_objs sorted in
                   let inputs_to_lex_stream_object =
                     map
                       (fun l ->
                         match hd l with (_, s, _) ->
                           s, map (fun (_, _, i) -> i) l)
                       collated
                   in
                     let outputs_from_lex_stream_object =
                       map
                         (function (s, is) ->
                            lex_stream_object
                              i xrefs parse opt s is user_pw
                              (mkpartial (Pdf.Dictionary !trailerdict)) (getgen s))
                         inputs_to_lex_stream_object
                     in
                       let object_lexemes_and_numbers =
                         flatten outputs_from_lex_stream_object
                       in
                         map
                           (fun (objnum, lexemes) ->
                              objnum,
                              (* Generation number of object in stream is always zero. *)
                              (ref (Pdf.Parsed (snd (parse ~objnum':objnum lexemes))), 0))
                           object_lexemes_and_numbers
         in
          objects_stream @ objects_nonstream, root, trailerdict
    in
      (* Fix Size entry and remove Prev and XRefStm *)
      let trailerdict' =
        Dictionary
          (add "/Size" (Integer (length objects))
            (remove "/Prev" (remove "/XRefStm" !trailerdict)))
      in
        {major = major;
         minor = minor;
         objects = Pdf.objects_of_list (Some (get_object i xrefs)) objects;
         root = root;
         trailerdict = trailerdict'}

(* \intf Read a PDF into memory, including its streams. *)
let pdf_of_channel ?(upw = "") ch =
  read_pdf upw true (input_of_channel ch) 

(* \intf Same, but delay reading of streams. *)
let pdf_of_channel_lazy ?(upw = "") ch =
  read_pdf upw false (input_of_channel ch)

(* \intf Similarly for inputs. *)
let pdf_of_input ?(upw = "") i =
  read_pdf upw true i

(* \intf And lazy on inputs. *)
let pdf_of_input_lazy ?(upw = "") i =
  read_pdf upw false i

(* \intf Read a whole PDF file into memory. Closes file. *)
let pdf_of_file ?(upw = "") f =
  try 
    let fh = open_in_bin f in
      let pdf = pdf_of_channel ~upw fh in
        close_in fh;
        pdf
  with
    | (PDFError _ | PDFSemanticError _ | PDFReadError _) as e -> raise e
    | Sys_error str -> raise (PDFError str)
    (*i | e -> raise (PDFError ("Could not load file (" ^ Printexc.to_string e ^
     * ")")) i*)

