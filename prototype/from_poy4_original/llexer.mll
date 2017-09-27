(* Lexer for POYL *)
{
    open Lparser
    exception Eof
}

let aword = ['A'-'Z' 'a'-'z' '0'-'9' '_']
let lowercase = ['a'-'z']
let integer = ['0'-'9']
let uppercase = ['A'-'Z']

rule token = parse
    | '#'[^'\n']*                   { COMMENT }
    | [' ' '\t' '\n']               { token lexbuf }
    | "import"                      { IMPORT }
    | "let"                         { LET }
    | "character"                   { CHARACTER }
    | "alphabet"                    { ALPHABET }
    | "word"                        { WORD }
    | ';'                           { SEMI }
    | ','                           { COMMA }
    | "return"                      { RETURN }
    | "if"                          { IF }
    | "then"                        { THEN }
    | "else"                        { ELSE }
    | "for"                         { FOR }
    | "each"                        { EACH }
    | "in"                          { IN }
    | "do"                          { DO }
    | "done"                        { DONE }
    | "using"                       { USING }
    | "begin"                       { BEGIN }
    | "end"                         { END }
    | '('                           { LP }
    | ')'                           { RP }
    | "not"                         { NOT }
    | "="                           { IS }
    | '+'                           { PLUS }
    | '-'                           { MINUS }
    | '*'                           { TIMES }
    | '/'                           { DIV }
    | "mod"                         { MOD }
    | '^'                           { JOIN_STRING }
    | '@'                           { JOIN_SETS }
    | "::"                          { JOIN_LIST }
    | "&"                           { JOIN_ARRAY }
    | ':'                           { PRE }
    | "=="                          { EQ } 
    | "<>"                          { NEQ }
    | '<'                           { LT }
    | '>'                           { GT }
    | "and"                         { AND }
    | "or"                          { OR }
    | "to"                          { TO }
    | "true"                        { BOOL (true) }
    | "false"                       { BOOL (false) }
    | "prob"                        { PROB }
    | "freq"                        { FREQ }
    | "lam"                         { LAMBDA }
    | "pre"                         { PREDECCESSOR }
    | "suc"                         { SUCCESSOR }
    | "hd"                          { HEAD }
    | "tl"                          { TAIL }
    | integer+ as f                 { INTEGER (int_of_string f) }
    | integer+ '.' integer* as f    { FLOAT (float_of_string f) } 


    | lowercase aword* as f          { NAME (f) }
    | uppercase aword+ as f         { COMMAND (f) }
    | '"'([^'"']* as f)'"'          { STRING (f) }
    | '\''([^'\'']* as f)'\''       { BASE (f) }
    | eof                           { EOF }
