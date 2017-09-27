(* Nexus lexer *)
{
    open NexusParser
    exception Eof
    let keyword_table = Hashtbl.create 53
    let token_table = [
        ("ALPHA", fun x -> ALPHA x);
        ("ANCSTATES", fun x -> ANCSTATES x);
        ("ASSUMPTIONS", fun x -> ASSUMPTIONS x);
        ("AVERAGE", fun x -> AVERAGE x);
        ("BEGIN", fun x -> BEGIN x);
        ("BINHEX", fun x -> BINHEX x);
        ("BOTH", fun x -> BOTH x);
        ("CHANGESET", fun x -> CHANGESET x);
        ("CHARACTER", fun x -> CHARACTER x);
        ("CHARACTERBRANCH", fun x -> CHARACTERBRANCH x);   (* used in POY block *)
        ("CHARACTERS", fun x -> CHARACTERS x);
        ("CHARLABELS", fun x -> CHARLABELS x);
        ("CHARPARTITION", fun x -> CHARPARTITION x);
        ("CHARSET", fun x -> CHARSET x);
        ("CHARSTATELABELS", fun x -> CHARSTATELABELS x);
        ("CODEORDER", fun x -> CODEORDER x);
        ("CODESET", fun x -> CODESET x);
(*        ("CODONS", fun x -> CODONS x); we can't handle the codons block *)
        ("CONDONPOSSET", fun x -> CONDONPOSSET x);
        ("CONTINUOUS", fun x -> CONTINUOUS x);
        ("COUNT", fun x -> COUNT x);
        ("CSTREE", fun x -> CSTREE x);
        ("DATA", fun x -> DATA x);
        ("DATATYPE", fun x -> DATATYPE x);
        ("DEFTYPE", fun x -> DEFTYPE x);
        ("DIAGONAL", fun x -> DIAGONAL x);
        ("DIMENSIONS", fun x -> DIMENSIONS x);
        ("DIMENSIONS", fun x -> DIMENSIONS x);
        ("DISTANCES", fun x -> DISTANCES x);
        ("DNA", fun x -> DNA x);
        ("DROS", fun x -> DROS x);
        ("ELIMINATE", fun x -> ELIMINATE x);
        ("ENCODE", fun x -> ENCODE x);
        ("END", fun x -> ENDNEXUS x);
        ("EPS", fun x -> EPS x);
        ("EQUATE", fun x -> EQUATE x);
        ("EXSET", fun x -> EXSET x);
        ("EXT", fun x -> EXT x);
        ("EXTENSIONS", fun x -> EXTENSIONS x);
        ("FILE", fun x -> FILE x);
        ("FORMAT", fun x -> FORMAT x);
        ("FREQUENCY", fun x -> FREQUENCY x);
        ("GAP", fun x -> GAP x);
        ("GAPMODE", fun x -> GAPMODE x);
        ("GAPOPENING", fun x -> GAPOPENING x);
        ("GENETICCODE", fun x -> GENETICCODE x);
        ("GIF", fun x -> GIF x);
        ("INDIVIDUALS", fun x -> INDIVIDUALS x);
        ("INLINE", fun x -> INLINE x);
        ("INTERLEAVE", fun x -> INTERLEAVE x);
        ("INTERLEAVE", fun x -> INTERLEAVE x);
        ("ITEMS", fun x -> ITEMS x);
        ("JPEG", fun x -> JPEG x);
        ("LABELS", fun x -> LABELS x);
        ("LINK", fun x -> LINK x);
        ("LIKELIHOOD", fun x -> LIKELIHOOD x);
        ("LOWER", fun x -> LOWER x); 
        ("MAM", fun x -> MAM x);
        ("MAP", fun x -> MAP x);                (* used in POY block *)
        ("MATCHCHAR", fun x -> MATCHCHAR x);
        ("MATRIX", fun x -> MATRIX x);
        ("MAX", fun x -> MAX x);
        ("MAXSTEPS", fun x -> MAXSTEPS x);
        ("MEDIAN", fun x -> MEDIAN x);
        ("MIN", fun x -> MIN x);
        ("MINSTEPS", fun x -> MINSTEPS x);
        ("MISSING", fun x -> MISSING x);
        ("MISSING", fun x -> MISSING x);
        ("MODEL", fun x -> MODEL x);
        ("MTDNA", fun x -> MTDNA x);
        ("NAMES", fun x -> NAMES x);            (* used in POY block *)
        ("NCHAR", fun x -> NCHAR x);
        ("NEWSTATE", fun x -> NEWSTATE x);
        ("NEWTAXA", fun x -> NEWTAXA x);
        ("NOLABELS", fun x -> NOLABELS x);
        ("NONE", fun x -> NONE x);
        ("NOTES", fun x -> NOTES x);
        ("NTAX", fun x -> NTAX x);
        ("NOTOKENS", fun x -> NOTOKENS x);
        ("NUCLEOTIDE", fun x -> NUCLEOTIDE x);
        ("NUCORDER", fun x -> NUCORDER x);
        ("OPTIONS", fun x -> OPTIONS x);
        ("PARAMETERS", fun x -> PARAMETERS x);
        ("PERCENT", fun x -> PERCENT x);
        ("PICT", fun x -> PICT x);
        ("PICTURE", fun x -> PICTURE x);
        ("POLYTCOUNT", fun x -> POLYTCOUNT x);
        ("POY", fun x -> POY x);                (* POY block start *)
        ("PRIORS", fun x -> PRIORS x);
        ("PROTEIN", fun x -> PROTEIN x);
        ("RESOURCE", fun x -> RESOURCE x);
        ("RESPECTCASE", fun x -> RESPECTCASE x);
        ("RNA", fun x -> RNA x);
        ("SAMPLESIZE", fun x -> SAMPLESIZE x);
        ("SETS", fun x -> SETS x);
        ("SITES", fun x -> SITES x);
        ("SOURCE", fun x -> SOURCE x);
        ("STANDARD", fun x -> STANDARD x);
        ("STATE", fun x -> STATE x);
        ("STATELABELS", fun x -> STATELABELS x);
        ("STATES", fun x -> STATES x);
        ("STATESET", fun x -> STATESET x);
        ("STATESFORMAT", fun x -> STATESFORMAT x);
        ("STATESPRESENT", fun x -> STATESPRESENT x);
        ("STDERROR", fun x -> STDERROR x);
        ("STEPMATRIX", fun x -> STEPMATRIX x);
        ("SYMBOLS", fun x -> SYMBOLS x);
        ("TAXA", fun x -> TAXA x);
        ("TAXON", fun x -> TAXON x);
        ("TAXLABELS", fun x -> TAXLABELS x);
        ("TAXPARTITION", fun x -> TAXPARTITION x);
        ("TAXSET", fun x -> TAXSET x);
        ("TCM", fun x -> TCM x);
        ("TEXT", fun x -> TEXT x);
        ("TITLE", fun x -> TITLE x);
        ("TIFF", fun x -> TIFF x);
        ("TOKENS", fun x -> TOKENS x);
        ("TRANSLATE", fun x -> TRANSLATE x);
        ("TRANSPOSE", fun x -> TRANSPOSE x);
        ("TREE", fun x -> TREE x);
        ("TREEPARTITION", fun x -> TREEPARTITION x);
        ("TREES", fun x -> TREES x);
        ("TREESET", fun x -> TREESET x);
        ("TRIANGLE", fun x -> TRIANGLE x);
        ("TYPESET", fun x -> TYPESET x);
        ("UNALIGNED", fun x -> UNALIGNED x);
        ("UNIVERSAL", fun x -> UNIVERSAL x);
        ("UPPER", fun x -> UPPER x);
        ("USERTYPE", fun x -> USERTYPE x);
        ("UTREE", fun x -> UTREE x);
        ("UUENCODE", fun x -> UUENCODE x);
        ("VARIANCE", fun x -> VARIANCE x);
        ("VARIATION", fun x -> VARIATION x);
        ("VECTOR", fun x -> VECTOR x);
        ("WTSET", fun x -> WTSET x);
        ("YEAST", fun x -> YEAST x);
    ]

    let _ =
        List.iter (fun (keyw, tok) -> 
            Hashtbl.add keyword_table keyw tok;
            Hashtbl.add keyword_table (String.lowercase keyw) tok)
        token_table
    let comment_depth = ref 0

}

rule raw = parse
     [^ ';']* as d      { DATA d }
and inquotes = parse
      [^ '"']+['"'] as d     { QUOTED d }
and insinglequotes = parse 
      ([ ^ '\'']+ as d) ['\''] { SINGLEQUOTED d }
and comment = parse
      [ ^ ']' '[']      { comment lexbuf }
    | [ '[']            { incr comment_depth; comment lexbuf }
    | [ ']' ]           { decr comment_depth; 
                        if !comment_depth = 0 then token lexbuf else
                            comment lexbuf}
and cstree = parse
      [ ^ ';']+ as id   { DATA_CSTREE id }
and token = parse
      [ ' ' '\009' '\010' '\011' '\015' '\014' '\013' '\012' ]       { token lexbuf } 
      (* skip blanks *)
    | [ '"' ]           { inquotes lexbuf }
    | [ '\'' ]           { insinglequotes lexbuf }
    | ['0'-'9']* [ 'a'-'z' 'A'-'Z' '#' '_' '|' '\128'-'\255']+ [ 'a'-'z' 'A'-'Z' '#' '_' '0'-'9' '|' '.' '\128'-'\255']* as id 
        { try
            let string = String.uppercase id in
            match string with
            | "#NEXUS" -> NEXUS
            | "PICTURE"
            | "TEXT"
            | "MATRIX" 
            | "UTREE"
            | "TREE" -> raw lexbuf
            | x -> (Hashtbl.find keyword_table string) id
        with
        | Not_found -> IDENT id }
    | [ ';' ]           { SEMICOLON }
    | [ ':' ]           { COLON }
    | [ '=' ]           { EQUAL }
    | [ ',' ]           { COMMA }
    | [ '/' ]           { BACKSLASH }
    | [ '\\' ]          { SLASH }
    | [ '-' ]           { DASH }
    | [ '(']            { LPARENT }
    | [ ')']            { RPARENT }
    | [ '*' ]           { STAR }
    | [ '[']            { incr comment_depth; comment lexbuf }
    | ['0'-'9']+ ['.'] ['0'-'9']* as i { FLOAT i }
    | ['0'-'9']+ ['.']? ['0'-'9']* ['e' 'E'] ['-' '+']? ['0'-'9']+ as i
                        { FLOAT i }
    | ['0'-'9']+ ['.'] ['0'-'9']*  as i
                        { FLOAT i }
    | ['0'-'9']+ as i   { INTEGER i }
    | [ '\'']([^ '\'']* as i) ['\''] { IDENT i }
    | [^ ' ' '\009' '\010' '\011' '\015' '\014' '\013' '\012' ] as i            { CHAR i }
    | eof               { raise Eof }
and tree_tokens = parse
     [ ' ' '\009' '\010' '\011' '\015' '\014' '\013' '\012' ] { tree_tokens lexbuf }
    | [ '*' ] { STAR }
    | [ '=' ] { EQUAL }
    | [ ',' ] { COMMA }
    | [ ':' ] { COLON }
    | [ ';' ] { SEMICOLON }
    | [ '(' ] { LPARENT }
    | [ ')' ] { RPARENT }
    | [ '\'' ] { insinglequotes lexbuf }
    | ['0'-'9']+['.']['0'-'9']* as i { FLOAT i }
    | ['0'-'9']+ as i { INTEGER i }
    | [^ '\'' '(' ')' ' ' '\009' '\010' '\011' '\015' '\014' '\013' '\012' ';' ':' ',' ]+ as i
        { IDENT i }
    | eof           { EOF }
