/* Parser for hennig files */
%{
let parse_error s = 
    try
        let b = (Parsing.symbol_start_pos ()) 
        and e = (Parsing.symbol_end_pos ()) in
        let b = string_of_int (b.Lexing.pos_cnum)
        and e = string_of_int (e.Lexing.pos_cnum) in
        Status.user_message Status.Error 
        (s ^ "@ between@ characters@ " ^ b ^ 
        "@ and@ " ^ e)
    with
    | _ -> Status.user_message Status.Error s
let report_error b e =
    let b = string_of_int (b.Lexing.pos_cnum)
    and e = string_of_int (e.Lexing.pos_cnum) in
    Status.user_message Status.Error 
    ("Unrecognized@ command@ between@ characters@ " ^ b ^ "@ and@ "
    ^ e)
%}
%token <string> DATA TREES
%token <string> TREAD
%token <string> WORD
%token <string> INT
%token <string> CHARNAME
%token <char> CHAR
%token ABSINCL
%token AGROUP
%token ALLTREES
%token ANCSTATES
%token APO
%token BBREAK
%token BEEP
%token BEST
%token BGROUND
%token BLENGTH
%token BLOCKS
%token BREAK
%token BSUPPORT
%token CCODE
%token CDIR
%token CHANGE
%token CHKMOVES
%token CHOMO
%token CKEEP
%token CLS
%token CLBUFFER
%token CNAMES
%token COLLAPSE
%token COMCOMP
%token CONDENSE
%token CONSTRAIN
%token COSTS
%token CSCORES
%token CSTREE
%token DMERGE
%token DRIFT
%token EDIT
%token ECHO
%token EXPORT
%token FIT
%token FILLSANK
%token FORCE
%token FREQDIFS
%token HELP
%token HOLD
%token HYBRID
%token IENUM
%token INCLTAX
%token INFO
%token LINTREES
%token LOG
%token KEEP
%token LENGTH
%token LQUOTE
%token MAJORITY
%token MAP
%token MINMAX
%token MIXTREES
%token MONO
%token MRP
%token MULT
%token MXRAM
%token MXPROC
%token NAKED
%token NELSEN
%token NSTATES
%token OUTGROUP
%token PROCEDURE
%token PAUSE
%token PFIJO
%token PIWE
%token PRUNCOM
%token PRUNMAJOR
%token PRUNNELSEN
%token PRUNTAX
%token PTNT
%token QCOLLAPSE
%token QNELSEN
%token QUOTE
%token QUIT
%token RANDTREES
%token RATCHET
%token RDIR
%token RECONS
%token REPORT
%token REROOT
%token RESAMPLE
%token RESOLS
%token RFREQS
%token RIDDUP
%token RSEED
%token RUN
%token SAVE
%token SCREEN
%token SCORES
%token SECTSCH
%token SHORTREAD
%token SILENT
%token SLFWT
%token SLAVEPROC
%token SMATRIX
%token SORT
%token SPRDIFF
%token SUBOPT
%token SVTXT
%token SYSTEM
%token TABLES
%token TAGSET
%token TAXCODE
%token TAXLABELS
%token TAXNAME
%token TCHOOSE
%token TCOMP
%token TEQUAL
%token TFUSE
%token TGROUP
%token THANKS
%token TIMEOUT
%token TNODES
%token TPLOT
%token TSAVE
%token TSHRINK
%token TSIZE
%token TTAGS
%token TXTSIZE
%token TZERT
%token VIEW
%token VVERSION
%token WARN
%token WATCH
%token XCOMP
%token XGROUP
%token XINACT
%token XMULT
%token XPERM
%token UNIQUE
%token UNSHARED
%token USMINMAX
%token ZZZ
%token CONTINUE
%token COPYTREE
%token CHKBREAK
%token ENDLOOP
%token ENDSWAP
%token ERRMSG
%token GOTO
%token IF
%token ITERRECS
%token LABEL
%token LOOP
%token MACFLOAT
%token MACRO
%token MACREPORT
%token MACSEED
%token MAKETABLE
%token PRIVATE
%token PROGRESS
%token RECURSE
%token RESETSWAP
%token RESETTIME
%token RETURN
%token SAFE
%token SET
%token SETARRAY
%token SETLOOP
%token SLAVEGET
%token SLAVESET
%token SPRIT
%token TBRIT
%token TRAVTREE
%token VAR
%token OPTCODE DNA PROTEINS NUMBER GAPS NOGAPS
%token LPARENT RPARENT GT EQUAL QUESTION SEMICOLON DASH LSQ RSQ PLUS STAR BACKSLASH LBRACKET RBRACKET DOT 
%type <Hennig.command> command
%start command
%type <(int * int * string)> xread
%start xread
%%

command:
    | DATA SEMICOLON { Hennig.Xread $1 }
    | CCODE character_change_list SEMICOLON { Hennig.Ccode $2 }
    | TREES SEMICOLON { Hennig.Tread $1 }
    | COSTS cost_change_list SEMICOLON { Hennig.Cost $2 }
    | PROCEDURE BACKSLASH SEMICOLON { Hennig.Ignore }
    | OPTCODE INT DOT INT SEMICOLON { Hennig.Ignore }
    | CNAMES char_names_list SEMICOLON { Hennig.Charname $2 }
    | NSTATES number_of_states SEMICOLON { Hennig.Nstates (Some $2) }
    | error SEMICOLON { 
        report_error (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ());
        Hennig.Ignore 
    }

gap:
    | GAPS       { Some `Gap }
    | NOGAPS     { Some `Nogap }
    |           { None }

number_of_states:
    | STAR          { `Number 8 }
    | DNA gap       { `Dna $2 }
    | PROTEINS gap  { `Protein $2 }
    | NUMBER INT    { `Number (int_of_string $2) }
char_names_list:
    | CHARNAME char_names_list { 
        let res = 
            (* Get rif of the closing semicolon *)
            let res = $1 in
            String.sub res 0 ((String.length res) - 1)
        in
        res :: $2 }
    | { [] }
character_change_list:
    | character_change character_change_list { $1 :: $2 }
    | { [] }
character_change:
    | PLUS character_list   { Hennig.Additive $2 }
    | DASH character_list   { Hennig.NonAdditive $2 }
    | LSQ character_list    { Hennig.Active $2 }
    | RSQ character_list    { Hennig.Inactive $2 }
    | LPARENT character_list { Hennig.Sankoff $2 }
    | RPARENT character_list { Hennig.NonAdditive $2 }
    | BACKSLASH INT character_list { Hennig.Weight (int_of_string $2, $3) }
character_list:
    | DOT { [Hennig.All] }
    | aux_character_list { $1 }
aux_character_list:
    | INT DOT INT aux_character_list { (Hennig.Range (int_of_string $1,
    int_of_string $3)) :: $4 }
    | INT aux_character_list { (Hennig.Single (int_of_string $1)) :: $2 }
    | { [] }
cost_change_list:
    | cost_change cost_change_list { $1 :: $2 }
    | { [] }
cost_change:
    | INT EQUAL chars GT chars INT 
        { (false, int_of_string $1, ($3, $5) , int_of_string $6) }
    | INT EQUAL chars BACKSLASH chars INT
        { (true, int_of_string $1, ($3, $5) , int_of_string $6) }
chars:
    | INT { $1 }
    | LSQ int_list RSQ { String.concat "" $2 }
int_list:
    | INT int_list { $1 :: $2 }
    | { [] }
xread:
    | INT INT DATA { (int_of_string $1), (int_of_string $2), $3 }
