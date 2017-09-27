/* Parser for nexus files */
%{
let mesquite_error =
    ("This@ seems@ to@ be@ a@ Mesquite@ \"NEXUS\"@ file.@ Unfortunately@ " ^
    "Mesquite@ has@ invented@ new@ commands@ (like@ TITLE)@ that@ are@ not@ a@ " ^
    "valid@ in@ NEXUS@ files.@ You@ will@ have@ to@ clean@ it@ by@ hand,@ " ^
    "but@ better@ read@ their@ information@ about@ it@ here:@ http://mesquiteproject.org/mesquite_folder/docs/mesquite/otherPrograms.html")
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
    | _ -> ()

let report_error text b e =
    let b = string_of_int (b.Lexing.pos_cnum)
    and e = string_of_int (e.Lexing.pos_cnum) in
    Status.user_message Status.Error 
    ("There@ was@ a@ parsing@ error@ between@ characters@ " ^ b ^ "@ and@ "
    ^ e ^ " in the " ^ text)
%}
%token EOF
%token <string> ALPHA
%token <string> ANCSTATES
%token <string> ASSUMPTIONS
%token <string> AVERAGE
%token <string> BEGIN
%token <string> BINHEX
%token <string> BOTH
%token <string> CHANGESET
%token <char> CHAR
%token <string> CHARACTER
%token <string> CHARACTERBRANCH
%token <string> CHARACTERS
%token <string> CHARLABELS
%token <string> CHARPARTITION
%token <string> CHARSET
%token <string> CHARSTATELABELS
%token <string> CODEORDER
%token <string> CODESET
%token <string> CODONS
%token <string> CONDONPOSSET
%token <string> CONTINUOUS
%token <string> COUNT
%token <string> CSTREE
%token <string> DATA
%token <string> DATA_CSTREE
%token <string> QUOTED
%token <string> SINGLEQUOTED
%token <string> DATATYPE
%token <string> DEFTYPE
%token <string> DIAGONAL
%token <string> DIMENSIONS
%token <string> DISTANCES
%token <string> DNA
%token <string> DROS
%token <string> ELIMINATE
%token <string> ENCODE
%token <string> ENDNEXUS
%token <string> EPS
%token <string> EQUATE
%token <string> EXSET
%token <string> EXT
%token <string> EXTENSIONS
%token <string> FILE
%token <string> FORMAT
%token <string> FREQUENCY
%token <string> GAP
%token <string> GAPMODE
%token <string> GAPOPENING
%token <string> GENETICCODE
%token <string> GIF
%token <string> INDIVIDUALS
%token <string> INLINE
%token <string> INTERLEAVE
%token <string> ITEMS
%token <string> JPEG
%token <string> LABELS
%token <string> LINK
%token <string> LIKELIHOOD
%token <string> LOWER
%token <string> MAM
%token <string> MAP
%token <string> MATCHCHAR
%token <string> MATRIX
%token <string> MAX
%token <string> MAXSTEPS
%token <string> MEDIAN
%token <string> MIN
%token <string> MINSTEPS
%token <string> MISSING
%token <string> MODEL
%token <string> MTDNA
%token <string> NAMES
%token <string> NCHAR
%token <string> NEWSTATE
%token <string> NEWTAXA
%token <string> NOLABELS
%token <string> NONE
%token <string> NOTES
%token <string> NTAX
%token <string> NOTOKENS
%token <string> NUCLEOTIDE
%token <string> NUCORDER
%token <string> OPTIONS
%token <string> PARAMETERS
%token <string> PERCENT
%token <string> PICT
%token <string> PICTURE
%token <string> POLYTCOUNT
%token <string> POY
%token <string> PRIORS
%token <string> PROTEIN
%token <string> RESOURCE
%token <string> RESPECTCASE
%token <string> RNA
%token <string> SAMPLESIZE
%token <string> SETS
%token <string> SITES
%token <string> SOURCE
%token <string> STANDARD
%token <string> STATE
%token <string> STATELABELS
%token <string> STATES
%token <string> STATESET
%token <string> STATESFORMAT
%token <string> STATESPRESENT
%token <string> STDERROR
%token <string> STEPMATRIX
%token <string> SYMBOLS
%token <string> TAXON
%token <string> TAXA
%token <string> TAXLABELS
%token <string> TAXPARTITION
%token <string> TAXSET
%token <string> TCM
%token <string> TEXT
%token <string> TIFF
%token <string> TITLE
%token <string> TOKENS
%token <string> TRANSLATE
%token <string> TRANSPOSE
%token <string> TREE
%token <string> TREEPARTITION
%token <string> TREES
%token <string> TREESET
%token <string> TRIANGLE
%token <string> TYPESET
%token <string> UNALIGNED
%token <string> UNIVERSAL
%token <string> UPPER
%token <string> USERTYPE
%token <string> UTREE
%token <string> UUENCODE
%token <string> VARIANCE
%token <string> VARIATION
%token <string> VECTOR
%token <string> WTSET
%token <string> YEAST
%token <string> EIDENT
%token NEXUS SEMICOLON EQUAL COMMA QUOTE BACKSLASH DASH LPARENT RPARENT STAR
COLON SLASH
%token <string> IDENT
%token <string> FLOAT
%token <string> INTEGER
%start tree
%type <Nexus.tree> tree
%start header
%type <unit> header
%start block
%type <Nexus.block> block
%start symbol_pair
%type <(string * string list)> symbol_pair
%start symbol_list
%type <string list> symbol_list
%%

header:
    | NEXUS { () }
    ;
block:
    | BEGIN TAXA SEMICOLON mesquite_broken taxa ENDNEXUS SEMICOLON  
        { Nexus.Taxa $5 }
    | BEGIN CHARACTERS SEMICOLON mesquite_broken characters ENDNEXUS SEMICOLON 
        { Nexus.Characters $5 }
    | BEGIN DATA SEMICOLON mesquite_broken characters ENDNEXUS SEMICOLON
        { Nexus.Characters $5 }
    | BEGIN UNALIGNED SEMICOLON mesquite_broken unaligned ENDNEXUS SEMICOLON
        { Nexus.Unaligned $5 }
    | BEGIN TREES SEMICOLON mesquite_broken trees ENDNEXUS SEMICOLON
        { Nexus.Trees $5 }
    | BEGIN NOTES SEMICOLON mesquite_broken notes ENDNEXUS SEMICOLON 
        { Nexus.Notes $5 }
    | BEGIN DISTANCES SEMICOLON mesquite_broken distances ENDNEXUS SEMICOLON 
        { Nexus.Distances $5 }
    | BEGIN ASSUMPTIONS SEMICOLON mesquite_broken assumptions ENDNEXUS SEMICOLON
        { Nexus.Assumptions $5 }
    | BEGIN SETS SEMICOLON mesquite_broken sets_block ENDNEXUS SEMICOLON
        { Nexus.Sets $5 }
    | BEGIN IDENT error ENDNEXUS SEMICOLON
        { Nexus.UnknownBlock $2 }
    | BEGIN TAXA SEMICOLON error ENDNEXUS SEMICOLON  
        { Nexus.Error $2 }
    | BEGIN UNALIGNED SEMICOLON error ENDNEXUS SEMICOLON
        { Nexus.Error $2 }
    | BEGIN NOTES SEMICOLON error ENDNEXUS SEMICOLON 
        { Nexus.Error $2 }
    | BEGIN DISTANCES SEMICOLON error ENDNEXUS SEMICOLON 
        { Nexus.Error $2 }
    | BEGIN SETS SEMICOLON error ENDNEXUS SEMICOLON
        { Nexus.Error $2 }
    | BEGIN POY SEMICOLON poy_block ENDNEXUS SEMICOLON
        { Nexus.Poy $4 }
    ;
mesquite_broken:
    | mesquite_broken_items mesquite_broken { $1 :: $2 }
    | { [] }
    ;
mesquite_broken_items:
    | TITLE any_thing_minus_end  SEMICOLON { () }
    | LINK any_thing_minus_end EQUAL any_thing_minus_end SEMICOLON { () }
    ;
set_in_block:
    | CHARSET nexus_word optional_lparent optional_standard_or_vector
    optional_rparent EQUAL characterset_list { ($2, Nexus.CharacterSet $7) }
    | STATESET nexus_word optional_lparent optional_standard_or_vector optional_rparent EQUAL characterset_list 
        { ($2, Nexus.StateSet $7) }
    | TAXSET nexus_word optional_lparent optional_standard_or_vector optional_rparent EQUAL characterset_list 
        { ($2, Nexus.TaxonSet $7) } 
    | TREESET nexus_word optional_lparent optional_standard_or_vector optional_rparent EQUAL characterset_list 
        { ($2, Nexus.TreeSet $7) }
    | CHARPARTITION nexus_word partition_contents  { $2, Nexus.CharPartition $3 }
    | TAXPARTITION nexus_word partition_contents { $2, Nexus.TaxPartition $3 }
    | TREEPARTITION nexus_word partition_contents { $2, Nexus.TreePartition $3 }
    ;
optional_lparent:
    | LPARENT  { () }
    | { () }
    ;
optional_rparent:
    | RPARENT { () }
    | { () }
    ;
partition_contents:
    | do_token EQUAL standard_type_set { Nexus.Standard $3 }
    | do_token STANDARD EQUAL standard_type_set { Nexus.Standard $4 }
    | do_token VECTOR EQUAL vector_type_set { Nexus.Vector $4 }
    ;
sets_block:
    | set_in_block SEMICOLON sets_block { $1 :: $3 }
    | set_in_block SEMICOLON { [$1] }
    ;
optional_standard_or_vector:
    | STANDARD { Some $1 }
    | VECTOR   { Some $1 }
    | { None }
    ;
assumptions:
    | assumption_items assumptions { $1 :: $2 }
    | { [] }
    ;
assumption_items:
    | optional_assumption_options   { Nexus.Options $1 }
    | optional_user_type            { Nexus.UserType $1 }
    | optional_type_set             { Nexus.TypeDef $1 }
    | optional_wtset                { Nexus.WeightDef $1 }
    | optional_exset                { Nexus.ExcludeSet $1 }
    | optional_ancstates            { Nexus.AncestralDef $1 }
    | error                         
    { report_error "Assumption Block" (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ());
        raise Parsing.Parse_error }
    ;
optional_assumption_options:
    | OPTIONS deftype polytcount gapmode SEMICOLON { ($2, $3, $4) }
    ;
deftype:
    | DEFTYPE EQUAL IDENT { Some $3 }
    | { None }
    ;
polytcount:
    | POLYTCOUNT EQUAL MINSTEPS { Nexus.MinSteps }
    | POLYTCOUNT EQUAL MAXSTEPS { Nexus.MaxSteps }
    | { Nexus.MinSteps }
    ;
gapmode:
    | GAPMODE EQUAL MISSING { Nexus.Missing }
    | GAPMODE EQUAL NEWSTATE { Nexus.NewState }
    | { Nexus.NewState }
    ;
optional_user_type:
    | USERTYPE IDENT user_type_definition SEMICOLON { ($2, $3) }
    ;
user_type_definition:
    | EQUAL INTEGER numbers_and_chars { Nexus.StepMatrix ($2, $3) }
    | STEPMATRIX EQUAL INTEGER numbers_and_chars 
        { Nexus.StepMatrix ($3, $4) }
    | CSTREE DATA_CSTREE { Nexus.CSTree $2 }
    ;
numbers_and_chars:
    | number_and_char numbers_and_chars { $1 :: $2 }
    | number_and_char { [$1] }
    ;
number_and_char:
    | INTEGER   { $1 }
    | FLOAT     { $1 }
    | CHAR      { Char.escaped $1 }
    | COLON     { ":" }
    | EQUAL     { "=" }
    | COMMA     { "," }
    | BACKSLASH { "/" }
    | DASH      { "-" }
    | STAR      { "*" }
    | IDENT     { $1 }
    ;
optional_type_set:
    | TYPESET optional_set_for_assumptions { $2 }
    ;
optional_wtset:
    | WTSET optional_set_for_assumptions { $2 }
    ;
do_token:
    | NOTOKENS { false }
    | TOKENS { true }
    | { false }
do_star:
    | STAR { true }
    | { false }
optional_set_for_assumptions:
    | do_star IDENT do_token EQUAL standard_type_set SEMICOLON 
        { ($1, $2, $3, Nexus.Standard $5) }
    | do_star IDENT STANDARD do_token EQUAL standard_type_set SEMICOLON 
        { ($1, $2, $4, Nexus.Standard $6) }
    | do_star IDENT VECTOR do_token EQUAL vector_type_set SEMICOLON 
        { ($1, $2, $4, Nexus.Vector $6) }
    ;
standard_type_set_item:
    | INTEGER COLON characterset_list { Nexus.Code ($1, $3) }
    | FLOAT COLON characterset_list   { Nexus.Code ($1, $3) }
    | IDENT COLON characterset_list   { Nexus.IName ($1, $3) }
    ;
standard_type_set:
    | standard_type_set_item COMMA standard_type_set { ($1 :: $3) }
    | standard_type_set_item { [$1] }
    | { [] }
    ;
vector_type_set:
    | INTEGER vector_type_set { ($1 :: $2) }
    | { [] }
    ;
optional_exset:
    | EXSET do_star nexus_word 
        EQUAL characterset_list SEMICOLON { ($2, $3, Nexus.STDStandard $5) }
    | EXSET do_star nexus_word STANDARD 
        EQUAL characterset_list SEMICOLON { ($2, $3, Nexus.STDStandard $6) }
    | EXSET do_star nexus_word VECTOR 
        EQUAL vector_type_set SEMICOLON { ($2, $3, Nexus.STDVector $6) }
    ;
optional_ancstates:
    | ANCSTATES optional_set_for_assumptions { $2 }
    ;
distances:
    | optional_distances_dimensions optional_format optional_taxlabels DATA SEMICOLON
        { ($1, $2, $3, $4) }
    ;
optional_distances_dimensions:
    | DIMENSIONS NEWTAXA NTAX EQUAL INTEGER NCHAR EQUAL INTEGER SEMICOLON
        { Some (true, $5, $8) }
    | DIMENSIONS NTAX EQUAL INTEGER NCHAR EQUAL INTEGER SEMICOLON
        { Some (false, $4, $7) }
    | { None }
    ;
notes:
    | optional_text optional_picture 
        { $1, $2 }
    ;
optional_text:
    | TEXT optional_set_pair_list SOURCE EQUAL source DATA SEMICOLON
        { Some ($2, $5, $6) }
    | { None }
    ;
optional_picture:
    | PICTURE optional_set_pair_list optional_pictureformat optional_encode SOURCE EQUAL source 
    DATA SEMICOLON { Some ($2, $3, $4, $7, $8) }
    | { None } 
    ;
optional_set_pair_list:
    | set_pair optional_set_pair_list { $1 :: $2 }
    | { [] }
    ;
set_pair:
    | TAXON EQUAL characterset_list     { Nexus.TaxonSet $3 }
    | CHARACTER EQUAL characterset_list { Nexus.CharacterSet $3 }
    | STATE EQUAL characterset_list      { Nexus.StateSet $3 }
    | TREE EQUAL characterset_list        { Nexus.TreeSet $3 }
    ;
source:
    | INLINE    {Nexus.Inline }
    | FILE      { Nexus.File }
    | RESOURCE  { Nexus.Resource }
    ;
optional_pictureformat:
    | FORMAT EQUAL pictureformat { Some $3 }
    | { None }
    ;
pictureformat:
    | PICT      { Nexus.Pict }
    | TIFF      { Nexus.Tiff }
    | EPS       { Nexus.Eps }
    | JPEG      { Nexus.Jpeg }
    | GIF       { Nexus.Gif }
    ;
optional_encode:
    | ENCODE EQUAL pictureencoding { Some $3 }
    | { None }
    ;
pictureencoding:
    | NONE  { Nexus.None }
    | UUENCODE { Nexus.UUEncode }
    | BINHEX    { Nexus.BinHex }
    ;
trees:
    | optional_translate tree_list { ($1, $2) }
    ;
optional_translate:
    | TRANSLATE names SEMICOLON
        { snd (List.fold_left (fun (i,acc) x -> (i+1),(string_of_int i,x)::acc) (1,[]) $2) }
    | TRANSLATE pairs_list SEMICOLON { $2 }
    | { [] }
    ;
tree_list:
    | DATA SEMICOLON tree_list { $1 :: $3 }
    | { [] }
    ;
poy_block:
    | CHARACTERBRANCH TREES EQUAL names NAMES EQUAL characterset_list SEMICOLON
                  MAP pairs_list_float SEMICOLON poy_block
        { Nexus.CharacterBranch ($4, $7, $10) :: $12}

    | LIKELIHOOD model_block poy_block { Nexus.Likelihood $2 :: $3 }
    | GAPOPENING do_star nexus_word EQUAL standard_type_set SEMICOLON poy_block
        { (Nexus.GapOpening ($2, $3, $5)) :: $7 }
    | WTSET do_star nexus_word EQUAL standard_type_set SEMICOLON poy_block
        { (Nexus.DynamicWeight ($2, $3, $5)) :: $7 }
    | TCM do_star nexus_word EQUAL standard_type_set SEMICOLON poy_block
        { (Nexus.Tcm ($2, $3, $5)) :: $7 }
    | { [] }
    ;
model_block:
    | MODEL EQUAL IDENT SEMICOLON model_block
            { (Nexus.Model $3) :: $5 }
    | VARIATION EQUAL IDENT SEMICOLON model_block
            { (Nexus.Variation $3) :: $5 }
    | SITES EQUAL INTEGER SEMICOLON model_block
            { (Nexus.Variation_Sites $3) :: $5 }
    | PERCENT EQUAL FLOAT SEMICOLON model_block
            { (Nexus.Variation_Invar $3) :: $5 }
    | ALPHA EQUAL FLOAT SEMICOLON model_block
            { (Nexus.Variation_Alpha $3) :: $5 }
    | PRIORS EQUAL pairs_list_float model_block
            { (Nexus.Priors $3) :: $4 }
    | CHARSET EQUAL characterset_list SEMICOLON model_block
            { (Nexus.Chars $3) :: $5 }
    | PARAMETERS EQUAL float_list model_block
            { (Nexus.Parameters $3) :: $4 }
    | FILE EQUAL QUOTED SEMICOLON model_block
            { (Nexus.Files $3) :: $5 }
    | SEMICOLON { [] }
    ;
float_list:
    | FLOAT float_list      { (float_of_string $1) :: $2 }
    | INTEGER float_list    { (float_of_string $1) :: $2 }
    | FLOAT SEMICOLON       { [(float_of_string $1)] }
    | INTEGER SEMICOLON     { [(float_of_string $1)] }
    ;
names:
    | IDENT COMMA names { $1 :: $3 }
    | INTEGER COMMA names { $1 :: $3 }
    | IDENT SEMICOLON { [$1] }
    ;
pairs_list:
    | identifiers IDENT COMMA pairs_list { ($1, $2) :: $4 }
    | identifiers IDENT { [$1, $2] }
    ;
pairs_list_float:
    | IDENT FLOAT COMMA pairs_list_float { ($1,(float_of_string $2)) :: $4 }
    | IDENT FLOAT SEMICOLON { [($1,(float_of_string $2))] }
    ;
characters:
    | DIMENSIONS optional_taxa_dimensions NCHAR EQUAL INTEGER SEMICOLON 
     optional_format optional_eliminate optional_taxlabels
     optional_charstatelabels optional_charlabels optional_statelabels 
     DATA SEMICOLON 
     { 
         { Nexus.char_taxon_dimensions = $2;
         Nexus.char_char_dimensions = $5;
         Nexus.char_format = $7;
         Nexus.char_eliminate = $8;
         Nexus.char_taxlabels = $9;
         Nexus.char_statelabels = $10;
         Nexus.char_charlabels = $11;
         Nexus.char_charstates = $12;
         Nexus.chars = $13;}
     }
unaligned:
    | optional_unaligned_dimensions optional_format DATA SEMICOLON
        { { Nexus.unal_taxon_dimensions = $1; unal_format = $2; unal = $3 } }
     ;
optional_unaligned_dimensions:
    | DIMENSIONS optional_taxa_dimensions { $2 }
    | { None }
    ;
optional_charlabels:
    | CHARLABELS taxonlist SEMICOLON    { $2 }
    |                                   { [] }
    ;
optional_charstatelabels:
    | CHARSTATELABELS charstatelables SEMICOLON { $2 }
    |                                           { [] }
    ;
charstatelables:
    | INTEGER nexus_word BACKSLASH taxonlist COMMA charstatelables { ($1, $2, $4) :: $6 }
    | INTEGER nexus_word BACKSLASH taxonlist { [] }
    ;
optional_statelabels:
    | STATELABELS statelabels SEMICOLON { $2 }
    |                           { [] }
    ;
statelabels:
    | INTEGER taxonlist COMMA statelabels { ($1, $2) :: $4 }
    |                                       { [] }
    ;
optional_taxlabels:
    | TAXLABELS taxonlist SEMICOLON { $2 }
    |                           { [] }
    ;
optional_eliminate:
    | ELIMINATE characterset SEMICOLON { Some $2 }
    |                           { None }
    ;
optional_format:
    | FORMAT format_items_list SEMICOLON { $2 }
    |           { [] }
    ;
format_items_list:
    | format_items format_items_list { $1 :: $2 }
    | error format_items_list { 
        report_error "Format list" (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ());
        $2 }
    |           { [] }
    ;
format_items:
    | DATATYPE EQUAL datatype { Nexus.Datatype $3 }
    | DATATYPE EQUAL error {
        Status.user_message Status.Error
        ("Many@ \"NEXUS\"@ files@ include@ in@ the@ CHARACTERS@ or@ DATA@ " ^
        "block@ the@ dataype@ @[<b>mixed@]@ or@ @[<b>restriction@]@ or@ something@ else.@ " ^
        "@ Those@ are@ most@ likely@ Mr.Bayes@ files.@ No@ matter@ that@ " ^
        "they@ say@ #NEXUS@ at@ the@ beginning,@ this@ is@ not@ a@ valid@ " ^
        "NEXUS@ file.@ I can@ not@ process@ this.@ I@ am@ sorry. " ^
        "You'll@ have@ to@ convert@ it@ to@ a@ valid@ nexus@ file.");
        raise Parsing.Parse_error }
    | RESPECTCASE { Nexus.RespectCase }
    | MISSING EQUAL symbol { Nexus.FMissing $3 }
    | GAP EQUAL symbol { Nexus.Gap $3 }
    | SYMBOLS EQUAL QUOTED { 
        let len = String.length $3 in
        Nexus.Symbols (String.sub $3 0 (len - 1))}
    | EQUATE EQUAL QUOTED { Nexus.Equate $3 }
    | MATCHCHAR EQUAL symbol { Nexus.MatchChar $3 }
    | NOLABELS     { Nexus.Labels false }
    | LABELS        { Nexus.Labels true }
    | TRANSPOSE     { Nexus.Transpose }
    | INTERLEAVE    EQUAL { 
        Status.user_message Status.Error
        ("Many@ \"NEXUS\"@ files@ include@ in@ the@ CHARACTERS@ or@ DATA@ " ^
        "block@ the@ directives@ interleave=yes@ or@ interleave=no.@ " ^
        "@[<b>That@ is@ not@ a@ valid@ NEXUS@ directive@].@ To@ fix@ your@ file@ " ^
        "replace@ interleave=yes@ with@ INTERLEAVE,@ or@ just@ remove@ it@ " ^
        "if@ you@ see@ interleave=no.");
        raise Parsing.Parse_error }
    | INTERLEAVE    { Nexus.Interleave }
    | ITEMS EQUAL item { Nexus.Items $3 }
    | STATESFORMAT EQUAL states_format { Nexus.StatesFormat $3 }
    | NOTOKENS     { Nexus.Tokens false }
    | TOKENS        { Nexus.Tokens true }
    | TRIANGLE EQUAL triangle_format { Nexus.Triangle $3 }
    ;
triangle_format:
    | LOWER { Nexus.Lower }
    | UPPER { Nexus.Upper }
    | BOTH { Nexus.Both }
    ;
datatype:
    | STANDARD { Nexus.DStandard }
    | DNA { Nexus.Dna }
    | RNA { Nexus.Rna }
    | NUCLEOTIDE { Nexus.Nucleotide }
    | PROTEIN { Nexus.Protein }
    | CONTINUOUS { Nexus.Continuous }
    ;
symbol:
    | IDENT { $1 }
    | DASH { "-" }
    | CHAR  { Char.escaped $1 }
    ;
symbol_list:
    | symbol symbol_list { $1 :: $2 }
    |       { [] }
    ;
symbol_pair:
    | symbol EQUAL symbol {($1, [$3])}
    | symbol EQUAL LPARENT symbol_list RPARENT { ($1, $4) }
    ;
item:
    | MIN { Nexus.Min }
    | MAX { Nexus.Max }
    | MEDIAN  { Nexus.Median }
    | AVERAGE { Nexus.Average }
    | VARIANCE { Nexus.Variance }
    | STDERROR { Nexus.Stderror }
    | SAMPLESIZE { Nexus.SampleSize }
    | STATES     { Nexus.States }
    ;
states_format:
    | STATESPRESENT { Nexus.StatesPresent }
    | INDIVIDUALS   { Nexus.Individuals }
    | COUNT         { Nexus.Count }
    | FREQUENCY     { Nexus.Frequency }
    ;
optional_taxa_dimensions:
    | NTAX EQUAL INTEGER { Some $3 }
    |               { None }
    ;
taxa:
    | DIMENSIONS NTAX EQUAL INTEGER SEMICOLON TAXLABELS taxonlist SEMICOLON 
        { ($4, $7) }
    ;
nexus_word :
    | IDENT { $1 }
    | SINGLEQUOTED { $1 }
    | DNA { $1 }
    | RNA { $1 }
    ;
taxonlist:
    | nexus_word taxonlist   { $1 :: $2 }
    |                   { [] }
    ;
characterset_list:
    | characterset characterset_list { $1 :: $2 }
    | characterset      { [$1] }
    ;
characterset:
    | INTEGER DASH CHAR optional_step   { Nexus.Range ($1, None, $4) }
    | INTEGER DASH INTEGER optional_step { Nexus.Range ($1, Some $3, $4) }
    | INTEGER              { Nexus.Single ($1) }
    | IDENT                { Nexus.Name $1 }
    | SINGLEQUOTED         { Nexus.Name $1 }
    | CHAR                 { Nexus.Name (String.make 1 $1) }
    | DNA                  { Nexus.Name "DNA" }
    | RNA                  { Nexus.Name "RNA" }
    | PROTEIN              { Nexus.Name "PROTEIN" }
    ;
optional_step:
    | SLASH INTEGER { (int_of_string $2) }
    | { 1 }
    ;
tree:
    | do_star nexus_word EQUAL single_tree EOF { $4 }
    ;
single_tree:
    | identifiers optional_length { Nexus.Leaf ($1, $2) }
    | LPARENT single_tree_list RPARENT optional_label optional_length 
                            { Nexus.Node ($2, $4, $5) }
    ;
identifiers:
    | IDENT { $1 }
    | INTEGER { $1 }
    ;
single_tree_list:
    | single_tree COMMA single_tree_list { $1 :: $3 }
    | single_tree { [$1] }
    ;
optional_length:
    | COLON INTEGER { Some (float_of_string $2) }
    | COLON FLOAT { Some (float_of_string $2) }
    | { None }
    ;
optional_label:
    | nexus_word     { Some $1 }
    | { None }
    ;
any_thing_minus_end:
    | TAXA  { () }
    | DATA  { () }
    | UNALIGNED  { () }
    | TREES  { () }
    | NOTES  { () }
    | DISTANCES  { () }
    | ASSUMPTIONS  { () }
    | SETS  { () }
    | CHARACTERS { () }
    | ANCSTATES { () }
    | AVERAGE { () }
    | BINHEX { () }
    | BOTH { () }
    | CHANGESET { () }
    | CHAR { () }
    | CHARACTER { () }
    | CHARLABELS { () }
    | CHARPARTITION { () }
    | CHARSTATELABELS { () }
    | CODEORDER { () }
    | CODESET { () }
    | CODONS { () }
    | CONDONPOSSET { () }
    | CONTINUOUS { () }
    | COUNT { () }
    | CSTREE { () }
    | QUOTED { () }
    | DATATYPE { () }
    | DEFTYPE { () }
    | DIAGONAL { () }
    | DIMENSIONS { () }
    | DNA { () }
    | DROS { () }
    | ELIMINATE { () }
    | ENCODE { () }
    | EPS { () }
    | EQUATE { () }
    | EXSET { () }
    | EXT { () }
    | EXTENSIONS { () }
    | FILE { () }
    | FORMAT { () }
    | FREQUENCY { () }
    | GAP { () }
    | GAPMODE { () }
    | GENETICCODE { () }
    | GIF { () }
    | INDIVIDUALS { () }
    | INLINE { () }
    | INTERLEAVE { () }
    | ITEMS { () }
    | JPEG { () }
    | LABELS { () }
    | LOWER { () }
    | MAM { () }
    | MATCHCHAR { () }
    | MATRIX { () }
    | MAX { () }
    | MAXSTEPS { () }
    | MEDIAN { () }
    | MIN { () }
    | MINSTEPS { () }
    | MISSING { () }
    | MTDNA { () }
    | NCHAR { () }
    | NEWSTATE { () }
    | NEWTAXA { () }
    | NONE { () }
    | NTAX { () }
    | NUCLEOTIDE { () }
    | NUCORDER { () }
    | OPTIONS { () }
    | PICT { () }
    | PICTURE { () }
    | POLYTCOUNT { () }
    | PROTEIN { () }
    | RESOURCE { () }
    | RESPECTCASE { () }
    | RNA { () }
    | SAMPLESIZE { () }
    | SOURCE { () }
    | STANDARD { () }
    | STATE { () }
    | STATELABELS { () }
    | STATES { () }
    | STATESET { () }
    | STATESFORMAT { () }
    | STATESPRESENT { () }
    | STDERROR { () }
    | STEPMATRIX { () }
    | SYMBOLS { () }
    | TAXON { () }
    | TAXLABELS { () }
    | TAXPARTITION { () }
    | TAXSET { () }
    | TEXT { () }
    | TIFF { () }
    | TOKENS { () }
    | TRANSLATE { () }
    | TRANSPOSE { () }
    | TREE { () }
    | TREEPARTITION { () }
    | TREESET { () }
    | TRIANGLE { () }
    | TYPESET { () }
    | UNIVERSAL { () }
    | UPPER { () }
    | USERTYPE { () }
    | UUENCODE { () }
    | VARIANCE { () }
    | VECTOR { () }
    | WTSET { () }
    | YEAST { () }
    | EIDENT { () }
    | STAR { () }
    | COLON { () }
    | IDENT { () }
    | FLOAT { () }
    | INTEGER { () }
    | SEMICOLON  { () }
    | EQUAL { () }
    | COMMA { () }
    | QUOTE { () }
    | BACKSLASH { () }
    | DASH { () }
    | LPARENT { () }
    | RPARENT { () }
    | SINGLEQUOTED { () }
    ;
