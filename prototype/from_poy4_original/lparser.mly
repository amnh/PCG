/* The basic primitive types that we support */
%token <bool> BOOL
%token <string> STRING
%token <string> BASE
%token <int> INTEGER
%token <float> FLOAT

/* User defined functions and variables and internal commands */
%token <string> NAME
%token <string> COMMAND

%token IMPORT
%token LET
%token IN
%token SEMI             /* ; */
%token IF
%token THEN
%token ELSE
%token FOR
%token EACH
%token DO
%token DONE
%token USING
%token BEGIN
%token END
%token LP               /* ( */
%token RP               /* ) */
%token NOT
%token IS
%token COMMA            /* , */
%token PLUS             /* + */
%token MINUS            /* - */
%token TIMES            /* * */
%token DIV              /* / */
%token MOD              /* mod */
%token JOIN_STRING      /* ^ */
%token JOIN_SETS        /* @ */
%token JOIN_ARRAY       /* & */
%token JOIN_LIST        /* :: */
%token IS               /* = */
%token EQ               /* == */
%token NEQ              /* <> */
%token LT               /* < */
%token GT               /* > */
%token AND
%token OR
%token TO
%token COMMENT          /* # .... \n */
%token RETURN
%token CHARACTER
%token ALPHABET
%token WORD
%token REC
%token PRE
%token HEAD
%token TAIL
%token PREDECCESSOR
%token SUCCESSOR
%token PROB
%token FREQ
%token LAMBDA
%token EOF
%left JOIN_LIST JOIN_SETS JOIN_ARRAY 
%right SEMI IS
%right HEAD TAIL
%right PREDECCESSOR SUCCESSOR 
%left PRE 
%nonassoc TO
%start macro
%type <Lang4.a_poy_macro> macro
%type <Lang4.expression Lang4.base> ubase 
%type <Lang4.expression Lang4.integer> uinteger
%type <Lang4.expression Lang4.sequence> usequence
%type <Lang4.expression> uexpression 
%type <Lang4.expression Lang4.func> ufun_application
%type <Lang4.expression Lang4.func> uname
%type <Lang4.expression> ufunction
%type <Lang4.boolean> uboolean
%%

macro:
    | EOF                        { [] }
    | COMMENT macro         { $2 }
    | fun_def macro         { $2 }
    | import_file macro     { $2 }
    | character macro       { $1 :: $2 }
    | probabilities macro   { $2 }
    | alphabet macro        { $2 }
;

/*                 ******************************************           */
/*                           POY Language Grammar                       */
/*                 ******************************************           */

fun_def:
    | definition SEMI SEMI
        { print_string "A function has been defined\n"; flush stdout; }
;

definition:
    LET the_function IS expression { () }
;

the_function :
    | NAME definition_parameters { () }
;

definition_parameters:
                                  { () }
    | NAME definition_parameters  { () }
;

boolean_term:
    | boolean_term AND boolean_composition { () }
    | boolean_term OR boolean_composition { () }
    | factor EQ factor { () }
    | factor NEQ factor { () }
    | factor GT factor { () }
    | factor LT factor { () }
;

boolean_composition:
    | NOT boolean_composition { () }
    | BOOL { () }
    | LP boolean_term RP { () }
;

arithmetic_composition:
    | arithmetic_composition PLUS term { () }
    | arithmetic_composition MINUS term { () }
    | term { () }
;

term:
    | term TIMES factor { () }
    | term DIV factor { () }
    | term MOD factor { () }
    | factor { () }
;

factor:
    | INTEGER { () }
    | FLOAT { () }
    | function_application { () }
    | LP arithmetic_composition RP { () }
;

string_composition:
    | STRING { () }
    | STRING JOIN_STRING string_composition { () }
    | LP string_composition RP JOIN_STRING string_composition { () }
    | LP string_composition RP { () }
;

valued_expression:
    | boolean_term { () }
    | string_composition { () }
    | arithmetic_composition { () }
;

valid_conditions:
    | boolean_term { () }
    | function_application { () }
;

valid_arithmetics:
    | arithmetic_composition { () }
;

else_flag:
    | ELSE valid_if_else_block { () }
;

valid_if_else_block:
    | without_if_then_else { () }
    | BEGIN expressions END { () }
;

if_then_no_else:
    | IF valid_conditions THEN without_if_then_else { () }
;

if_then_else:
    | IF valid_conditions THEN valid_if_else_block else_flag { () }
;

using_flag:
    { () }
    | BEGIN USING expressions END { () }
;

for_to:
    | FOR NAME IS valid_arithmetics TO valid_arithmetics DO expressions 
      DONE using_flag { () }
;

without_if_then_else:
    | NAME IS valued_expression { () }
    | if_then_no_else { () }
    | for_to { () }
    | RETURN valued_expression { () }
    | valued_expression { () }
;

expression:
    | without_if_then_else { () }
    | if_then_else { () }
    | BEGIN expressions END { () }
;

function_application:
    | NAME function_parameters { () }
    | command_application { () }
;

command_application:
    COMMAND function_parameters { () }
;

function_parameters:
    { () }
    | NAME function_parameters { () }
    | INTEGER function_parameters { () }
    | FLOAT function_parameters { () }
    | BOOL function_parameters { () }
    | COMMAND function_parameters { () }
    | LP arithmetic_composition RP function_parameters { () }
    | LP boolean_composition RP function_parameters { () }
    | LP string_composition RP function_parameters { () }
;

expressions:
    { () }
    | expression { () }
    | expression SEMI expressions { () }
;

/*                 ******************************************           */
/*                              Importing a File                        */
/*                 ******************************************           */
import_file:
    IMPORT STRING { () }
    | IMPORT STRING SEMI SEMI { () }
;

/*                 ******************************************           */
/*                          Character Specification                     */
/*                 ******************************************           */
character:
    | CHARACTER NAME STRING { `Acharacter ($2, []) }
    | CHARACTER NAME STRING SEMI SEMI { `Acharacter ($2, []) }
    | CHARACTER NAME BEGIN ulanguage END { `Acharacter ($2, $4) }
    | CHARACTER NAME BEGIN ulanguage END SEMI SEMI { `Acharacter ($2, $4) }
;

/*                 ******************************************           */
/*                             U Language Grammar                       */
/*                 ******************************************           */

uname:
    | NAME { `Var $1 }
;

uboolean:
    | NAME EQ INTEGER   { `Boolean (`Var $1, `CInt $3) }
    | NAME EQ STRING    { `Boolean (`Var $1, `CSeq $3) }
    | NAME EQ BASE      { `Boolean (`Var $1, `CBase $3) }
;

uinteger:
    | INTEGER                   
        { `CInt $1 }
    | PREDECCESSOR INTEGER      
        { `Predeccessor (`CInt $2) }
    | SUCCESSOR INTEGER         
        { `Successor (`CInt $2) }
    | PREDECCESSOR uname         
        { `Predeccessor ($2 :> Lang4.expression Lang4.integer) }
    | SUCCESSOR uname            
        { `Successor ($2 :> Lang4.expression Lang4.integer) }
    | PREDECCESSOR LP ufun_application RP 
        { `Predeccessor ($3 :> Lang4.expression Lang4.integer) }
    | SUCCESSOR LP ufun_application RP    
        { `Successor ($3 :> Lang4.expression Lang4.integer) }
;

ubase:
    | BASE 
        { `CBase $1 }
    | HEAD STRING 
        { `Head (`CSeq $2 ) }
    | HEAD uname 
        { `Head ($2 :> Lang4.expression Lang4.sequence) }
    | HEAD LP ufun_application RP   
        { `Head ($3 :> Lang4.expression Lang4.sequence) }
    | HEAD LP usequence RP          
        { `Head ($3 :> Lang4.expression Lang4.sequence) }
; 

usequence:
    | STRING { `CSeq $1 }
    | TAIL STRING { `Tail (`CSeq $2) }
    | TAIL NAME { `Tail (`Var $2)}
    | TAIL LP ufun_application RP { `Tail ($3 :> Lang4.expression Lang4.sequence) }
    | TAIL LP usequence RP { `Tail ($3) }
    | BASE PRE STRING { `Prepend (`CBase $1, `CSeq $3) }
    | BASE PRE uname   { 
        `Prepend (`CBase $1, ($3 :> Lang4.expression Lang4.sequence)) }
    | uname PRE uname   { 
        `Prepend (($1 :> Lang4.expression Lang4.base), 
            ($3 :> Lang4.expression Lang4.sequence)) }
    | uname PRE LP ufun_application RP { 
        `Prepend (($1 :> Lang4.expression Lang4.base),
            ($4 :> Lang4.expression Lang4.sequence)) }
    | BASE PRE LP ufun_application RP { 
        `Prepend (`CBase $1, ($4 :> Lang4.expression Lang4.sequence)) }
    | LP ufun_application RP PRE STRING     { 
        `Prepend (($2 :> Lang4.expression Lang4.base), `CSeq $5) }
    | LP ufun_application RP PRE uname       { 
        `Prepend (($2 :> Lang4.expression Lang4.base),
            ($5 :> Lang4.expression Lang4.sequence)) }
    | LP ufun_application RP PRE LP ufun_application RP     { 
        `Prepend (($2 :> Lang4.expression Lang4.base),
            ($6 :> Lang4.expression Lang4.sequence)) }
    | LP ubase RP PRE LP ufun_application RP  { `Prepend ($2, ($6 :>
    Lang4.expression Lang4.sequence)) }
    | LP ufun_application RP PRE LP usequence RP  { `Prepend (($2 :>
    Lang4.expression Lang4.base), $6) }
    | LP ubase RP PRE LP usequence RP  { `Prepend ($2, $6) }
;

uexpression:
    | uname                 { $1 :> Lang4.expression }
    | ufun_application      { $1 :> Lang4.expression }
    | ubase                 { $1 :> Lang4.expression }
    | usequence             { $1 :> Lang4.expression }
    | uinteger              { $1 :> Lang4.expression }
;

ufun_parameters:
    | uname                           
        { [($1 :> Lang4.expression) ] : Lang4.expression list }
    | uconstant                           
        { [$1] : Lang4.expression list }
    | LP uexpression RP                   
        { [$2] : Lang4.expression list }
    | uconstant ufun_parameters             
        { ($1 :: $2) : Lang4.expression list }
    | uname ufun_parameters 
        { ($1 :> Lang4.expression) :: $2  : Lang4.expression list }
    | LP uexpression RP ufun_parameters   
        { ($2 :: $4) : Lang4.expression list }
;

ufun_application:
    | NAME ufun_parameters 
        { (`Function ($1, $2) :> Lang4.expression Lang4.func)}
;

uconstant:
    | LAMBDA    { `CSeq "" }
    | INTEGER   { `CInt $1 }
    | STRING    { `CSeq $1 }
    | BASE      { `CBase $1 }
;

ufun_declaration_param:
    |               { [] }
    | NAME ufun_declaration_param { `Var $1 :: $2 }
;

ufun_declaration:
    | NAME ufun_declaration_param { `Var $1, $2 }
;

ufunction:
    | IF uboolean THEN ufunction ELSE ufunction 
        { `If_then ($2, $4, $6) :> Lang4.expression }
    | uexpression         
        { $1 }
;

ufun:
    LET ufun_declaration IS ufunction { let a, b = $2 in `Aufunc (a, b, $4) }
;

ulanguage:
    | ufun                    { [$1] }
    | ufun ulanguage          { $1 :: $2 }
;

/*                 ******************************************           */
/*                         Probability Specification                    */
/*                 ******************************************           */
simple_probability:
    | PROB NAME IS expression { () }
    | PROB NAME IS FREQ { () }
    | PROB STRING IN NAME IS expression { () }
;

set_of_probabilities:
     { () }
    | simple_probability SEMI set_of_probabilities { () }
;

probabilities:
    | simple_probability { () }
    | simple_probability SEMI SEMI { () }
    | PROB NAME BEGIN set_of_probabilities END { () }
    | PROB NAME BEGIN set_of_probabilities END SEMI SEMI { () }
;

/*                 ******************************************           */
/*                           Alphabet Specification                     */
/*                 ******************************************           */
alphabet_elements:
      { () }
    | NAME alphabet_elements { () }
;

alphabet:   
    | ALPHABET NAME BEGIN alphabet_elements END { () }
    | ALPHABET NAME BEGIN alphabet_elements END SEMI SEMI { () }
;
/*                 ******************************************           */
