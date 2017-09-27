(* Hennig lexer *)
{
    open HennigParser
    exception Eof

    let ( --> ) a b = b a

    (* to avoid complete prefixes of another command *)
    let full_commands = Hashtbl.create 200 (* there are around 170 full commands *)
    (* this is the end all Hashtable --initially holds only prefixes *)
    let keyword_table = Hashtbl.create 1000 (* there are around 700 prefixes *)

    let () = 
       (** [make_all_prefixes min cmd output] 
        * makes all prefixes, and ONLY prefixes and adds
        * them to the keyword table. Duplicates are left in, and filtered
        * latter. The full commands are added to the full_command table. *)
        let make_all_prefixes min_len fullcommand output =
            let max_len = String.length fullcommand in 
            let rec prepend len =
                if len > (max_len-1) then
                    Hashtbl.add full_commands fullcommand output
                else 
                    let comm = 
                        String.uppercase (String.sub fullcommand 0 len) 
                    in
                    (* _could_ do checking here, but we'd have to check later anyway *)
                    Hashtbl.add keyword_table comm output;
                    prepend (len + 1) 
            in
            prepend min_len 
        in
        (** [remove_dups tbl] 
         * removes dups from the table. Add more prefixes? run this again *)
        let remove_dups () =
            let rec remove_all elm =
                if Hashtbl.mem keyword_table elm then
                    ( Hashtbl.remove keyword_table elm; remove_all elm )
                else ()
            in
            (* remove dups and full commands from prefixes, completely *)
            Hashtbl.iter
                (fun x y ->
                    if Hashtbl.mem full_commands x then remove_all x
                    else match Hashtbl.find_all keyword_table x with
                        | hd :: [] -> ()
                        | _ -> remove_all x
                ) keyword_table;
            (* add in full commands *)
            Hashtbl.iter (fun x y -> Hashtbl.add keyword_table x y) full_commands
        in
        (* full commands, no prefixes allowed. If there is a possibility of a
        * prefix, then add it to the list after these using make_all_prefixes *)
        Hashtbl.add full_commands "DNA"  DNA;
        Hashtbl.add full_commands "PROT" PROTEINS;
        Hashtbl.add full_commands "NUM" NUMBER;
        Hashtbl.add full_commands "GAPS" GAPS;
        Hashtbl.add full_commands "NOGAPS" NOGAPS;
        (* The table of all the commands accepted by TNT in July, 2008 *)
        (* We won't interpret them all, but it should be easy to add support 
        * to some command if needeed, and we will maintain as much compatibility
        * as possible *)
        make_all_prefixes 2 "ABSINCL" ABSINCL;
        make_all_prefixes 2 "AGROUP" AGROUP;
        make_all_prefixes 2 "ALLTREES" ALLTREES;
        make_all_prefixes 2 "ANCSTATES" ANCSTATES;
        make_all_prefixes 2 "APO" APO;
        make_all_prefixes 2 "BBREAK" BBREAK;
        make_all_prefixes 2 "BEEP" BEEP;
        make_all_prefixes 2 "BEST" BEST;
        make_all_prefixes 2 "BGROUND" BGROUND;
        make_all_prefixes 2 "BLENGTH" BLENGTH;
        make_all_prefixes 2 "BLOCKS" BLOCKS;
        make_all_prefixes 2 "BREAK" BREAK;
        make_all_prefixes 2 "BSUPPORT" BSUPPORT;
        make_all_prefixes 2 "CCODE" CCODE;
        make_all_prefixes 2 "CDIR" CDIR;
        make_all_prefixes 2 "CHANGE" CHANGE;
        make_all_prefixes 2 "CHKMOVES" CHKMOVES;
        make_all_prefixes 2 "CHOMO" CHOMO;
        make_all_prefixes 2 "CKEEP" CKEEP;
        make_all_prefixes 2 "CLS" CLS;
        make_all_prefixes 2 "CLBUFFER" CLBUFFER;
        make_all_prefixes 2 "CNAMES" CNAMES;
        make_all_prefixes 2 "COLLAPSE" COLLAPSE;
        make_all_prefixes 2 "COMCOMP" COMCOMP;
        make_all_prefixes 2 "CONDENSE" CONDENSE;
        make_all_prefixes 2 "CONSTRAIN" CONSTRAIN;
        make_all_prefixes 2 "COSTS" COSTS;
        make_all_prefixes 2 "CSCORES" CSCORES;
        make_all_prefixes 2 "CSTREE" CSTREE;
        make_all_prefixes 2 "DMERGE" DMERGE;
        make_all_prefixes 2 "DRIFT" DRIFT;
        make_all_prefixes 2 "EDIT" EDIT;
        make_all_prefixes 2 "ECHO" ECHO;
        make_all_prefixes 2 "EXPORT" EXPORT;
        make_all_prefixes 2 "FIT" FIT;
        make_all_prefixes 2 "FILLSANK" FILLSANK;
        make_all_prefixes 2 "FORCE" FORCE;
        make_all_prefixes 2 "FREQDIFS" FREQDIFS;
        make_all_prefixes 2 "HELP" HELP;
        make_all_prefixes 2 "HOLD" HOLD;
        make_all_prefixes 2 "HYBRID" HYBRID;
        make_all_prefixes 2 "IENUM" IENUM;
        make_all_prefixes 2 "INCLTAX" INCLTAX;
        make_all_prefixes 2 "INFO" INFO;
        make_all_prefixes 2 "LINTREES" LINTREES;
        make_all_prefixes 2 "LOG" LOG;
        make_all_prefixes 2 "KEEP" KEEP;
        make_all_prefixes 2 "LENGTH" LENGTH;
        make_all_prefixes 2 "LQUOTE" LQUOTE;
        make_all_prefixes 2 "MAJORITY" MAJORITY;
        make_all_prefixes 2 "MAP" MAP;
        make_all_prefixes 2 "MINMAX" MINMAX;
        make_all_prefixes 2 "MIXTREES" MIXTREES;
        make_all_prefixes 2 "MONO" MONO;
        make_all_prefixes 2 "MRP" MRP;
        make_all_prefixes 2 "MULT" MULT;
        make_all_prefixes 2 "MXRAM" MXRAM;
        make_all_prefixes 2 "MXPROC" MXPROC;
        make_all_prefixes 2 "NAKED" NAKED;
        make_all_prefixes 2 "NELSEN" NELSEN;
        make_all_prefixes 2 "NSTATES" NSTATES;
        make_all_prefixes 2 "OUTGROUP" OUTGROUP;
        make_all_prefixes 2 "PROCEDURE" PROCEDURE;
        make_all_prefixes 2 "PAUSE" PAUSE;
        make_all_prefixes 2 "PFIJO" PFIJO;
        make_all_prefixes 2 "PIWE" PIWE;
        make_all_prefixes 2 "PRUNCOM" PRUNCOM;
        make_all_prefixes 2 "PRUNMAJOR" PRUNMAJOR;
        make_all_prefixes 2 "PRUNNELSEN" PRUNNELSEN;
        make_all_prefixes 2 "PRUNTAX" PRUNTAX;
        make_all_prefixes 2 "PTNT" PTNT;
        make_all_prefixes 2 "QCOLLAPSE" QCOLLAPSE;
        make_all_prefixes 2 "QNELSEN" QNELSEN;
        make_all_prefixes 2 "QUOTE" QUOTE;
        make_all_prefixes 2 "QUIT" QUIT;
        make_all_prefixes 2 "RANDTREES" RANDTREES;
        make_all_prefixes 2 "RATCHET" RATCHET;
        make_all_prefixes 2 "RDIR" RDIR;
        make_all_prefixes 2 "RECONS" RECONS;
        make_all_prefixes 2 "REPORT" REPORT;
        make_all_prefixes 2 "REROOT" REROOT;
        make_all_prefixes 2 "RESAMPLE" RESAMPLE;
        make_all_prefixes 2 "RESOLS" RESOLS;
        make_all_prefixes 2 "RFREQS" RFREQS;
        make_all_prefixes 2 "RIDDUP" RIDDUP;
        make_all_prefixes 2 "RSEED" RSEED;
        make_all_prefixes 2 "RUN" RUN;
        make_all_prefixes 2 "SAVE" SAVE;
        make_all_prefixes 2 "SCREEN" SCREEN;
        make_all_prefixes 2 "SCORES" SCORES;
        make_all_prefixes 2 "SECTSCH" SECTSCH;
        make_all_prefixes 2 "SHORTREAD" SHORTREAD;
        make_all_prefixes 2 "SILENT" SILENT;
        make_all_prefixes 2 "SLFWT" SLFWT;
        make_all_prefixes 2 "SLAVEPROC" SLAVEPROC;
        make_all_prefixes 2 "SMATRIX" SMATRIX;
        make_all_prefixes 2 "SORT" SORT;
        make_all_prefixes 2 "SPRDIFF" SPRDIFF;
        make_all_prefixes 2 "SUBOPT" SUBOPT;
        make_all_prefixes 2 "SVTXT" SVTXT;
        make_all_prefixes 2 "SYSTEM" SYSTEM;
        make_all_prefixes 2 "TABLES" TABLES;
        make_all_prefixes 2 "TAGSET" TAGSET;
        make_all_prefixes 2 "TAXCODE" TAXCODE;
        make_all_prefixes 2 "TAXLABELS" TAXLABELS;
        make_all_prefixes 2 "TAXNAME" TAXNAME;
        make_all_prefixes 2 "TCHOOSE" TCHOOSE;
        make_all_prefixes 2 "TCOMP" TCOMP;
        make_all_prefixes 2 "TEQUAL" TEQUAL;
        make_all_prefixes 2 "TFUSE" TFUSE;
        make_all_prefixes 2 "TGROUP" TGROUP;
        make_all_prefixes 2 "THANKS" THANKS;
        make_all_prefixes 2 "TIMEOUT" TIMEOUT;
        make_all_prefixes 2 "TNODES" TNODES;
        make_all_prefixes 2 "TPLOT" TPLOT;
        make_all_prefixes 2 "TSAVE" TSAVE;
        make_all_prefixes 2 "TSHRINK" TSHRINK;
        make_all_prefixes 2 "TSIZE" TSIZE;
        make_all_prefixes 2 "TTAGS" TTAGS;
        make_all_prefixes 2 "TXTSIZE" TXTSIZE;
        make_all_prefixes 2 "TZERT" TZERT;
        make_all_prefixes 2 "VIEW " VIEW ;
        make_all_prefixes 2 "VVERSION" VVERSION;
        make_all_prefixes 2 "WARN" WARN;
        make_all_prefixes 2 "WATCH" WATCH;
        make_all_prefixes 2 "XCOMP" XCOMP;
        make_all_prefixes 2 "XGROUP" XGROUP;
        make_all_prefixes 2 "XINACT" XINACT;
        make_all_prefixes 2 "XMULT" XMULT;
        make_all_prefixes 2 "XPERM" XPERM;
        make_all_prefixes 2 "UNIQUE" UNIQUE;
        make_all_prefixes 2 "UNSHARED" UNSHARED;
        make_all_prefixes 2 "USMINMAX" USMINMAX;
        make_all_prefixes 2 "ZZZ" ZZZ;
        make_all_prefixes 2 "CONTINUE" CONTINUE;
        make_all_prefixes 2 "COPYTREE" COPYTREE;
        make_all_prefixes 2 "CHKBREAK" CHKBREAK;
        (* Comment them out because of clash with Camlp4.
        * make_all_prefixes 2 "ELSE" ELSE;
        make_all_prefixes 2 "END" END;*)
        make_all_prefixes 2 "ENDLOOP" ENDLOOP;
        make_all_prefixes 2 "ENDSWAP" ENDSWAP;
        make_all_prefixes 2 "ERRMSG" ERRMSG;
        make_all_prefixes 2 "GOTO" GOTO;
        make_all_prefixes 2 "IF" IF;
        make_all_prefixes 2 "ITERRECS" ITERRECS;
        make_all_prefixes 2 "LABEL" LABEL;
        make_all_prefixes 2 "LOOP" LOOP;
        make_all_prefixes 2 "MACFLOAT" MACFLOAT;
        make_all_prefixes 2 "MACRO" MACRO;
        make_all_prefixes 2 "MACREPORT" MACREPORT;
        make_all_prefixes 2 "MACSEED" MACSEED;
        make_all_prefixes 2 "MAKETABLE" MAKETABLE;
        make_all_prefixes 2 "PRIVATE" PRIVATE;
        make_all_prefixes 2 "PROGRESS" PROGRESS;
        make_all_prefixes 2 "RECURSE" RECURSE;
        make_all_prefixes 2 "RESETSWAP" RESETSWAP;
        make_all_prefixes 2 "RESETTIME" RESETTIME;
        make_all_prefixes 2 "RETURN" RETURN;
        make_all_prefixes 2 "SAFE" SAFE;
        make_all_prefixes 2 "SET" SET;
        make_all_prefixes 2 "SETARRAY" SETARRAY;
        make_all_prefixes 2 "SETLOOP" SETLOOP;
        make_all_prefixes 2 "SLAVEGET" SLAVEGET;
        make_all_prefixes 2 "SLAVESET" SLAVESET;
        make_all_prefixes 2 "SPRIT" SPRIT;
        make_all_prefixes 2 "TBRIT" TBRIT;
        make_all_prefixes 3 "TRAVTREE" TRAVTREE;
        make_all_prefixes 2 "VAR" VAR;
        (* REMOVE DUPS IS THE LAST OPERATION *)
        remove_dups ()

    let is_prefix a b =
        let la = String.length a in
        la <= (String.length b) &&
        (a = (String.sub b 0 la))
}

rule token = parse
      [ ' ' '\t' '\n' '\010' '\013' '\012' ]    { token lexbuf }
    | [';'] { SEMICOLON }
    | [ '-' ] { DASH }
    | [ '0' - '9']+ as id { INT id }
    | [ '[' ] { LSQ }
    | [ ']' ] { RSQ }
    | [ '{' ] { characters_names lexbuf }
    | [ '+' ] { PLUS }
    | [ '*' ] { STAR }
    | [ '/' ] { BACKSLASH }
    | [ '.' ] { DOT }
    | [ '>' ] { GT }
    | [ '=' ] { EQUAL }
    | [ '?' ] { QUESTION }
    | [ '(' ] { LPARENT }
    | [ ')' ] { RPARENT }
    | [ 'a'-'z' 'A'-'Z']+ as word { 
        let uword = String.uppercase word in
        try Hashtbl.find keyword_table uword with
        | Not_found -> 
                if is_prefix uword "TREAD" then rawtree lexbuf
                else if is_prefix uword "XREAD" then raw lexbuf
                else WORD word }
    | [ ^ '\000' ] as ch { CHAR ch } 
    | eof       { raise Eof }
and characters_names = parse
     [^ ';']+[';'] as r { CHARNAME r }
and raw = parse
     [^ ';']* as d      { DATA d }
and rawtree = parse
      [ ' ' '\t' '\n' '\010' '\013' '\012' ]    { rawtree lexbuf }
    | [ '\'' ]  { ignore_quote2 lexbuf }
    | [^ '\'' ';']* as d      { TREES d }
and ignore_quote2 = parse
    | [^ '\'']+['\'']   { rawtree lexbuf }
and ignore_quote = parse
    | [^ '\'']*['\'']   { xread lexbuf }
and xread = parse 
      [ ' ' '\t' '\n' '\010' '\013' '\012' ]    { xread lexbuf }
    | [ '\'' ]  { ignore_quote lexbuf }
    | [ '0'-'9']+ as integer { INT integer }
    | ['a'-'z' 'A'-'Z' '_' '-' '.' '?'][^ '\000']+ eof as data    { DATA data }
