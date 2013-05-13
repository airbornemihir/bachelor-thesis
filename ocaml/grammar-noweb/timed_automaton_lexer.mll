  (* File timed_automaton_lexer.mll *)
  {
   open Timed_automaton_parser        (* The type token is defined in parser.mli *)
exception Eof
 }
  let intexp = ['0'-'9']+
  let identexp = ['A'-'Z'](['A'-'Z']|['0'-'9'])*
  rule token = parse
  [' ' '\t']       { token lexbuf }     (* skip blanks *)
| ['\n' ]          { token lexbuf }
| intexp as lxm    { INT(int_of_string lxm) }
| "TRUE"           { TRUE }
| "FALSE"          { FALSE }
| "ACT"            { ACT }
| "RESET"          { RESET }
| identexp as lxm  { IDENT(lxm)}
| "=>"             { RARROW }
| ':'              { COLON }
| ';'              { SEMI }
| '{'              { LBRACE }
| '}'              { RBRACE }
| "#locations"     { NUMLOCATIONS }
| "#trans"         { NUMTRANS }
| "#clocks"        { NUMCLOCKS }
| "#actions"       { NUMACTIONS }
| "#init"          { NUMINIT }
| "location"       { LOCATION }
| "prop"           { PROP }
| "invar"          { INVAR }
| "trans"          { TRANS }
| "goto"           { GOTO }
| "<="             { LE }
| ">="             { GE }
| '<'              { LT }
| '>'              { GT }
| '='              { EQ }
| "and"            { AND}
| "AND"            { AND}
| eof              { EOF }
