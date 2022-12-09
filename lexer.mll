{
  open Lexing
  open Parser

  exception Error of char
}

let alpha = ['a'-'z' 'A'-'Z']
let num   = ['0'-'9']
let bool  = "true" | "false"
let ident = alpha (alpha | num | '-' | '_')*

rule token = parse
| eof           { Lend }
| [ ' ' '\t' ]  { token lexbuf }
| '\n'          { Lexing.new_line lexbuf; token lexbuf }
| num+ as n     { Lint (int_of_string n) }
| "return"      { Lreturn }
| "int"         { Ltype (Int_t) }
| "bool"        { Ltype (Bool_t) }
| bool as b     { Lbool (bool_of_string b) }
| '='           { Lassign }
| ';'           { Lsc }
| '+'           { Ladd}
| ident as i    { Lvar i }
| '#'           { comment lexbuf }
| _ as c        { raise (Error c) }

and comment = parse
| eof  { Lend }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| _    { comment lexbuf }
