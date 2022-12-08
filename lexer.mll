{
  open Lexing
  open Parser

  exception Error of char
}

let alpha = ['a'-'z' 'A'-'Z']
let num   = ['0'-'9']
let ident = alpha (alpha | num | '-' | '_')*

rule token = parse
| eof           { Lend }
| [ ' ' '\t' ]  { token lexbuf }
| '\n'          { Lexing.new_line lexbuf; token lexbuf }
| num+ as n     { Lint (int_of_string n) }
| "int"         { Ltype (Int_t) }
| '='           { Lassign }
| ';'           { Lsc }
| ident as i    { Lvar i }
| _ as c        { raise (Error c) }
