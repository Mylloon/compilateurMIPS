{
  open Errors
  open Lexing
  open Parser
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
  | bool as b     { Lbool (bool_of_string b) }
  | "return"      { Lreturn }
  | "int"         { Ltype (Int_t) }
  | "bool"        { Ltype (Bool_t) }
  | "void"        { Ltype (Void_t) }
  | "str"         { Ltype (Str_t) }
  | "if"          { Lif }
  | "else"        { Lelse }
  | "while"       { Lwhile }
  | '{'           { Lbracedeb }
  | '}'           { Lbracefin }
  | '('           { Lpardeb }
  | ')'           { Lparfin }
  | ','           { Lcomma }
  | '='           { Lassign }
  | ';'           { Lsc }
  | '+'           { Ladd }
  | '-'           { Lsub }
  | '*'           { Lmul }
  | '/'           { Ldiv }
  | '%'           { Lrem }
  | "=="          { Lseq }
  | ">="          { Lsge }
  | ">"           { Lsgt }
  | "<="          { Lsle }
  | "<"           { Lslt }
  | "!="          { Lsne }
  | "&&"          { Land }
  | "||"          { Lor }
  | '"'           { read_string (Buffer.create 16) lexbuf }
  | ident as i    { Lvar i }
  | '#'           { comment lexbuf }
  | _ as c        { raise (LexerErrorC c) }

and comment = parse
  | eof   { Lend }
  | '\n'  { Lexing.new_line lexbuf; token lexbuf }
  | _     { comment lexbuf }

and read_string buffer = parse
  | '"'           { Lstr (Buffer.contents buffer) }
  | '\\' 'n'      { Buffer.add_string buffer "\\n"; read_string buffer lexbuf }
  | [^ '"' '\\']+ { Buffer.add_string buffer (Lexing.lexeme lexbuf)
                  ; read_string buffer lexbuf
                  }
  | _ as c        { raise (LexerErrorC c) }
  | eof           { raise (LexerErrorS "String is not terminated") }
