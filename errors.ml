open Ast
open Lexing

exception LexerErrorC of char
exception LexerErrorS of string
exception SemanticsError of string * Lexing.position

let err msg pos =
  Printf.eprintf
    "Error on line %d col %d: %s.\n"
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)
    msg;
  exit 1
;;

let rec string_of_type_t = function
  | Magic_t -> "magic"
  | Void_t -> "void"
  | Int_t -> "int"
  | Bool_t -> "bool"
  | Str_t -> "str"
  | Func_t (r, a) ->
    (if List.length a > 1 then "(" else "")
    ^ String.concat ", " (List.map string_of_type_t a)
    ^ (if List.length a > 1 then ")" else "")
    ^ " -> "
    ^ string_of_type_t r
;;

let errt expected given pos =
  raise
    (SemanticsError
       ( Printf.sprintf
           "Expected %s but given %s"
           (String.concat ", " (List.map string_of_type_t expected))
           (String.concat ", " (List.map string_of_type_t given))
       , pos ))
;;

let warn msg (pos : Lexing.position) =
  Printf.eprintf
    "Warning on line %d col %d: %s.\n"
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)
    msg
;;
