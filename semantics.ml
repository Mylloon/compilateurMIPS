open Ast
open Ast.IR
open Baselib

exception Error of string * Lexing.position

let rec analyze_expr expr env =
  match expr with
  | Syntax.Int n -> Int n.value

let analyze parsed =
  analyze_expr parsed Baselib._types_
