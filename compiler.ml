open Ast.IR
open Mips
module Env = Map.Make (String)

let compile_value = function
  | Int n -> [ Li (V0, n) ]
;;

let compile_expr = function
  | Val v -> compile_value v
;;

let compile ir = { text = Baselib.builtins @ compile_expr ir; data = [] }
