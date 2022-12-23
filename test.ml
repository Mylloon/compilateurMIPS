open Ast
open Ast.IR1
open Ast.V1
open Errors

let debug_parser oc parsed =
  let rec fmt_v = function
    | Syntax.Void -> "Void"
    | Syntax.Int d -> "Int " ^ string_of_int d
    | Syntax.Bool d -> "Bool " ^ string_of_bool d
    | Syntax.Str s -> "Str \"" ^ s ^ "\""
  and fmt_e = function
    | Syntax.Val d -> "Val (" ^ fmt_v d.value ^ ")"
    | Syntax.Var d -> "Var \"" ^ d.name ^ "\""
    | Syntax.Call d ->
      "Call (\"" ^ d.func ^ "\", [ " ^ String.concat " ; " (List.map fmt_e d.args) ^ " ])"
  and fmt_i = function
    | Syntax.Decl d -> "Decl(" ^ string_of_type_t d.type_t ^ ") \"" ^ d.name ^ "\""
    | Syntax.Assign d -> "Assign (\"" ^ d.var ^ "\", " ^ fmt_e d.expr ^ ")"
    | Syntax.Do d -> "Do (" ^ fmt_e d.expr ^ ")"
    | Syntax.Cond c ->
      "Cond (" ^ fmt_e c.expr ^ ", " ^ fmt_b c.if_b ^ ", " ^ fmt_b c.else_b ^ ")"
    | Syntax.Loop l -> "Loop (" ^ fmt_e l.expr ^ ", " ^ fmt_b l.block ^ ")"
    | Syntax.Return d -> "Return (" ^ fmt_e d.expr ^ ")"
  and fmt_b b = " [ " ^ String.concat "\n ; " (List.map fmt_i b) ^ "\n ]"
  and fmt_f = function
    | Syntax.Func d ->
      let s = if List.length d.args > 0 then " " else "" in
      "Func ( "
      ^ string_of_type_t d.type_t
      ^ ", \""
      ^ d.func
      ^ "\", ["
      ^ s
      ^ String.concat
          ", "
          (List.map
             (fun a ->
               match a with
               | Syntax.Arg a -> "(" ^ string_of_type_t a.type_t ^ ")" ^ a.name ^ "")
             d.args)
      ^ s
      ^ "], [\n"
      ^ fmt_b d.code
      ^ "])"
  and fmt_p p = "[ " ^ String.concat "\n; " (List.map fmt_f p) ^ "\n]" in
  Printf.fprintf oc "%s\n" (fmt_p parsed)
;;

let debug_semantics oc ast =
  let rec fmt_v = function
    | Void -> "Void"
    | Int n -> "Int " ^ string_of_int n
    | Bool b -> "Bool " ^ string_of_bool b
    | Str s -> "Str \"" ^ s ^ "\""
  and fmt_e = function
    | Val v -> "Val (" ^ fmt_v v ^ ")"
    | Var v -> "Var \"" ^ v ^ "\""
    | Call (f, a) ->
      "Call (\"" ^ f ^ "\", [ " ^ String.concat " ; " (List.map fmt_e a) ^ " ])"
  and fmt_i = function
    | Decl v -> "Decl \"" ^ v ^ "\""
    | Assign (v, e) -> "Assign (\"" ^ v ^ "\", " ^ fmt_e e ^ ")"
    | Do e -> "Do (" ^ fmt_e e ^ ")"
    | Cond (c, i, e) -> "Cond (" ^ fmt_e c ^ ", " ^ fmt_b i ^ ", " ^ fmt_b e ^ ")"
    | Loop (c, b) -> "Loop (" ^ fmt_e c ^ ", " ^ fmt_b b ^ ")"
    | Return e -> "Return (" ^ fmt_e e ^ ")"
  and fmt_b b = "[ " ^ String.concat "\n ; " (List.map fmt_i b) ^ "\n ]"
  and fmt_f = function
    | Func (f, args, b) ->
      let s = if List.length args > 0 then " " else "" in
      "Func ( "
      ^ f
      ^ ", ["
      ^ s
      ^ String.concat " ; " args
      ^ s
      ^ "], [\n"
      ^ fmt_b b
      ^ "])\n"
  and fmt_p p = "[ " ^ String.concat "\n; " (List.map fmt_f p) ^ "]" in
  Printf.fprintf oc "%s\n" (fmt_p ast)
;;
