open Ast
open Ast.IR
open Errors

let debug_parser oc parsed =
  let rec fmt_v = function
    | Syntax.Void -> "Void"
    | Syntax.Int d -> "Int " ^ string_of_int d
    | Syntax.Bool d -> "Bool " ^ string_of_bool d
  and fmt_e = function
    | Syntax.Val d -> "Val (" ^ fmt_v d.value ^ ")"
    | Syntax.Var d -> "Var \"" ^ d.name ^ "\""
    | Syntax.Call d ->
      "Call (\"" ^ d.func ^ "\", [ " ^ String.concat " ; " (List.map fmt_e d.args) ^ " ])"
  and fmt_i = function
    | Syntax.Decl d -> "Decl(" ^ string_of_type_t d.type_t ^ ") \"" ^ d.name ^ "\""
    | Syntax.Assign d -> "Assign (\"" ^ d.var ^ "\", " ^ fmt_e d.expr ^ ")"
    | Syntax.Do d -> "Do (" ^ fmt_e d.expr ^ ")"
    | Syntax.Return d -> "Return (" ^ fmt_e d.expr ^ ")"
  and fmt_b b = "[ " ^ String.concat "\n; " (List.map fmt_i b) ^ " ]"
  and fmt_f = function
    | Syntax.Func d ->
      "Func ( "
      ^ string_of_type_t d.type_t
      ^ ", \""
      ^ d.func
      ^ ", ["
      ^ String.concat
          "\n; "
          (List.map
             (fun a ->
               match a with
               | Syntax.Arg a -> " (" ^ string_of_type_t a.type_t ^ ")" ^ a.name ^ " ")
             d.args)
      ^ "], ["
      ^ fmt_b d.code
      ^ "])\n"
  and fmt_p p = "[ " ^ String.concat "\n; " (List.map fmt_f p) ^ "]" in
  Printf.fprintf oc "%s\n" (fmt_p parsed)
;;

let debug_semantics oc ast =
  let rec fmt_v = function
    | Void -> "Void"
    | Int n -> "Int " ^ string_of_int n
    | Bool b -> "Bool " ^ string_of_bool b
  and fmt_e = function
    | Val v -> "Val (" ^ fmt_v v ^ ")"
    | Var v -> "Var \"" ^ v ^ "\""
    | Call (f, a) ->
      "Call (\"" ^ f ^ "\", [ " ^ String.concat " ; " (List.map fmt_e a) ^ " ])"
  and fmt_i = function
    | Decl v -> "Decl \"" ^ v ^ "\""
    | Assign (v, e) -> "Assign (\"" ^ v ^ "\", " ^ fmt_e e ^ ")"
    | Do e -> "Do (" ^ fmt_e e ^ ")"
    | Return e -> "Return (" ^ fmt_e e ^ ")"
  and fmt_b b = "[ " ^ String.concat "\n; " (List.map fmt_i b) ^ " ]" in
  Printf.fprintf oc "%s\n" (fmt_b ast)
;;
