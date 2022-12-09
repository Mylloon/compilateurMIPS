open Ast
open Ast.IR
open Baselib

(* Erreurs *)

exception Error of string * Lexing.position

let errt expected given pos =
  let rec string_of_type_t = function
    | Int_t -> "int"
    | Bool_t -> "bool"
    | Func_t (r, a) ->
      (if List.length a > 1 then "(" else "")
      ^ String.concat ", " (List.map string_of_type_t a)
      ^ (if List.length a > 1 then ")" else "")
      ^ " -> "
      ^ string_of_type_t r
  in
  raise
    (Error
       ( Printf.sprintf
           "Expected %s but given %s"
           (string_of_type_t expected)
           (string_of_type_t given)
       , pos ))
;;

let warn msg (pos : Lexing.position) =
  Printf.eprintf
    "Warning on line %d col %d: %s.\n"
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)
    msg
;;

(* Sémantique *)

let analyze_value = function
  | Syntax.Int n -> Int n, Int_t
  | Syntax.Bool b -> Bool b, Bool_t
;;

let rec analyze_expr env ua t = function
  | Syntax.Val v ->
    let v2, new_t = analyze_value v.value in
    if new_t != t then errt t new_t v.pos;
    Val v2, new_t
  | Syntax.Var v ->
    if not (Env.mem v.name env)
    then raise (Error ("Unbound variable \"" ^ v.name ^ "\"", v.pos));
    if List.mem v.name ua then warn ("Unassigned variable \"" ^ v.name ^ "\"") v.pos;
    let new_t = Env.find v.name env in
    if new_t != t then errt t new_t v.pos;
    Var v.name, new_t
  | Syntax.Call c ->
    (match Env.find c.func env with
    | Func_t (ret_t, tl) ->
      if ret_t != t then errt t ret_t c.pos;
      ( Call
          ( c.func
          , List.map2
              (fun t e ->
                let e2, t2 = analyze_expr env ua t e in
                if t2 = t
                then e2
                else
                  errt
                    t
                    t2
                    (match e with
                    | Syntax.Val v -> v.pos
                    | Syntax.Var v -> v.pos
                    | Syntax.Call c -> c.pos))
              tl
              c.args )
      , ret_t )
    | _ -> raise (Error ("\"" ^ c.func ^ "\" isn't a function", c.pos)))
;;

let analyze_instr env ua = function
  | Syntax.Decl d -> Decl d.name, Env.add d.name d.type_t env, [ d.name ] @ ua
  | Syntax.Assign a ->
    if not (Env.mem a.var env)
    then raise (Error ("Unbound variable \"" ^ a.var ^ "\"", a.pos));
    let ae, et = analyze_expr env ua (Env.find a.var env) a.expr in
    Assign (a.var, ae), env, List.filter (fun x -> x <> a.var) ua
  | Syntax.Return r ->
    let ae, _ = analyze_expr env ua Int_t r.expr in
    Return ae, env, []
;;

let rec analyze_block env ua = function
  | [] -> []
  | instr :: new_block ->
    let new_instr, new_env, new_ua = analyze_instr env ua instr in
    new_instr :: analyze_block new_env new_ua new_block
;;

let analyze parsed = analyze_block _types_ [] parsed

let emit oc ast =
  let rec fmt_v = function
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
    | Return e -> "Return (" ^ fmt_e e ^ ")"
  and fmt_b b = "[ " ^ String.concat "\n; " (List.map fmt_i b) ^ " ]" in
  Printf.fprintf oc "%s\n" (fmt_b ast)
;;
