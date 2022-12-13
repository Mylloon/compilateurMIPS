open Ast
open Ast.IR1
open Ast.V1
open Baselib
open Errors

let analyze_value = function
  | Syntax.Void -> Void, Void_t
  | Syntax.Int n -> Int n, Int_t
  | Syntax.Bool b -> Bool b, Bool_t
  | Syntax.Str s -> Str s, Str_t
;;

let rec analyze_expr env ua t = function
  | Syntax.Val v ->
    let v2, new_t = analyze_value v.value in
    if new_t != t && t != Magic_t then errt t new_t v.pos;
    Val v2, new_t
  | Syntax.Var v ->
    if not (Env.mem v.name env)
    then raise (SemanticsError ("Unbound variable \"" ^ v.name ^ "\"", v.pos));
    if List.mem v.name ua then warn ("Unassigned variable \"" ^ v.name ^ "\"") v.pos;
    let new_t = Env.find v.name env in
    if new_t != t then errt t new_t v.pos;
    Var v.name, new_t
  | Syntax.Call c ->
    (match Env.find c.func env with
    | Func_t (ret_t, tl) ->
      if ret_t != t && t != Magic_t then errt ret_t t c.pos;
      if List.length tl != List.length c.args
      then
        raise
          (SemanticsError
             ( Printf.sprintf
                 "Function \"%s\" expects %d arguments but %d was given"
                 c.func
                 (List.length tl)
                 (List.length c.args)
             , c.pos ));
      let args =
        List.map2
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
          c.args
      in
      Call (c.func, args), ret_t
    | _ -> raise (SemanticsError ("\"" ^ c.func ^ "\" isn't a function", c.pos)))
;;

let rec analyze_instr env ua ret_t = function
  | Syntax.Decl d -> Decl d.name, Env.add d.name d.type_t env, [ d.name ] @ ua
  | Syntax.Assign a ->
    if not (Env.mem a.var env)
    then raise (SemanticsError ("Unbound variable \"" ^ a.var ^ "\"", a.pos));
    let ae, et = analyze_expr env ua (Env.find a.var env) a.expr in
    Assign (a.var, ae), env, List.filter (fun x -> x <> a.var) ua
  | Syntax.Do d ->
    let ae, _ = analyze_expr env ua Magic_t d.expr in
    Do ae, env, []
  | Syntax.Cond c ->
    let cond, _ = analyze_expr env ua Bool_t c.expr in
    let if_b, _ = analyze_block env ua Magic_t c.pos c.if_b in
    let else_b, _ = analyze_block env ua Magic_t c.pos c.else_b in
    Cond (cond, if_b, else_b), env, []
  | Syntax.Loop l ->
    let cond, _ = analyze_expr env ua Bool_t l.expr in
    let block, _ = analyze_block env ua Magic_t l.pos l.block in
    Loop (cond, block), env, []
  | Syntax.Return r ->
    let ae, _ = analyze_expr env ua ret_t r.expr in
    Return ae, env, []

and analyze_block env ua ret_t pos = function
  | [] ->
    if ret_t != Void_t && ret_t != Magic_t
    then warn "Non-void function without return" pos;
    [], ua
  | instr :: new_block ->
    let new_instr, new_env, ua1 = analyze_instr env ua ret_t instr in
    (match new_instr with
    | Return _ -> [ new_instr ], ua1
    | _ ->
      let new_block, ua2 = analyze_block new_env ua1 ret_t pos new_block in
      new_instr :: new_block, ua2)
;;

let analyze_func env ua = function
  | Syntax.Func f ->
    let rec add_args env = function
      | [] -> env
      | h :: t ->
        (match h with
        | Syntax.Arg a -> add_args (Env.add a.name a.type_t env) t)
    in
    let block, _ = analyze_block (add_args env f.args) ua f.type_t f.pos f.code in
    ( Func
        ( f.func
        , List.map
            (fun a ->
              match a with
              | Syntax.Arg a -> a.name)
            f.args
        , block )
    , Env.add
        f.func
        (Func_t
           ( f.type_t
           , List.map
               (fun a ->
                 match a with
                 | Syntax.Arg a -> a.type_t)
               f.args ))
        env )
;;

let rec analyze_prog env ua b default = function
  | [] ->
    if b
    then []
    else raise (SemanticsError ("No " ^ default ^ " function", Lexing.dummy_pos))
  | fn :: suite ->
    let fn, new_env = analyze_func env ua fn in
    fn :: analyze_prog new_env ua (if b then b else Env.mem default new_env) default suite
;;

let analyze parsed = analyze_prog _types_ [] false "main" parsed
