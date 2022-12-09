open Ast
open Mips
module Env = Map.Make (String)

let _types_ =
  Env.of_seq
    (List.to_seq
       [ "%add", Func_t (Int_t, [ Int_t; Int_t ])
       ; "%sub", Func_t (Int_t, [ Int_t; Int_t ])
       ; "%mul", Func_t (Int_t, [ Int_t; Int_t ])
       ; "%div", Func_t (Int_t, [ Int_t; Int_t ])
       ])
;;

let builtins =
  List.fold_left
    (fun env (fn, impl) -> Env.add fn impl env)
    Env.empty
    [ "%add", [ Lw (T0, Mem (SP, 4)); Lw (T1, Mem (SP, 0)); Add (V0, T0, T1) ]
    ; "%sub", [ Lw (T0, Mem (SP, 4)); Lw (T1, Mem (SP, 0)); Sub (V0, T0, T1) ]
    ; "%mul", [ Lw (T0, Mem (SP, 4)); Lw (T1, Mem (SP, 0)); Mul (V0, T0, T1) ]
    ; "%div", [ Lw (T0, Mem (SP, 4)); Lw (T1, Mem (SP, 0)); Div (V0, T0, T1) ]
    ]
;;
