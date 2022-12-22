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
       ; "%rem", Func_t (Int_t, [ Int_t; Int_t ])
       ; "%seq", Func_t (Bool_t, [ Int_t; Int_t ])
       ; "%sge", Func_t (Bool_t, [ Int_t; Int_t ])
       ; "%sgt", Func_t (Bool_t, [ Int_t; Int_t ])
       ; "%sle", Func_t (Bool_t, [ Int_t; Int_t ])
       ; "%slt", Func_t (Bool_t, [ Int_t; Int_t ])
       ; "%sne", Func_t (Bool_t, [ Int_t; Int_t ])
       ; "%and", Func_t (Int_t, [ Int_t; Int_t ])
       ; "%or", Func_t (Int_t, [ Int_t; Int_t ])
       ; "puti", Func_t (Void_t, [ Int_t ])
       ; "puts", Func_t (Void_t, [ Str_t ])
       ; "geti", Func_t (Int_t, [])
       ; "abs", Func_t (Int_t, [ Int_t ])
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
    ; "%rem", [ Lw (T0, Mem (SP, 4)); Lw (T1, Mem (SP, 0)); Rem (V0, T0, T1) ]
    ; "%seq", [ Lw (T0, Mem (SP, 4)); Lw (T1, Mem (SP, 0)); Seq (V0, T0, T1) ]
    ; "%sge", [ Lw (T0, Mem (SP, 4)); Lw (T1, Mem (SP, 0)); Sge (V0, T0, T1) ]
    ; "%sgt", [ Lw (T0, Mem (SP, 4)); Lw (T1, Mem (SP, 0)); Sgt (V0, T0, T1) ]
    ; "%sle", [ Lw (T0, Mem (SP, 4)); Lw (T1, Mem (SP, 0)); Sle (V0, T0, T1) ]
    ; "%slt", [ Lw (T0, Mem (SP, 4)); Lw (T1, Mem (SP, 0)); Slt (V0, T0, T1) ]
    ; "%sne", [ Lw (T0, Mem (SP, 4)); Lw (T1, Mem (SP, 0)); Sne (V0, T0, T1) ]
    ; "%and", [ Lw (T0, Mem (SP, 4)); Lw (T1, Mem (SP, 0)); And (V0, T0, T1) ]
    ; "%or", [ Lw (T0, Mem (SP, 4)); Lw (T1, Mem (SP, 0)); Or (V0, T0, T1) ]
    ; "puti", [ Lw (A0, Mem (SP, 0)); Li (V0, Syscall.print_int); Syscall ]
    ; "puts", [ Lw (A0, Mem (SP, 0)); Li (V0, Syscall.print_str); Syscall ]
    ; "geti", [ Lw (A0, Mem (SP, 0)); Li (V0, Syscall.read_int); Syscall ]
    ; "abs", [ Lw (T0, Mem (SP, 0)); Abs (V0, T0) ]
    ]
;;
