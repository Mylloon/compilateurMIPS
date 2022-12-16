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
       ; "%big", Func_t (Bool_t, [ Int_t; Int_t ])
       ; "%sml", Func_t (Bool_t, [ Int_t; Int_t ])
       ; "%equ", Func_t (Bool_t, [ Int_t; Int_t ])
       ; "%neq", Func_t (Bool_t, [ Int_t; Int_t ])
       ; "puti", Func_t (Void_t, [ Int_t ])
       ; "puts", Func_t (Void_t, [ Str_t ])
       ; "geti", Func_t (Int_t, [])
       ; "alloc", Func_t (Ptr_t Magic_t, [ Int_t ])
       ; "%deref", Func_t (Magic_t, [ Ptr_t Magic_t ])
       ])
;;

let builtins_special = [ "%equ"; "%neq" ]

let builtins uniq =
  List.fold_left
    (fun env (fn, impl) -> Env.add fn impl env)
    Env.empty
    [ "%add", [ Lw (T0, Mem (SP, 4)); Lw (T1, Mem (SP, 0)); Add (V0, T0, T1) ]
    ; "%sub", [ Lw (T0, Mem (SP, 4)); Lw (T1, Mem (SP, 0)); Sub (V0, T0, T1) ]
    ; "%mul", [ Lw (T0, Mem (SP, 4)); Lw (T1, Mem (SP, 0)); Mul (V0, T0, T1) ]
    ; "%div", [ Lw (T0, Mem (SP, 4)); Lw (T1, Mem (SP, 0)); Div (V0, T0, T1) ]
    ; "%big", [ Lw (T0, Mem (SP, 4)); Lw (T1, Mem (SP, 0)); Slt (V0, T0, T1) ]
    ; "%sml", [ Lw (T0, Mem (SP, 4)); Lw (T1, Mem (SP, 0)); Slt (V0, T1, T0) ]
    ; ( "%equ"
      , [ Lw (T0, Mem (SP, 0))
        ; Lw (T1, Mem (SP, 4))
        ; Beq (T0, T1, "_equ" ^ uniq)
        ; Li (V0, 0)
        ; J ("_equend" ^ uniq)
        ; Label ("_equ" ^ uniq)
        ; Li (V0, 1)
        ; Label ("_equend" ^ uniq)
        ] )
    ; ( "%neq"
      , [ Lw (T0, Mem (SP, 0))
        ; Lw (T1, Mem (SP, 4))
        ; Beq (T0, T1, "_neq" ^ uniq)
        ; Li (V0, 1)
        ; J ("_neqend" ^ uniq)
        ; Label ("_neq" ^ uniq)
        ; Li (V0, 0)
        ; Label ("_neqend" ^ uniq)
        ] )
    ; "puti", [ Lw (A0, Mem (SP, 0)); Li (V0, Syscall.print_int); Syscall ]
    ; "puts", [ Lw (A0, Mem (SP, 0)); Li (V0, Syscall.print_str); Syscall ]
    ; "geti", [ Lw (A0, Mem (SP, 0)); Li (V0, Syscall.read_int); Syscall ]
    ; "alloc", [ Lw (A0, Mem (SP, 0)); Li (V0, Syscall.sbrk); Syscall ]
    ; "%deref", [ Lw (T0, Mem (SP, 0)); Lw (A0, Mem (T0, 0)); Syscall ]
    ]
;;
