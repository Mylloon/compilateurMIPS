type type_t =
  | Int_t
  | Bool_t

module Syntax = struct
  type ident = string

  type value =
    | Int of int
    | Bool of bool

  type expr =
    | Val of
        { value : value
        ; pos : Lexing.position
        }
    | Var of
        { name : ident
        ; pos : Lexing.position
        }

  type instr =
    | Decl of
        { name : ident
        ; type_t : type_t
        ; pos : Lexing.position
        }
    | Assign of
        { var : ident
        ; expr : expr
        ; pos : Lexing.position
        }

  and block = instr list
end

module IR = struct
  type ident = string

  type value =
    | Int of int
    | Bool of bool

  type expr =
    | Val of value
    | Var of ident

  type instr =
    | Decl of ident
    | Assign of ident * expr

  and block = instr list
end
