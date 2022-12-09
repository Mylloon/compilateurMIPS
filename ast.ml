type type_t =
  | Void_t
  | Int_t
  | Bool_t
  | Func_t of type_t * type_t list

module Syntax = struct
  type ident = string

  type value =
    | Void
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
    | Call of
        { func : ident
        ; args : expr list
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
    | Return of
        { expr : expr
        ; pos : Lexing.position
        }

  and block = instr list
end

module IR = struct
  type ident = string

  type value =
    | Void
    | Int of int
    | Bool of bool

  type expr =
    | Val of value
    | Var of ident
    | Call of ident * expr list

  type instr =
    | Decl of ident
    | Assign of ident * expr
    | Return of expr

  and block = instr list
end
