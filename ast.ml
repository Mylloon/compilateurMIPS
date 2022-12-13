type type_t =
  | Magic_t
  | Void_t
  | Int_t
  | Bool_t
  | Str_t
  | Func_t of type_t * type_t list

module Syntax = struct
  type ident = string

  type value =
    | Void
    | Int of int
    | Bool of bool
    | Str of string

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
    | Do of
        { expr : expr
        ; pos : Lexing.position
        }
    | Cond of
        { expr : expr
        ; if_b : block
        ; else_b : block
        ; pos : Lexing.position
        }
    | Loop of
        { expr : expr
        ; block : block
        ; pos : Lexing.position
        }
    | Return of
        { expr : expr
        ; pos : Lexing.position
        }

  and block = instr list

  type arg =
    | Arg of
        { type_t : type_t
        ; name : ident
        }

  type args = arg list

  type def =
    | Func of
        { func : ident
        ; type_t : type_t
        ; args : args
        ; code : block
        ; pos : Lexing.position
        }

  type prog = def list
end

module type Parameters = sig
  type value
end

module V1 = struct
  type value =
    | Void
    | Int of int
    | Bool of bool
    | Str of string
end

module V2 = struct
  type value =
    | Void
    | Int of int
    | Bool of bool
    | Data of string
end

module IR (P : Parameters) = struct
  type ident = string

  type expr =
    | Val of P.value
    | Var of ident
    | Call of ident * expr list

  type instr =
    | Decl of ident
    | Assign of ident * expr
    | Do of expr
    | Cond of expr * block * block
    | Loop of expr * block
    | Return of expr

  and block = instr list

  type def = Func of ident * ident list * block
  type prog = def list
end

module IR1 = IR (V1)
module IR2 = IR (V2)
