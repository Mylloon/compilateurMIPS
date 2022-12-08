module Syntax = struct
  type expr =
    | Int of
        { value : int
        ; pos : Lexing.position
        }
end

module IR = struct
  type value = Int of int
  type expr = Val of value
end
