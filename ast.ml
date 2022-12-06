module Syntax = struct
  type expr =
    | Int of { value: int
             ; pos: Lexing.position }
end

module IR = struct
  type expr =
    | Int of int
end
