open Ast

module Env = Map.Make(String)

let _types_ = Env.empty

let builtins = []
