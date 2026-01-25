open Syntax
open Parser

module TypingEnvironment = Map.Make(String)

let typechecker