open Syntax
open Parser

type raw_function_value =
  | VFun of variable * expr
  | VFix of variable * variable * expr

type value =
  | Clos of environment * raw_function_value
  | VInt of int
  | VBool of bool
  | VString of string
  | VContent of string
and environment = variable -> value

let eval (e : expr) : value = failwith "TODO"