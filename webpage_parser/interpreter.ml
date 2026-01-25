open Lexic
open Syntax
open Parser
open Typechecker

exception InterpreterError of string

type raw_function_value =
  | VFst
  | VSnd
  | VFun of variable * expr
  | VFix of variable * variable * expr

type value =
  | Clos of environment * raw_function_value
  | VInt of int
  | VBool of bool
  | VString of string
  | VContent of string
  | VCouple of value * value
and environment = value StringMap.t

let rec ( ^^ ) (n : int) (p : int) = match p with
  | 0 -> 1
  | 1 -> n
  | p -> let p', r = p/2, p mod 2 in
    let np' = n ^^ p' in
    np' * np' + if r = 1 then n else 0

let rec eval (env : environment) (e1 : expr) : value = match e1 with
  | Empty -> assert false
  | Let (x, e, e') -> failwith "TODO"
  | Fun (x, e) -> failwith "TODO"
  | Fix (f, x, e) -> failwith "TODO"
  | App (e, e') -> failwith "TODO"
  | If (c, t, e) -> failwith "TODO"
  | Seq (e, e') -> failwith "TODO"
  | Html h -> VContent h
  | Var x -> begin match StringMap.find_opt x env with
    | Some v -> v
    | None -> raise (InterpreterError (Printf.sprintf "%s: Undefined variable" x))
  end
  | Couple (e, e') ->
    let v = eval env e in
    let v' = eval env e' in
    VCouple (v, v')
  | Fst -> Clos (StringMap.empty, VFst)
  | Snd -> Clos (StringMap.empty, VSnd)
  | Plus (e, e') -> begin match eval env e, eval env e' with
    | VInt n, VInt m -> VInt (n + m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Plus (e, e')))))
  end
  | Minus (e, e') -> begin match eval env e, eval env e' with
    | VInt n, VInt m -> VInt (n - m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Plus (e, e')))))
  end
  | Neg e -> begin match eval env e with
    | VInt n -> VInt (-n)
    | _ -> raise (InterpreterError (Printf.sprintf "%s: Integer expected." (string_of_expr (Neg e))))
  end
  | Mult (e, e') -> begin match eval env e, eval env e' with
    | VInt n, VInt m -> VInt (n * m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Plus (e, e')))))
  end
  | Div (e, e') -> begin match eval env e, eval env e' with
    | VInt n, VInt m -> VInt (n / m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Plus (e, e')))))
  end
  | Pow (e, e') -> begin match eval env e, eval env e' with
    | VInt n, VInt m -> VInt (n ^^ m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Plus (e, e')))))
  end
  | Int n -> VInt n
  | Gt (e, e') -> begin match eval env e, eval env e' with
    | VInt v, VInt v' -> VBool (v > v')
    | VContent v, VContent v' -> VBool (v > v')
    | VString v, VString v' -> VBool (v > v')
    | VBool v, VBool v' -> VBool (v > v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Cannot compare functions." (string_of_expr (Plus (e, e')))))
  end
  | Lt (e, e') -> begin match eval env e, eval env e' with
    | VInt v, VInt v' -> VBool (v < v')
    | VContent v, VContent v' -> VBool (v < v')
    | VString v, VString v' -> VBool (v < v')
    | VBool v, VBool v' -> VBool (v < v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Cannot compare functions." (string_of_expr (Plus (e, e')))))
  end
  | Geq (e, e') -> begin match eval env e, eval env e' with
    | VInt v, VInt v' -> VBool (v >= v')
    | VContent v, VContent v' -> VBool (v >= v')
    | VString v, VString v' -> VBool (v >= v')
    | VBool v, VBool v' -> VBool (v >= v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Cannot compare functions." (string_of_expr (Plus (e, e')))))
  end
  | Leq (e, e') -> begin match eval env e, eval env e' with
    | VInt v, VInt v' -> VBool (v <= v')
    | VContent v, VContent v' -> VBool (v <= v')
    | VString v, VString v' -> VBool (v <= v')
    | VBool v, VBool v' -> VBool (v <= v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Cannot compare functions." (string_of_expr (Plus (e, e')))))
  end
  | Eq (e, e') -> begin match eval env e, eval env e' with
    | VInt v, VInt v' -> VBool (v = v')
    | VContent v, VContent v' -> VBool (v = v')
    | VString v, VString v' -> VBool (v = v')
    | VBool v, VBool v' -> VBool (v = v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Cannot compare functions." (string_of_expr (Plus (e, e')))))
  end
  | Neq (e, e') -> begin match eval env e, eval env e' with
    | VInt v, VInt v' -> VBool (v <> v')
    | VContent v, VContent v' -> VBool (v <> v')
    | VString v, VString v' -> VBool (v <> v')
    | VBool v, VBool v' -> VBool (v <> v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Cannot compare functions." (string_of_expr (Plus (e, e')))))
  end
  | And (e, e') -> begin match eval env e, eval env e' with
    | VInt n, VInt m -> VInt (n ^^ m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Plus (e, e')))))
  end
  | Or (e, e') -> begin match eval env e, eval env e' with
    | VInt n, VInt m -> VInt (n ^^ m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Plus (e, e')))))
  end
  | Not e -> begin match eval env e with
    | VInt n -> VInt (-n)
    | _ -> raise (InterpreterError (Printf.sprintf "%s: Integer expected." (string_of_expr (Neg e))))
  end
  | Bool b -> VBool b
  | Concat (e, e') -> begin match eval env e, eval env e' with
    | VInt n, VInt m -> VInt (n ^^ m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Plus (e, e')))))
  end
  | String s -> VString s
  | Fstring s -> failwith "TODO"

(* TODO add "garbage-collection" *)