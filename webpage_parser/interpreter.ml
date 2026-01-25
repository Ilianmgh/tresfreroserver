open Lexic
open Syntax
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

(** Pretty-printing *)

let rec string_of_value (v1 : value) : string = match v1 with
  | VInt n -> Printf.sprintf "%d" n
  | VBool b -> if b then "true" else "false"
  | VString f -> f
  | VContent h -> h
  | VCouple (v, v') -> Printf.sprintf "(%s, %s)" (string_of_value v) (string_of_value v')
  | Clos (_, VFst) -> Printf.sprintf "⟨∅, fst⟩"
  | Clos (_, VSnd) -> Printf.sprintf "⟨∅, snd⟩"
  | Clos (env, VFun (x, e)) -> Printf.sprintf "⟨%s, fun %s -> %s⟩" (string_of_env env) x (string_of_expr e)
  | Clos (env, VFix (f, x, e)) -> Printf.sprintf "⟨%s, fixfun %s %s -> %s⟩" (string_of_env env) f x (string_of_expr e)
and string_of_env (env : environment) : string =
  let string_of_one_env_binding (x : variable) (v : value) (acc : string) : string =
    if acc = "" then
      Printf.sprintf "%s ↦ %s" x (string_of_value v)
    else
      Printf.sprintf "%s ↦ %s, %s" x (string_of_value v) acc
  in
  StringMap.fold string_of_one_env_binding env ""

let rec ( ^^ ) (n : int) (p : int) = match p with
  | 0 -> 1
  | 1 -> n
  | p -> let p', r = p/2, p mod 2 in
    let np' = n ^^ p' in
    np' * np' + if r = 1 then n else 0

let drop = fun x -> ()

let rec eval (env : environment) (e1 : expr) : value = match e1 with
  | Empty -> assert false
  | Let (x, e, e') -> let v = eval env e in eval (StringMap.add x v env) e'
  | Fun (x, e) -> Clos (env, VFun (x, e))
  | Fix (f, x, e) -> Clos (env, VFix (f, x, e))
  | App (e, e') -> begin match eval env e with
    | Clos (_, VFst) -> begin match eval env e' with
      | VCouple (v, v') -> v
      | _ -> raise (InterpreterError (Printf.sprintf "%s: pair expected." (string_of_expr e)))
    end
    | Clos (_, VSnd) -> begin match eval env e' with
      | VCouple (v, v') -> v'
      | _ -> raise (InterpreterError (Printf.sprintf "%s: pair expected." (string_of_expr e)))
    end
    | Clos (env', VFun (x, e_f)) -> let v = eval env e' in eval (StringMap.add x v env') e_f
    | Clos (env', VFix (f, x, e_f)) -> let v = eval env e' in
      let env'_x = StringMap.add x v env' in
      let env'_f_x = StringMap.add f (Clos (env', VFix (f, x, e_f))) env'_x in
      eval env'_f_x e_f
    | _ -> raise (InterpreterError (Printf.sprintf "%s: it is not a function, it cannot be applied." (string_of_expr e)))
  end
  | If (c, t, e) -> begin match eval env c with
    | VBool b -> if b then eval env t else eval env e
    | _ -> raise (InterpreterError (Printf.sprintf "%s: boolean expected" (string_of_expr c)))
  end
  | Seq (e, e') ->
    let v = eval env e in
    let v' = eval env e' in
    drop v;
    v'
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
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Minus (e, e')))))
  end
  | Neg e -> begin match eval env e with
    | VInt n -> VInt (-n)
    | _ -> raise (InterpreterError (Printf.sprintf "%s: Integer expected." (string_of_expr (Neg e))))
  end
  | Mult (e, e') -> begin match eval env e, eval env e' with
    | VInt n, VInt m -> VInt (n * m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Mult (e, e')))))
  end
  | Div (e, e') -> begin match eval env e, eval env e' with
    | VInt n, VInt m -> VInt (n / m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Div (e, e')))))
  end
  | Pow (e, e') -> begin match eval env e, eval env e' with
    | VInt n, VInt m -> VInt (n ^^ m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Pow (e, e')))))
  end
  | Int n -> VInt n
  | Gt (e, e') -> begin match eval env e, eval env e' with
    | VInt v, VInt v' -> VBool (v > v')
    | VContent v, VContent v' -> VBool (v > v')
    | VString v, VString v' -> VBool (v > v')
    | VBool v, VBool v' -> VBool (v > v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Cannot compare functions." (string_of_expr (Gt (e, e')))))
  end
  | Lt (e, e') -> begin match eval env e, eval env e' with
    | VInt v, VInt v' -> VBool (v < v')
    | VContent v, VContent v' -> VBool (v < v')
    | VString v, VString v' -> VBool (v < v')
    | VBool v, VBool v' -> VBool (v < v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Cannot compare functions." (string_of_expr (Lt (e, e')))))
  end
  | Geq (e, e') -> begin match eval env e, eval env e' with
    | VInt v, VInt v' -> VBool (v >= v')
    | VContent v, VContent v' -> VBool (v >= v')
    | VString v, VString v' -> VBool (v >= v')
    | VBool v, VBool v' -> VBool (v >= v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Cannot compare functions." (string_of_expr (Geq (e, e')))))
  end
  | Leq (e, e') -> begin match eval env e, eval env e' with
    | VInt v, VInt v' -> VBool (v <= v')
    | VContent v, VContent v' -> VBool (v <= v')
    | VString v, VString v' -> VBool (v <= v')
    | VBool v, VBool v' -> VBool (v <= v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Cannot compare functions." (string_of_expr (Leq (e, e')))))
  end
  | Eq (e, e') -> begin match eval env e, eval env e' with
    | VInt v, VInt v' -> VBool (v = v')
    | VContent v, VContent v' -> VBool (v = v')
    | VString v, VString v' -> VBool (v = v')
    | VBool v, VBool v' -> VBool (v = v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Cannot compare functions." (string_of_expr (Eq (e, e')))))
  end
  | Neq (e, e') -> begin match eval env e, eval env e' with
    | VInt v, VInt v' -> VBool (v <> v')
    | VContent v, VContent v' -> VBool (v <> v')
    | VString v, VString v' -> VBool (v <> v')
    | VBool v, VBool v' -> VBool (v <> v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Cannot compare functions." (string_of_expr (Neq (e, e')))))
  end
  | And (e, e') -> begin match eval env e, eval env e' with
    | VBool b, VBool b' -> VBool (b && b')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Booleans expected." (string_of_expr (And (e, e')))))
  end
  | Or (e, e') -> begin match eval env e, eval env e' with
    | VBool b, VBool b' -> VBool (b || b')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Booleans expected." (string_of_expr (And (e, e')))))
  end
  | Not e -> begin match eval env e with
    | VBool b -> VBool (not b)
    | _ -> raise (InterpreterError (Printf.sprintf "%s: Integer expected." (string_of_expr (Neg e))))
  end
  | Bool b -> VBool b
  | Concat (e, e') -> begin match eval env e, eval env e' with
    | VInt n, VInt m -> VInt (n ^^ m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Strings expected." (string_of_expr (Concat (e, e')))))
  end
  | String s -> VString s
  | Fstring s -> failwith "TODO"

(* TODO add "garbage-collection" *)