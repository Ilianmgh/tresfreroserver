open Utils
open Lexic
open Syntax
open Typechecker
include Value (* TODO open and add each open Value to each file that needs it *)

exception InterpreterError of string

let rec ( ^^ ) (n : int) (p : int) = match p with
  | 0 -> 1
  | 1 -> n
  | p -> let p', r = p/2, p mod 2 in
    let np' = n ^^ p' in
    np' * np' + if r = 1 then n else 0

let drop = fun x -> ()

let combine_array (a : 'a array) (b : 'b array) : ('a * 'b) array =
  if Array.length a <> Array.length b then
    raise (Invalid_argument "Cannot combine arrays of different lengths")
  else
    Array.init (Array.length a) (fun i -> (a.(i), b.(i)))

let value_of_query (db : Sqlite3.db) (combine_lines_into_table : value -> value -> value) (combine_cells_into_line : value -> string -> string -> value) (query : string) : value =
  let value_acc = ref (VPure "") in
  let value_of_query_res (value_acc : value ref) (combine_cells_into_line : value -> string -> string -> value) (line : Sqlite3.row) (hdrs : Sqlite3.headers) : unit =
    let fold_line_to_value (value_acc : value) ((hdr, content) : Sqlite3.header * string option) : value = match content with
      | None -> combine_cells_into_line value_acc hdr "NULL"
      | Some data ->  combine_cells_into_line value_acc hdr data
    in
    value_acc := combine_lines_into_table !value_acc (Array.fold_left fold_line_to_value (VPure "") (combine_array hdrs line))
  in
  let exec_code = Sqlite3.exec db ~cb:(value_of_query_res value_acc combine_cells_into_line) query in
  match exec_code with
    | OK -> !value_acc
    | _ ->  raise (InterpreterError (Printf.sprintf "SQL query \"%s\" failed: %s" query (Sqlite3.Rc.to_string exec_code)))

(** [eval_expr env e1 = v] where [v] is the evaluation of expression [e] following the program semantics (cf documentation). FIXME TO IMPLEMENT SESSION/COOKIES VARIABLES, BUT BETTER, maybe return the environment to retrieve sessions (and at some point, cookies) variables. maybe return only the interesting environment e.g. the sub environment Session and Cookie, not all the local variables. *)
let rec eval_expr (env : environment) (e1 : expr) : value = match e1 with
  | Empty -> assert false
  | Let (x, e, e') -> let v = eval_expr env e in eval_expr (Environment.add x v env) e'
  | Fun (x, e) -> Clos (env, VFun (x, e))
  | Fix (f, x, e) -> Clos (env, VFix (f, x, e))
  | App (e, e') -> begin match eval_expr env e with
    | Clos (_, VExternFunction (_, Args1 f)) -> f (eval_expr env e')
    | Clos (_, VExternFunction (name, Args2 f)) -> Clos (Environment.empty, VExternFunction (name, Args1 (f (eval_expr env e'))))
    | Clos (_, VExternFunction (name, Args3 f)) -> Clos (Environment.empty, VExternFunction (name, Args2 (f (eval_expr env e'))))
    | Clos (_, VExternFunction (name, Args4 f)) -> Clos (Environment.empty, VExternFunction (name, Args3 (f (eval_expr env e'))))
    | Clos (env', VFun (x, e_f)) -> let v = eval_expr env e' in eval_expr (Environment.add x v env') e_f
    | Clos (env', VFix (f, x, e_f)) -> let v = eval_expr env e' in
      let env'_x = Environment.add x v env' in
      let env'_f_x = Environment.add f (Clos (env', VFix (f, x, e_f))) env'_x in
      eval_expr env'_f_x e_f
    | _ -> raise (InterpreterError (Printf.sprintf "%s: it is not a function, it cannot be applied." (string_of_expr e)))
  end
  | If (c, t, e) -> begin match eval_expr env c with
    | VBool b -> if b then eval_expr env t else eval_expr env e
    | _ -> raise (InterpreterError (Printf.sprintf "%s: boolean expected" (string_of_expr c)))
  end
  | Seq (e, e') ->
    let v = eval_expr env e in
    let v' = eval_expr env e' in
    drop v;
    v'
  | Html lst_e -> VContent (snd (eval env lst_e)) (* FIXME change here too cf comment in [eval] *)
  | Var x -> begin match Environment.find_opt x env with
    | Some v -> v
    | None -> raise (InterpreterError (Printf.sprintf "%s: Undefined variable" x))
  end
  | Couple (e, e') ->
    let v = eval_expr env e in
    let v' = eval_expr env e' in
    VCouple (v, v')
  | Plus (e, e') -> begin match eval_expr env e, eval_expr env e' with
    | VInt n, VInt m -> VInt (n + m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Plus (e, e')))))
  end
  | Minus (e, e') -> begin match eval_expr env e, eval_expr env e' with
    | VInt n, VInt m -> VInt (n - m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Minus (e, e')))))
  end
  | Neg e -> begin match eval_expr env e with
    | VInt n -> VInt (-n)
    | _ -> raise (InterpreterError (Printf.sprintf "%s: Integer expected." (string_of_expr (Neg e))))
  end
  | Mult (e, e') -> begin match eval_expr env e, eval_expr env e' with
    | VInt n, VInt m -> VInt (n * m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Mult (e, e')))))
  end
  | Div (e, e') -> begin match eval_expr env e, eval_expr env e' with
    | VInt n, VInt m -> VInt (n / m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Div (e, e')))))
  end
  | Pow (e, e') -> begin match eval_expr env e, eval_expr env e' with
    | VInt n, VInt m -> VInt (n ^^ m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Pow (e, e')))))
  end
  | Int n -> VInt n
  | Gt (e, e') -> begin match eval_expr env e, eval_expr env e' with
    | VInt v, VInt v' -> VBool (v > v')
    | VContent v, VContent v' -> VBool (v > v')
    | VString v, VString v' -> VBool (v > v')
    | VBool v, VBool v' -> VBool (v > v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Uncomparable." (string_of_expr (Gt (e, e')))))
  end
  | Lt (e, e') -> begin match eval_expr env e, eval_expr env e' with
    | VInt v, VInt v' -> VBool (v < v')
    | VContent v, VContent v' -> VBool (v < v')
    | VString v, VString v' -> VBool (v < v')
    | VBool v, VBool v' -> VBool (v < v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Uncomparable." (string_of_expr (Lt (e, e')))))
  end
  | Geq (e, e') -> begin match eval_expr env e, eval_expr env e' with
    | VInt v, VInt v' -> VBool (v >= v')
    | VContent v, VContent v' -> VBool (v >= v')
    | VString v, VString v' -> VBool (v >= v')
    | VBool v, VBool v' -> VBool (v >= v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Uncomparable." (string_of_expr (Geq (e, e')))))
  end
  | Leq (e, e') -> begin match eval_expr env e, eval_expr env e' with
    | VInt v, VInt v' -> VBool (v <= v')
    | VContent v, VContent v' -> VBool (v <= v')
    | VString v, VString v' -> VBool (v <= v')
    | VBool v, VBool v' -> VBool (v <= v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Uncomparable." (string_of_expr (Leq (e, e')))))
  end
  | Eq (e, e') -> begin match eval_expr env e, eval_expr env e' with
    | VInt v, VInt v' -> VBool (v = v')
    | VContent v, VContent v' -> VBool (v = v')
    | VString v, VString v' -> VBool (v = v')
    | VBool v, VBool v' -> VBool (v = v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Uncomparable." (string_of_expr (Eq (e, e')))))
  end
  | Neq (e, e') -> begin match eval_expr env e, eval_expr env e' with
    | VInt v, VInt v' -> VBool (v <> v')
    | VContent v, VContent v' -> VBool (v <> v')
    | VString v, VString v' -> VBool (v <> v')
    | VBool v, VBool v' -> VBool (v <> v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Uncomparable." (string_of_expr (Neq (e, e')))))
  end
  | And (e, e') -> begin match eval_expr env e, eval_expr env e' with
    | VBool b, VBool b' -> VBool (b && b')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Booleans expected." (string_of_expr (And (e, e')))))
  end
  | Or (e, e') -> begin match eval_expr env e, eval_expr env e' with
    | VBool b, VBool b' -> VBool (b || b')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Booleans expected." (string_of_expr (And (e, e')))))
  end
  | Not e -> begin match eval_expr env e with
    | VBool b -> VBool (not b)
    | _ -> raise (InterpreterError (Printf.sprintf "%s: Integer expected." (string_of_expr (Neg e))))
  end
  | Bool b -> VBool b
  | Concat (e, e') -> begin match eval_expr env e, eval_expr env e' with
    | VString s1, VString s2 -> VString (s1 ^ s2)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Strings expected." (string_of_expr (Concat (e, e')))))
  end
  | String s -> VString s
  | Fstring s -> raise (UnsupportedError "FString are not supported by now")
  | WithModule (module_name, e) -> begin match Environment.submap_opt module_name env with (* FIXME like in typechecker *)
    | None -> raise (InterpreterError (Printf.sprintf "%s: undefined module." module_name))
    | Some sub_env -> eval_expr sub_env e 
  end
(** [eval env page = ((session_vars, env'), res)] where [res] is the evaluation of [page] following the program semantics (cf. documentation). [env'] is the resulting environment ([env] + declared globals, etc) and [session_vars] is the list of globally-declared session variable (at top-level only). FIXME at some point, add a function to Session module to do that instead. See what to change. Should we authorise effect in expression or rather add another global declaration ? *)
and eval (env : environment) (page : dynml_webpage) : (string list * environment) * value list =
  (* Actually evaluating [page] *)
  let values_and_env = List.fold_left begin fun already_evald element -> begin match already_evald with
      | [] -> assert false
      | ((cur_session_vars, cur_env), v) :: already_evald' -> begin match element with
        | Script e -> let v_e = eval_expr cur_env e in ((cur_session_vars, cur_env), v_e) :: ((cur_session_vars, cur_env), v) :: already_evald' (* FIXME change here if we want to take into account nested session variables declarations *)
        | Pure s -> ((cur_session_vars, cur_env), VPure s) :: ((cur_session_vars, cur_env), v) :: already_evald'
        | Decl (ExprDecl (x, e)) -> let v_e = eval_expr cur_env e in (update_env x v_e (cur_session_vars, cur_env), v) :: already_evald' (* evaluating a global only enriches the environment, no value is added *)
        | Decl (TypeDecl (x, e)) -> raise (UnsupportedError "Type declarations are not supported by now (eval)")
      end
    end
  end [(([], env), VBool true)] page
  in
  (* Tidying up the results *)
  let values_and_env = List.tl (List.rev values_and_env) in (* removing the dummy true value. *)
  let final_env = match values_and_env with
    | (final_env', _) :: _ -> final_env'
    | [] -> ([], env)
  in
  (final_env, List.map snd values_and_env)

and extern_sqlite_exec = fun db fold_lines fold_cells str_query -> match db, fold_lines, fold_cells, str_query with
  | VDb db, Clos (captured_combine_lines, VFun (prev_lines_acc, body_of_newline)), Clos (captured_combine_cells, VFun (acc, body_function_of_hs_and_content)), VString query ->
    (value_of_query db
      (fun v1 v2 -> eval_expr captured_combine_lines (App (App (Fun (prev_lines_acc, body_of_newline), expr_of_value v1), expr_of_value v2)))
      (fun line_acc hd content -> eval_expr captured_combine_cells (App (App ((App ((Fun (acc, body_function_of_hs_and_content)), expr_of_value line_acc)), String hd), String content)))
      query)
  | _, _, _, _ -> raise (InterpreterError (Printf.sprintf "%s, %s, %s, %s: Expected a database, a line folding function, a cell folding function and a SQL query (as a string)." (string_of_value db) (string_of_value fold_lines) (string_of_value fold_cells) (string_of_value str_query))) (* TODO maybe refine this bit *)

let extern_sqlite_open_db (db_path : value) : value = match db_path with
  | VString s -> VDb (Sqlite3.db_open s)
  | _ -> raise (InterpreterError (Printf.sprintf "%s: path to a database expected." (string_of_value db_path)))
let extern_sqlite_close_db (vdb : value) : value = begin match vdb with
    | VDb db -> VBool (Sqlite3.db_close db)
    | _ -> raise (InterpreterError (Printf.sprintf "%s: database expected." (string_of_value vdb)))
  end 

(* TODO add "garbage-collection" *)