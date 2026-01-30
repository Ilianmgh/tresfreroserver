open Utils
open Lexic
open Syntax
open Typechecker

exception InterpreterError of string

type raw_function_value =
  | VFst
  | VSnd
  | VSqliteOpenDb
  | VSqliteCloseDb
  | VSqliteExec | VSqliteExecOne of value | VSqliteExecTwo of value * value (* FIXME this is supposed to take into account partial application. It's lame but FIXME *)
  | VFun of variable * expr
  | VFix of variable * variable * expr

and value =
  | Clos of environment * raw_function_value
  | VDb of Sqlite3.db
  | VInt of int
  | VBool of bool
  | VString of string
  | VPure of string
  | VContent of value list (* set VContent at a value list add VPure of string ....*)
  | VCouple of value * value
and environment = value StringMap.t

(** Pretty-printing *)

(** correctly espaces characters for web rendering *)
let web_of_string (s : string) : string =
  let rec web_of_string_acc (s : string) (i : int) (n : int) (acc : char list) : char list = if i < n then begin match s.[i] with
      | '&' ->  web_of_string_acc s (i+1) n (';' :: 'p' :: 'm' :: 'a' :: '&' :: acc)
      | '<' ->  web_of_string_acc s (i+1) n (';' :: 't' :: 'l' :: '&' :: acc)
      | '>' ->  web_of_string_acc s (i+1) n (';' :: 't' :: 'g' :: '&' :: acc)
      | '"' ->  web_of_string_acc s (i+1) n (';' :: 't' :: 'o' :: 'u' :: 'q' :: '&' :: acc)
      | '\'' -> web_of_string_acc s (i+1) n (';' :: 's' :: 'o' :: 'p' :: 'a' :: '&' :: acc)
      | c -> web_of_string_acc s (i+1) n (c :: acc)
    end else
      acc
  in
  string_of_char_list (List.rev (web_of_string_acc s 0 (String.length s) []))

let rec fprintf_value (out : out_channel) ?(escape_html : bool = false) (v1 : value) : unit = match v1 with (* TODO Ugly duplicated code, find a way to sort this out *)
  | VInt n -> Printf.fprintf out "%d" n
  | VDb db -> Printf.fprintf out "adatabase" (* TODO !!!! *)
  | VBool b -> if b then Printf.fprintf out "true" else Printf.fprintf out "false"
  | VString s -> Printf.fprintf out "%s" (web_of_string s)
  | VPure h -> if escape_html then Printf.fprintf out "%s" (web_of_string h) else Printf.fprintf out "%s" h (* FIXME not sure if useful since bypassed in `produce_page` *)
  | VContent l -> List.iter (fun v -> fprintf_value out ~escape_html:escape_html v) l
  | VCouple (v, v') -> begin
    Printf.fprintf out "(";
    fprintf_value out ~escape_html:escape_html v;
    Printf.fprintf out ",";
    fprintf_value out ~escape_html:escape_html v';
    Printf.fprintf out ")"
  end
  | Clos (_, VFst) -> Printf.fprintf out "⟨∅, fst⟩"
  | Clos (_, VSnd) -> Printf.fprintf out "⟨∅, snd⟩"
  | Clos (_, VSqliteOpenDb) -> Printf.fprintf out "⟨∅, sqlite3_opendb⟩"
  | Clos (_, VSqliteCloseDb) -> Printf.fprintf out "⟨∅, sqlite3_closedb⟩"
  | Clos (_, VSqliteExec) -> Printf.fprintf out "⟨∅, sqlite3_exec⟩"
  | Clos (_, VSqliteExecOne v) -> begin
    Printf.fprintf out "⟨∅, sqlite3_exec ";
    fprintf_value out ~escape_html:escape_html v;
    Printf.fprintf out "⟩"
  end
  | Clos (_, VSqliteExecTwo (v1, v2)) -> begin
    Printf.fprintf out "⟨∅, sqlite3_exec ";
    fprintf_value out ~escape_html:escape_html v1;
    Printf.fprintf out " ";
    fprintf_value out ~escape_html:escape_html v2;
    Printf.fprintf out "⟩"
  end
  | Clos (env, VFun (x, e)) -> begin
    Printf.fprintf out "⟨";
    fprintf_env out ~escape_html:escape_html env;
    Printf.fprintf out ", fun %s -> %s⟩" x (string_of_expr e); (* FIXME maybe write fpritnf_expr *)
  end
  | Clos (env, VFix (f, x, e)) -> begin
    Printf.fprintf out "⟨";
    fprintf_env out ~escape_html:escape_html env;
    Printf.fprintf out ", fixfun %s %s -> %s⟩" f x (string_of_expr e); (* FIXME maybe write fpritnf_expr *)
  end
and fprintf_env (out : out_channel) ?(escape_html : bool = false) (env : environment) : unit =
  if StringMap.is_empty env then Printf.fprintf out "∅" else begin
    let string_of_one_env_binding (x : variable) (v : value) : unit =
      Printf.fprintf out ", %s ↦ " x;
      fprintf_value out ~escape_html:true v
    in
    StringMap.iter string_of_one_env_binding env
  end

let rec string_of_value ?(escape_html : bool = false) (v1 : value) : string = match v1 with
  | VDb db -> "adatabase" (* TODO !!!! *)
  | VInt n -> Printf.sprintf "%d" n
  | VBool b -> if b then "true" else "false"
  | VString f -> f
  | VPure h -> if escape_html then web_of_string h else h (* FIXME not sure if useful since bypassed in `produce_page` *)
  | VContent l -> List.fold_left (fun acc v -> Printf.sprintf "%s%s" acc (string_of_value ~escape_html:escape_html v)) "" l
  | VCouple (v, v') -> Printf.sprintf "(%s, %s)" (string_of_value ~escape_html:escape_html v) (string_of_value ~escape_html:escape_html v')
  | Clos (_, VFst) -> "⟨∅, fst⟩"
  | Clos (_, VSnd) -> "⟨∅, snd⟩"
  | Clos (_, VSqliteOpenDb) -> "⟨∅, sqlite3_opendb⟩"
  | Clos (_, VSqliteCloseDb) -> "⟨∅, sqlite3_closedb⟩"
  | Clos (_, VSqliteExec) -> "⟨∅, sqlite3_exec⟩"
  | Clos (_, VSqliteExecOne v) -> Printf.sprintf "⟨∅, sqlite3_exec %s⟩" (string_of_value ~escape_html:escape_html v)
  | Clos (_, VSqliteExecTwo (v1, v2)) -> Printf.sprintf "⟨∅, sqlite3_exec %s %s⟩" (string_of_value ~escape_html:escape_html v1) (string_of_value ~escape_html:escape_html v2)
  | Clos (env, VFun (x, e)) -> Printf.sprintf "⟨%s, fun %s -> %s⟩" (string_of_env ~escape_html:escape_html env) x (string_of_expr e)
  | Clos (env, VFix (f, x, e)) -> Printf.sprintf "⟨%s, fixfun %s %s -> %s⟩" (string_of_env ~escape_html:escape_html env) f x (string_of_expr e)
and string_of_env ?(escape_html : bool = false) (env : environment) : string = if StringMap.is_empty env then "∅" else begin
    let string_of_one_env_binding (x : variable) (v : value) (acc : string) : string =
      if acc = "" then
        Printf.sprintf "%s ↦ %s" x (string_of_value ~escape_html:true v)
      else
        Printf.sprintf "%s ↦ %s, %s" x (string_of_value ~escape_html:true v) acc
    in
    StringMap.fold string_of_one_env_binding env ""
  end

let rec ( ^^ ) (n : int) (p : int) = match p with
  | 0 -> 1
  | 1 -> n
  | p -> let p', r = p/2, p mod 2 in
    let np' = n ^^ p' in
    np' * np' + if r = 1 then n else 0

let drop = fun x -> ()

let string_of_query (db : Sqlite3.db) (query : string) : string =
  let res_str = ref [] in
  let string_of_query_res (res_storage : string list ref) (line : Sqlite3.row) (hdrs : Sqlite3.headers) =
    Array.iter2 begin fun hdr -> function
      | None -> res_storage := (Printf.sprintf "%s: NULL" hdr) :: (!res_storage)
      | Some x -> res_storage := (Printf.sprintf "%s: %s" hdr x) :: (!res_storage)
    end hdrs line
  in
  Sqlite3.exec db ~cb:(string_of_query_res res_str) query;
  String.concat ", " (List.rev !res_str)

let rec eval_expr (env : environment) (e1 : expr) : value = match e1 with
  | Empty -> assert false
  | Let (x, e, e') -> let v = eval_expr env e in eval_expr (StringMap.add x v env) e'
  | Fun (x, e) -> Clos (env, VFun (x, e))
  | Fix (f, x, e) -> Clos (env, VFix (f, x, e))
  | App (e, e') -> begin match eval_expr env e with
    | Clos (_, VFst) -> begin match eval_expr env e' with
      | VCouple (v, v') -> v
      | _ -> raise (InterpreterError (Printf.sprintf "%s: pair expected." (string_of_expr e)))
    end
    | Clos (_, VSnd) -> begin match eval_expr env e' with
      | VCouple (v, v') -> v'
      | _ -> raise (InterpreterError (Printf.sprintf "%s: pair expected." (string_of_expr e)))
    end
    | Clos (_, VSqliteOpenDb) -> begin match eval_expr env e' with
      | VString s -> VDb (Sqlite3.db_open s)
      | _ -> raise (InterpreterError (Printf.sprintf "%s: path to a database expected." (string_of_expr e)))
    end
    | Clos (_, VSqliteCloseDb) -> begin match eval_expr env e' with
      | VDb db -> VBool (Sqlite3.db_close db)
      | _ -> raise (InterpreterError (Printf.sprintf "%s: database expected." (string_of_expr e)))
    end
    | Clos (_, VSqliteExec) -> begin match eval_expr env e' with
      | VDb db -> Clos (StringMap.empty, VSqliteExecOne (VDb db))
      | _ -> raise (InterpreterError (Printf.sprintf "%s: database expected." (string_of_expr e)))
    end
    | Clos (_, VSqliteExecOne db) -> begin match eval_expr env e' with
      | Clos (captured, func) -> Clos (StringMap.empty, VSqliteExecTwo (db, Clos (captured, func)))
      | _ -> raise (InterpreterError (Printf.sprintf "%s: function expected." (string_of_expr e)))
    end
    | Clos (_, VSqliteExecTwo (VDb db, func)) -> begin match eval_expr env e' with
      | VString query -> VString (string_of_query db query) (* TODO FOR NOW IT'S A STRING BUT IS NOT EVEN OF THE RIGHT TYPE, PLUS IT IGNORES THE FUNCTION, SIMPLY TO SEE IF IT WORKS *)
      | _ -> raise (InterpreterError (Printf.sprintf "%s: string expected." (string_of_expr e)))
    end
    | Clos (env', VFun (x, e_f)) -> let v = eval_expr env e' in eval_expr (StringMap.add x v env') e_f
    | Clos (env', VFix (f, x, e_f)) -> let v = eval_expr env e' in
      let env'_x = StringMap.add x v env' in
      let env'_f_x = StringMap.add f (Clos (env', VFix (f, x, e_f))) env'_x in
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
  | Html lst_e -> VContent (eval env lst_e)
  | Var x -> begin match StringMap.find_opt x env with
    | Some v -> v
    | None -> raise (InterpreterError (Printf.sprintf "%s: Undefined variable" x))
  end
  | Couple (e, e') ->
    let v = eval_expr env e in
    let v' = eval_expr env e' in
    VCouple (v, v')
  | Fst -> Clos (StringMap.empty, VFst)
  | Snd -> Clos (StringMap.empty, VSnd)
  | SqliteOpenDb -> Clos (StringMap.empty, VSqliteOpenDb)
  | SqliteCloseDb -> Clos (StringMap.empty, VSqliteCloseDb)
  | SqliteExec -> Clos (StringMap.empty, VSqliteExec)
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
  | Fstring s -> failwith "TODO"

and eval (env : environment) (page : dynml_webpage) : value list =
  let values_and_env = List.fold_left begin fun already_evald element -> begin match already_evald with
      | [] -> assert false
      | (cur_env, v) :: already_evald' -> begin match element with
        | Script e -> let v_e = eval_expr cur_env e in (cur_env, v_e) :: (cur_env, v) :: already_evald'
        | Pure s -> (cur_env, VPure s) :: (cur_env, v) :: already_evald'
        | Decl (ExprDecl (x, e)) -> let v_e = eval_expr cur_env e in (StringMap.add x v_e cur_env, v) :: already_evald' (* evaluating a global only enriches the environment, no value is added *)
        | Decl (TypeDecl (x, e)) -> failwith "TODO"
      end
    end
  end [(env, VBool true)] page
  in
  List.map (fun (x, y) -> y) (List.tl (List.rev values_and_env)) (* FIXME DANS LE MAUVAIS SENS WTF ????FIXMEFIXMEFIXME *)

(* TODO add "garbage-collection" *)