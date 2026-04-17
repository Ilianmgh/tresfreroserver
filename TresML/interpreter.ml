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

(** [escape_single_quote_from_string s] returns a string identical to [s], where single quotes within single quotes are escaped. *)
let escape_single_quote_from_string (s : string) : string = s

let value_of_query (db : Sqlite3.db) (combine_lines_into_table : value -> value -> value) (combine_cells_into_line : value -> string -> string -> value) (query : string) : value =
  (* we first need to escape ' character from the query, since they are the string delimiters in SQLite. *)
  let query = escape_single_quote_from_string query in
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
    | OK | DONE -> !value_acc
    | _ -> raise (InterpreterError (Printf.sprintf "SQL query \"%s\" failed with error: %s" query (Sqlite3.Rc.to_string exec_code)))

(** [eval_expr env e1 = v] where [v] is the evaluation of expression [e] following the program semantics (cf documentation). FIXME TO IMPLEMENT SESSION/COOKIES VARIABLES, BUT BETTER, maybe return the environment to retrieve sessions (and at some point, cookies) variables. maybe return only the interesting environment e.g. the sub environment Session and Cookie, not all the local variables. *)
let rec eval_expr (orig_env : environment) (env : environment) (e1 : expr) : (string option) * value = match e1 with
  | Empty -> assert false
  | Let (x, e, e') -> let location, v = eval_expr orig_env env e in eval_expr orig_env (Environment.add x v env) e'
  | Fun (x, e) -> None, Clos (env, VFun (x, e))
  | Fix (f, x, e) -> None, Clos (env, VFix (f, x, e))
  | App (e, e') -> begin match eval_expr orig_env env e with
    | location, Clos (_, VExternFunction (_, Args1 f)) ->
      let location', v_arg1 = eval_expr orig_env env e' in
      let applied_extern_f = f orig_env v_arg1 in
      begin match applied_extern_f with
        | VLocation path -> Some path, applied_extern_f (* If a function returns a location, we indicate to redirect to the pointed page. *)
        | v -> (location', v)
      end
    | location, Clos (_, VExternFunction (name, Args2 f)) ->
      let location', v_arg1 = eval_expr orig_env env e' in
      location', Clos (Environment.empty, VExternFunction (name, Args1 (straightforward_fun_dropping_reset_env (f orig_env v_arg1))))
    | location, Clos (_, VExternFunction (name, Args3 f)) ->
      let location', v_arg1 = eval_expr orig_env env e' in
      location', Clos (Environment.empty, VExternFunction (name, Args2 (straightforward_fun_dropping_reset_env (f orig_env v_arg1))))
    | location, Clos (_, VExternFunction (name, Args4 f)) ->
      let location', v_arg1 = eval_expr orig_env env e' in
      location', Clos (Environment.empty, VExternFunction (name, Args3 (straightforward_fun_dropping_reset_env (f orig_env v_arg1))))
    | location, Clos (env', VFun (x, e_f)) -> let location', v = eval_expr orig_env env e' in eval_expr orig_env (Environment.add x v env') e_f
    | location, Clos (env', VFix (f, x, e_f)) -> let location', v = eval_expr orig_env env e' in
      let env'_x = Environment.add x v env' in
      let env'_f_x = Environment.add f (Clos (env', VFix (f, x, e_f))) env'_x in
      eval_expr orig_env env'_f_x e_f
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: it is not a function, it cannot be applied." (string_of_expr e)))
  end
  | If (c, t, e) -> begin match eval_expr orig_env env c with
    | location, VBool b -> if b then eval_expr orig_env env t else eval_expr orig_env env e
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: boolean expected" (string_of_expr c)))
  end
  | Seq (e, e') ->
    let v = eval_expr orig_env env e in
    let v' = eval_expr orig_env env e' in
    drop v;
    v'
  | Html lst_e -> let env, location, v = eval_page orig_env env lst_e in location, VContent v (* FIXME change here too cf comment in [eval] *)
  | Var x -> begin match Environment.find_opt x env with
    | Some v -> None, v
    | None -> None, VUndefinedVariable
  end
  | Couple (e, e') ->
    let location, v = eval_expr orig_env env e in
    let location', v' = eval_expr orig_env env e' in
    location', VCouple (v, v') (* FIXME not sure of the semantics of locations here *)
  | Plus (e, e') -> begin match eval_expr orig_env env e, eval_expr orig_env env e' with
    | (location, VInt n), (location', VInt m) -> location', VInt (n + m) (* FIXME not sure of the semantics of locations here *)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Plus (e, e')))))
  end
  | Minus (e, e') -> begin match eval_expr orig_env env e, eval_expr orig_env env e' with
    | (location, VInt n), (location', VInt m) -> location', VInt (n - m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Minus (e, e')))))
  end
  | Neg e -> begin match eval_expr orig_env env e with
    | (location, VInt n) -> location, VInt (-n)
    | _ -> raise (InterpreterError (Printf.sprintf "%s: Integer expected." (string_of_expr (Neg e))))
  end
  | Mult (e, e') -> begin match eval_expr orig_env env e, eval_expr orig_env env e' with
    | (location, VInt n), (location', VInt m) -> location', VInt (n * m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Mult (e, e')))))
  end
  | Div (e, e') -> begin match eval_expr orig_env env e, eval_expr orig_env env e' with
    | (location, VInt n), (location', VInt m) -> location', VInt (n / m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Div (e, e')))))
  end
  | Pow (e, e') -> begin match eval_expr orig_env env e, eval_expr orig_env env e' with
    | (location, VInt n), (location', VInt m) -> location', VInt (n ^^ m)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Integers expected." (string_of_expr (Pow (e, e')))))
  end
  | Int n -> None, VInt n
  | Gt (e, e') -> begin match eval_expr orig_env env e, eval_expr orig_env env e' with
    | (location, VInt v), (location', VInt v') -> location', VBool (v > v')
    | (location, VContent v), (location', VContent v') -> location', VBool (v > v')
    | (location, VString v), (location', VString v') -> location', VBool (v > v')
    | (location, VBool v), (location', VBool v') -> location', VBool (v > v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Uncomparable." (string_of_expr (Gt (e, e')))))
  end
  | Lt (e, e') -> begin match eval_expr orig_env env e, eval_expr orig_env env e' with
    | (location, VInt v), (location', VInt v') -> location', VBool (v < v')
    | (location, VContent v), (location', VContent v') -> location', VBool (v < v')
    | (location, VString v), (location', VString v') -> location', VBool (v < v')
    | (location, VBool v), (location', VBool v') -> location', VBool (v < v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Uncomparable." (string_of_expr (Lt (e, e')))))
  end
  | Geq (e, e') -> begin match eval_expr orig_env env e, eval_expr orig_env env e' with
    | (location, VInt v), (location', VInt v') -> location', VBool (v >= v')
    | (location, VContent v), (location', VContent v') -> location', VBool (v >= v')
    | (location, VString v), (location', VString v') -> location', VBool (v >= v')
    | (location, VBool v), (location', VBool v') -> location', VBool (v >= v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Uncomparable." (string_of_expr (Geq (e, e')))))
  end
  | Leq (e, e') -> begin match eval_expr orig_env env e, eval_expr orig_env env e' with
    | (location, VInt v), (location', VInt v') -> location', VBool (v <= v')
    | (location, VContent v), (location', VContent v') -> location', VBool (v <= v')
    | (location, VString v), (location', VString v') -> location', VBool (v <= v')
    | (location, VBool v), (location', VBool v') -> location', VBool (v <= v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Uncomparable." (string_of_expr (Leq (e, e')))))
  end
  | Eq (e, e') -> begin match eval_expr orig_env env e, eval_expr orig_env env e' with
    | (location, VInt v), (location', VInt v') -> location', VBool (v = v')
    | (location, VContent v), (location', VContent v') -> location', VBool (v = v')
    | (location, VString v), (location', VString v') -> location', VBool (v = v')
    | (location, VBool v), (location', VBool v') -> location', VBool (v = v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Uncomparable." (string_of_expr (Eq (e, e')))))
  end
  | Neq (e, e') -> begin match eval_expr orig_env env e, eval_expr orig_env env e' with
    | (location, VInt v), (location', VInt v') -> location', VBool (v <> v')
    | (location, VContent v), (location', VContent v') -> location', VBool (v <> v')
    | (location, VString v), (location', VString v') -> location', VBool (v <> v')
    | (location, VBool v), (location', VBool v') -> location', VBool (v <> v')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Uncomparable." (string_of_expr (Neq (e, e')))))
  end
  | And (e, e') -> begin match eval_expr orig_env env e, eval_expr orig_env env e' with
    | (location, VBool b), (location', VBool b') -> location', VBool (b && b')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Booleans expected." (string_of_expr (And (e, e')))))
  end
  | Or (e, e') -> begin match eval_expr orig_env env e, eval_expr orig_env env e' with
    | (location, VBool b), (location', VBool b') -> location', VBool (b || b')
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Booleans expected." (string_of_expr (And (e, e')))))
  end
  | Not e -> begin match eval_expr orig_env env e with
    | location, VBool b -> location, VBool (not b)
    | _ -> raise (InterpreterError (Printf.sprintf "%s: Integer expected." (string_of_expr (Neg e))))
  end
  | Bool b -> None, VBool b
  | Concat (e, e') -> begin match eval_expr orig_env env e, eval_expr orig_env env e' with
    | (location, VString s1), (location', VString s2) -> location', VString (s1 ^ s2)
    | _, _ -> raise (InterpreterError (Printf.sprintf "%s: Strings expected." (string_of_expr (Concat (e, e')))))
  end
  | String s -> None, VString s
  | Fstring lst -> None, VString begin
    List.fold_left begin fun acc -> function
      | FstrExpr e -> Printf.sprintf "%s%s" acc (string_of_value ~escape_html:false (snd (eval_expr orig_env env e)))
      | FstrString s -> Printf.sprintf "%s%s" acc s
    end "" lst
  end
  | Unit -> None, VUnit
  | WithModule (module_name, e) -> begin match Environment.submap_opt module_name env with (* FIXME like in typechecker *)
    | None -> raise (InterpreterError (Printf.sprintf "%s: undefined module." module_name))
    | Some sub_env -> eval_expr orig_env sub_env e 
  end

(** [eval_page orig_env env page = (env', location, res)] same as [eval]. Except when encountering inserted page that needs to reset the environment, sets the environment to [orig_env]. *)
and eval_page (orig_env : environment) (env : environment) (page : dynml_webpage) : environment * string option * value list =
  (* Actually evaluating [page] *)
  let final_location, values_and_env = List.fold_left begin fun already_evald element -> begin match already_evald with
      | _, [] -> assert false
      | location, ((cur_env, v) :: already_evald') -> begin match element with
        | Script e -> let new_location, v_e = eval_expr orig_env cur_env e in
          new_location, (cur_env, v_e) :: (cur_env, v) :: already_evald' (* FIXME change here if we want to take into account nested session variables declarations *)
        | Pure s -> location, (cur_env, VPure s) :: (cur_env, v) :: already_evald'
        | Decl (ExprDecl (x, e)) -> let e_location, v_e = eval_expr orig_env cur_env e in
          let new_location =
            begin match e_location with
              | None -> location
              | Some actual_e_location -> Some actual_e_location
            end
          in
          new_location, (Environment.add x v_e cur_env, v) :: already_evald' (* evaluating a global only enriches the environment, no value is added *)
        | Decl (ModuleExprDecl (modu, x, e)) -> let e_location, v_e = eval_expr orig_env cur_env e in
          let new_location =
            begin match e_location with
              | None -> location
              | Some actual_e_location -> Some actual_e_location
            end
          in
          new_location, (Environment.add_to_sub [modu] x v_e cur_env, v) :: already_evald' (* evaluating a global only enriches the environment, no value is added *)
        | Decl (OpenModule modu) -> begin match Environment.submap_opt modu cur_env with
          | None -> raise (InterpreterError (Printf.sprintf "%s: undefined module." modu))
          | Some modu_typenv -> location, (Environment.hoist_submap cur_env modu, v) :: already_evald' (* FIXME /!\ for now, open actually has the semantic of OCaml's include *)
            (* evaluating a global only enriches the environment, no value is added *)
        end
        | Decl (ImportModule _) -> failwith "Trying to type an unlinked page (here link in compilation, not link as in the Web)."
        | Decl (Inserted (mode, insd_page)) -> begin
          let (final_env_insd_page, _, evald_insd_page) = if mode.reset_environment then
              eval_page orig_env orig_env insd_page
            else
              eval_page orig_env env insd_page
          in
          let env_after_evaluation = if mode.final_env_available then
              final_env_insd_page
            else
              Environment.empty
          in
          (* adding the content of the page so it is accessible as ModuleName.Import.content iff mode.content_available = true *)
          let final_env_from_insd = begin
              if mode.content_available then
                Environment.add_and_path ["Meta"] "content" (VContent evald_insd_page)
              else
                (fun x -> x)
            end env_after_evaluation 
          in
          let next_env = Environment.add_sub mode.module_name final_env_from_insd cur_env in
          location, ((next_env, v) :: already_evald')
          (* evaluating a global only enriches the environment, no value is added *)
        end
        | Decl (TypeDecl (x, e)) -> raise (UnsupportedError "Type declarations are not supported by now (eval)")
      end
    end
  end (None, [(env, VBool true)]) page
  in
  (* Tidying up the results *)
  let values_and_env = List.tl (List.rev values_and_env) in (* removing the dummy true value. *)
  let final_env = match values_and_env with
    | (final_env', _) :: _ -> final_env'
    | [] -> env
  in
  (final_env, final_location, List.map snd values_and_env)

and extern_sqlite_exec_with_reset_env = fun (orig_env : environment) db fold_lines fold_cells str_query -> match db, fold_lines, fold_cells, str_query with
  | VDb db, Clos (captured_combine_lines, VFun (prev_lines_acc, body_of_newline)), Clos (captured_combine_cells, VFun (acc, body_function_of_hs_and_content)), VString query ->
    (value_of_query db
      begin fun v1 v2 -> snd (
          eval_expr
            Environment.empty
            captured_combine_lines
            (App (App (Fun (prev_lines_acc, body_of_newline), expr_of_value v1), expr_of_value v2))
        )
      end
      begin
        fun line_acc hd content -> snd (
          eval_expr
            (Environment.add acc line_acc orig_env)
            captured_combine_cells
            (App (App (body_function_of_hs_and_content, String hd), String content))
          )
      end
      query)
  | _, _, _, _ -> raise (InterpreterError (Printf.sprintf "%s, %s, %s, %s: Expected a database, a line folding function, a cell folding function and a SQL query (as a string)." (string_of_value db) (string_of_value fold_lines) (string_of_value fold_cells) (string_of_value str_query))) (* TODO maybe refine this bit *)

let extern_sqlite_open_db_with_root_path (root_path : string) (v : value) : value = match v with
  | VString db_path -> begin Printf.fprintf stderr "root: %s\ndb: %s\n" root_path db_path; match separator_ended_dir_path_opt root_path with
    | None -> raise (InterpreterError (Printf.sprintf "%s: Root path is not a directory." root_path))
    | Some dir_root_path -> if is_subfolder db_path then
        VDb (Sqlite3.db_open (dir_root_path ^ db_path))
      else
        raise (InterpreterError (Printf.sprintf "trying to open database %s: Access denied, cannot load file outside of project root. See --root option." db_path))
  end
  | _ -> raise (InterpreterError (Printf.sprintf "%s: path to a database expected." (string_of_value v)))
let extern_sqlite_close_db (vdb : value) : value = begin match vdb with
    | VDb db -> VBool (Sqlite3.db_close db)
    | _ -> raise (InterpreterError (Printf.sprintf "%s: database expected." (string_of_value vdb)))
  end 

let ml_string_replace = fun template replacement s ->  begin match template, replacement, s with
    | VString template, VString replacement, VString s -> VString (Str.global_replace (Str.regexp template) replacement s)
    | _ -> raise (InterpreterError (Printf.sprintf "%s, %s, %s: strings expected." (string_of_value template) (string_of_value replacement) (string_of_value s)))
  end 

let ml_redirect (v : value) : value = match v with
  | VString path -> VLocation path
  | _ -> raise (InterpreterError (Printf.sprintf "%s: string expected" (string_of_value v)))

(** If [i] is nonnegative and [len_s] is less than the length of [s], [ustring_get s from i = c] where [c] is a string representing the [i]-th utf-8 character of [s[from..len_s]]. *)
let rec ustring_get (s : string) (len_s_ascii : int) (from_ascii : int) (j_utf8 : int) (orig_j_utf8 : int) : string =
  if from_ascii < len_s_ascii then
    let length_cur_uchar = Uchar.utf_decode_length (String.get_utf_8_uchar s from_ascii) in
    if j_utf8 = 0 then
      String.sub s from_ascii length_cur_uchar
    else
      ustring_get s len_s_ascii (from_ascii + length_cur_uchar) (j_utf8 - 1) orig_j_utf8
  else
    raise (InterpreterError (Printf.sprintf "%d-th character of %s: get out of bounds." orig_j_utf8 s))

let ml_string_get (v_s : value) (v_i : value) : value = match v_s, v_i with
  | VString s, VInt i -> VString (ustring_get s (String.length s) 0 i i)
  | _, _ -> raise (InterpreterError (Printf.sprintf "%s, %s: string and integer expected." (string_of_value v_s) (string_of_value v_i)))

let ml_fst (v : value) : value = match v with
  | VCouple (v1, v2) -> v1
  | _ -> raise (InterpreterError (Printf.sprintf "%s: expected a pair." (string_of_value v)))

let ml_snd (v : value) : value = match v with
  | VCouple (v1, v2) -> v2
  | _ -> raise (InterpreterError (Printf.sprintf "%s: expected a pair." (string_of_value v)))

let ml_not (v : value) : value = match v with
  | VBool true -> VBool false
  | VBool false -> VBool true
  | _ -> raise (InterpreterError (Printf.sprintf "%s: expected a boolean." (string_of_value v)))

let ml_is_defined (v : value) : value = match v with
  | VUndefinedVariable -> VBool false
  | _ -> VBool true

(** [eval env page = (env', location, res)] where [res] is the evaluation of [page] following the program semantics (cf. documentation). [env'] is the resulting environment ([env] + declared globals, etc) and [session_vars] is the list of globally-declared session variable (at top-level only). *)
let eval (env : environment) (page : dynml_webpage) : environment * string option * value list = eval_page env env page

(* TODO add "garbage-collection" *)