open Utils
open Syntax

type raw_function_value =
  | VFst
  | VSnd
  | VSqliteOpenDb
  | VSqliteCloseDb
  | VSqliteExecPartialApp of value list
  | VFun of variable * expr
  | VFix of variable * variable * expr

and value =
  | Clos of environment * raw_function_value
  | VDb of Sqlite3.db
  | VInt of int
  | VBool of bool
  | VString of string
  | VPure of string
  | VContent of value list
  | VCouple of value * value
and environment = value StringMap.t

(** [eval_expr anyEnv (expr_of_value v) = v].
  [expr_of_value v] tries to be as simple as possible for a lightweight re-evaluation. *)
let rec expr_of_value (v1 : value) : expr = match v1 with
  | VDb db -> raise (UnsupportedError "TODO not sure it's supposed to work here (reminder, it's designed for value_of_query)")
  | VInt n -> Int n
  | VBool b -> Bool b
  | VString s -> String s
  | VPure h -> Html [Pure h]
  | VContent l -> Html (List.map (fun v -> Script (expr_of_value v)) l)
  | VCouple (v, v') -> Couple (expr_of_value v, expr_of_value v')
  | Clos (_, VFst) -> Fst
  | Clos (_, VSnd) -> Snd
  (* I'm not sure we really want the following cases to work. At least for [value_of_query], I can't think of a useful use case *)
  | Clos (_, VSqliteOpenDb) -> SqliteOpenDb
  | Clos (_, VSqliteCloseDb) -> SqliteCloseDb
  | Clos (_, VSqliteExecPartialApp []) -> SqliteExec
  | Clos (_, VSqliteExecPartialApp [db]) -> App (SqliteExec, expr_of_value db) (* remark: won't actually work since db is not implemented *)
  | Clos (_, VSqliteExecPartialApp [db; func]) -> App (App (SqliteExec, expr_of_value db), expr_of_value func)
  | Clos (_, VSqliteExecPartialApp _) -> raise (UnsupportedError "Trying to get expr of value sqlite_exec applied to too many arguments")
  | Clos (env, VFun (x, e)) -> raise (UnsupportedError "TODO not sure it's supposed to work here (reminder, it's designed for value_of_query)")
  | Clos (env, VFix (f, x, e)) -> raise (UnsupportedError "TODO not sure it's supposed to work here (reminder, it's designed for value_of_query)")

(** [update_env x v session_vars env] adds binding [x] |-> [v] to [env]. If [x] is a session variable, adds it to [session_vars]. *)
let update_env (x : string) (v : value) (session_vars, env : string list * environment) : string list * environment = (* FIXME maybe write a module for environment to wrap session variables with it and, at some point, cookies. Maybe it'll help for cookies. *)
  let new_session_vars = if Str.string_match (Str.regexp "session") x 0 then (* may be overkill.. String.sub & = ? *)
      x :: session_vars (* for now, adding the full variable, maybe strip it from the prefix. Will be easier when namespaces are implemented *)
    else
      session_vars
  in
  (new_session_vars, StringMap.add x v env)

(** Pretty-printing *)

(** Correctly escapes characters for web rendering cf https://html.spec.whatwg.org/multipage/named-characters.html TODO to them all (?) *)
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
  | Clos (_, VSqliteExecPartialApp vals) -> begin
    Printf.fprintf out "⟨∅, sqlite3_exec";
    List.iter (fun v -> Printf.fprintf out " "; fprintf_value out ~escape_html:escape_html v) vals;
    Printf.fprintf out "⟩"
  end
  | Clos (env, VFun (x, e)) -> begin
    Printf.fprintf out "⟨";
    fprintf_env out ~escape_html:escape_html env;
    Printf.fprintf out ", fun %s -&gt; %s⟩" x (string_of_expr e); (* FIXME maybe write fpritnf_expr *)
  end
  | Clos (env, VFix (f, x, e)) -> begin
    Printf.fprintf out "⟨";
    fprintf_env out ~escape_html:escape_html env;
    Printf.fprintf out ", fixfun %s %s -&gt; %s⟩" f x (string_of_expr e); (* FIXME maybe write fpritnf_expr *)
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
  | Clos (_, VSqliteExecPartialApp vals) -> begin
    Printf.sprintf "⟨∅, sqlite3_exec %s⟩" (String.concat " " (List.map (fun v -> string_of_value ~escape_html:escape_html v) vals))
  end
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

(** Representation to send variable bindings to server *)

(** [repr_of_value v] provides a unique, decodable string for [v] : [repr_of_value (value_of_repr sv) = sv] *)
let repr_of_value (v1 : value) : string = match v1 with
  | VInt n -> Printf.sprintf "%d" n
  | _ -> failwith "TODO"
(** [value_of_repr sv] decodes the string-encoded value [sv] : [value_of_repr (repr_of_value v) = v] *)
let value_of_repr (sv : string) : value = VInt (int_of_string sv) (* TODO generalize *)