open Utils
open Syntax

(** For genericity, extern functino can accept any value. But a function expecting a int has to match said value with a VInt, and in other cases, raises this error. *)
exception InvalidMlArgument of string

type raw_function_value =
  | VExternFunction of string * extern_function (* an extern function, for pretty-printing, has to have an associated _name_, generally the name of the function within the ml language (although maybe at some point we could want to use something else). *)
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
  | VLocation of string
and environment = value Environment.t
(** A pre-defined symbol is either a value, or a function from values to value with an arbitrary number of arguments. *)
and extern_function =
    Args1 of (value -> value)
  | Args2 of (value -> value -> value)
  | Args3 of (value -> value -> value -> value)
  | Args4 of (value -> value -> value -> value -> value)

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
  (* I'm not sure we really want the following cases to work. At least for [value_of_query], I can't think of a useful use case *)
  | Clos (_, VExternFunction (name, _)) -> raise (UnsupportedError "Trying to get expr of value of an external function. _TODO: ADD VARIABLES IN VALUES_")
  | Clos (env, VFun (x, e)) -> raise (UnsupportedError "TODO not sure it's supposed to work here (reminder, it's designed for value_of_query)")
  | Clos (env, VFix (f, x, e)) -> raise (UnsupportedError "TODO not sure it's supposed to work here (reminder, it's designed for value_of_query)")
  | _ -> raise (UnsupportedError "TODO not sure it's supposed to work here (reminder, it's designed for value_of_query)")

(** Pretty-printing *)

(** Correctly escapes characters for web rendering cf https://html.spec.whatwg.org/multipage/named-characters.html TODO do them all (?) *)
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
  | VLocation s -> Printf.fprintf out "to:%s" (web_of_string s)
  | VPure h -> if escape_html then Printf.fprintf out "%s" (web_of_string h) else Printf.fprintf out "%s" h (* FIXME not sure if useful since bypassed in `produce_page` *)
  | VContent l -> List.iter (fun v -> fprintf_value out ~escape_html:escape_html v) l
  | VCouple (v, v') -> begin
    Printf.fprintf out "(";
    fprintf_value out ~escape_html:escape_html v;
    Printf.fprintf out ",";
    fprintf_value out ~escape_html:escape_html v';
    Printf.fprintf out ")"
  end
  | Clos (_, VExternFunction (name, _)) -> Printf.fprintf out "%s" name
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
  if Environment.is_empty env then Printf.fprintf out "∅" else begin
    let fprintf_one_env_binding (prefix : string list) (x : variable) (v : value) : unit =
      (* Printf.fprintf out ", %s%s ↦ " prefix x; *)
      Printf.fprintf out ", %s ↦ " x;
      fprintf_value out ~escape_html:true v
    in
    Environment.iter fprintf_one_env_binding env
  end

let rec string_of_value ?(escape_html : bool = false) (v1 : value) : string = match v1 with
  | VDb db -> "adatabase" (* TODO !!!! *)
  | VInt n -> Printf.sprintf "%d" n
  | VBool b -> if b then "true" else "false"
  | VString f -> f
  | VLocation s -> Printf.sprintf "to:%s" (web_of_string s)
  | VPure h -> if escape_html then web_of_string h else h (* FIXME not sure if useful since bypassed in `produce_page` *)
  | VContent l -> List.fold_left (fun acc v -> Printf.sprintf "%s%s" acc (string_of_value ~escape_html:escape_html v)) "" l
  | VCouple (v, v') -> Printf.sprintf "(%s, %s)" (string_of_value ~escape_html:escape_html v) (string_of_value ~escape_html:escape_html v')
  | Clos (_, VExternFunction (name, _)) -> name
  | Clos (env, VFun (x, e)) -> Printf.sprintf "⟨%s, fun %s -> %s⟩" (string_of_env ~escape_html:escape_html env) x (string_of_expr e)
  | Clos (env, VFix (f, x, e)) -> Printf.sprintf "⟨%s, fixfun %s %s -> %s⟩" (string_of_env ~escape_html:escape_html env) f x (string_of_expr e)
and string_of_env ?(escape_html : bool = false) (env : environment) : string = if Environment.is_empty env then "∅" else begin
    let string_of_one_env_binding (prefix : string list) (x : variable) (v : value) (acc : string) : string = (* FIXME never prints prefixes *)
      if acc = "" then
        if List.is_empty prefix then
          Printf.sprintf "%s ↦ %s" x (string_of_value ~escape_html:true v)
        else
          Printf.sprintf "%s.%s ↦ %s" (String.concat "." (List.rev prefix)) x (string_of_value ~escape_html:true v)
      else
        if List.is_empty prefix then
          Printf.sprintf "%s ↦ %s, %s" x (string_of_value ~escape_html:true v) acc
        else
          Printf.sprintf "%s.%s ↦ %s, %s" (String.concat "." (List.rev prefix)) x (string_of_value ~escape_html:true v) acc
    in
    Environment.fold string_of_one_env_binding env ""
  end

(** Representation to send variable bindings to server *)

(** [repr_of_value v] provides a unique, decodable string for [v] : [repr_of_value (value_of_repr sv) = sv] *)
let repr_of_value (v1 : value) : string = match v1 with
  | VInt n -> Printf.sprintf "int:%d" n
  | VString s -> Printf.sprintf "string:%s" s
  | _ -> failwith "TODO implement repr_of_value for the remaining of the possible values"
(** [value_of_repr sv] decodes the string-encoded value [sv] : [value_of_repr (repr_of_value v) = v] *)
let value_of_repr (sv : string) : value =
  let new_str = List.hd (List.rev (String.split_on_char ':' sv)) in
  Printf.fprintf stderr "\n\n\nvalue_of_repr %s = %s\n\n\n" sv new_str;
  VString new_str (* TODO actually respect the spec + then generalize *)