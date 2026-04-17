open Utils
open Syntax
open TypeSyntax

(** For genericity, extern function can accept any value. But a function expecting a int has to match said value with a VInt, and in other cases, raises this error. *)
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
  | VUnit
  | VPure of string
  | VContent of value list
  | VCouple of value * value
  | VLocation of string
and environment = value Environment.t
(** A pre-defined function is a function from values to value with 1, 2, 3 or 4 arguments.
  The first argument is an reset environment: for instance if the evaluation of the function
  necessitate to evaluate an inserted page, this environment is passed if this inserted page
  requires a reset environment. Can happend for instance if the extern function is higher-order
  (takes a function in argument). *)
and extern_function =
    Args1 of (environment -> value -> value)
  | Args2 of (environment -> value -> value -> value)
  | Args3 of (environment -> value -> value -> value -> value)
  | Args4 of (environment -> value -> value -> value -> value -> value)

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

(** If [f] is a function, [straightforward_fun_dropping_reset_env f] is the function that takes the same argument + a reset environment
  and makes no use of this environment, otherwise has the same semantic as [f].
  Is used to construct extern_function that does not require a reset environment e.g. first-order function. *)
let straightforward_fun_dropping_reset_env (f : 'a) : environment -> 'a =
  fun (_ : environment) -> f

(** Pretty-printing *)

(** Correctly escapes characters for web rendering cf https://html.spec.whatwg.org/multipage/named-characters.html TODO do them all (?) *)
let web_of_string (s : string) : string =
  let rec web_of_string_acc (s : string) (i : int) (n : int) (acc : char list) : char list = if i < n then begin match s.[i] with
      | '&' ->  web_of_string_acc s (i+1) n (';' :: 'p' :: 'm' :: 'a' :: '&' :: acc)
      | '<' ->  web_of_string_acc s (i+1) n (';' :: 't' :: 'l' :: '&' :: acc)
      | '>' ->  web_of_string_acc s (i+1) n (';' :: 't' :: 'g' :: '&' :: acc)
      | '"' ->  web_of_string_acc s (i+1) n (';' :: 't' :: 'o' :: 'u' :: 'q' :: '&' :: acc)
      | '\'' -> web_of_string_acc s (i+1) n (';' :: 's' :: 'o' :: 'p' :: 'a' :: '&' :: acc)
      | '`' -> web_of_string_acc s (i+1) n (';' :: 'e' :: 'v' :: 'a' :: 'r' :: 'g' :: '&' :: acc)
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
  | VPure h -> if escape_html then Printf.fprintf out "%s" (web_of_string h) else Printf.fprintf out "%s" h (* FIXME not sure if useful since bypassed in `output_page` *)
  | VContent l -> List.iter (fun v -> fprintf_value out ~escape_html:escape_html v) l
  | VCouple (v, v') -> begin
    Printf.fprintf out "(";
    fprintf_value out ~escape_html:escape_html v;
    Printf.fprintf out ",";
    fprintf_value out ~escape_html:escape_html v';
    Printf.fprintf out ")"
  end
  | VUnit -> Printf.fprintf out "()"
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
      Printf.fprintf out ", %s%s ↦ " (if prefix = [] then "" else List.fold_left (fun acc modu -> Printf.sprintf "%s%s." acc modu) "" prefix) x; (* TODO fix!!!!!!! see string_of_env*)
      fprintf_value out ~escape_html:true v
    in
    Environment.iter fprintf_one_env_binding env
  end

let rec string_of_value ?(escape_html : bool = false) (v1 : value) : string = match v1 with
  | VDb db -> "adatabase" (* TODO !!!! *)
  | VInt n -> Printf.sprintf "%d" n
  | VBool b -> if b then "true" else "false"
  | VString s -> if escape_html then web_of_string s else s
  | VLocation s -> Printf.sprintf "to:%s" (web_of_string s)
  | VPure h -> h
  | VContent l -> List.fold_left (fun acc v -> Printf.sprintf "%s%s" acc (string_of_value ~escape_html:escape_html v)) "" l
  | VCouple (v, v') -> Printf.sprintf "(%s, %s)" (string_of_value ~escape_html:escape_html v) (string_of_value ~escape_html:escape_html v')
  | VUnit -> "()"
  | Clos (_, VExternFunction (name, _)) -> name
  | Clos (env, VFun (x, e)) -> Printf.sprintf "⟨%s, fun %s -> %s⟩" (string_of_env ~escape_html:escape_html env) x (string_of_expr e)
  | Clos (env, VFix (f, x, e)) -> Printf.sprintf "⟨%s, fixfun %s %s -> %s⟩" (string_of_env ~escape_html:escape_html env) f x (string_of_expr e)
and string_of_env ?(escape_html : bool = false) (env : environment) : string = if Environment.is_empty env then "∅" else begin
    let string_of_one_env_binding (prefix : string list) (x : variable) (v : value) (acc : string) : string = (* FIXME never prints prefixes *)
      if acc = "" then
        if List.is_empty prefix then
          Printf.sprintf "%s ↦ %s" x (string_of_value ~escape_html:escape_html v)
        else
          Printf.sprintf "%s.%s ↦ %s" (String.concat "." (List.rev prefix)) x (string_of_value ~escape_html:escape_html v)
      else
        if List.is_empty prefix then
          Printf.sprintf "%s ↦ %s, %s" x (string_of_value ~escape_html:escape_html v) acc
        else
          Printf.sprintf "%s.%s ↦ %s, %s" (String.concat "." (List.rev prefix)) x (string_of_value ~escape_html:escape_html v) acc
    in
    Environment.fold string_of_one_env_binding env ""
  end

(** Representation to send variable bindings to server *)

(** [repr_of_value v] provides a unique, decodable string for [v] : [repr_of_value (value_of_repr sv) = sv] *)
let rec repr_of_value (v1 : value) : string = match v1 with
  | VInt n -> Printf.sprintf "int:%d" n
  | VString s -> Printf.sprintf "string:%s" (String.escaped s)
  | VBool true -> Printf.sprintf "bool:t"
  | VBool false -> Printf.sprintf "bool:f"
  | VUnit -> "unit:"
  | VPure s -> Printf.sprintf "pure:%s" s
  | VContent l -> failwith "TODO implement actual parser for complex type (content & couples)"(*Printf.sprintf "content:%s" (List.fold_left (fun acc v -> Printf.sprintf "%s;%s" acc (repr_of_value v)) "" l)*)
  | VCouple (v, v') -> failwith "TODO implement actual parser for complex type (content & couples)"(*Printf.sprintf "couple:%s;%s" (repr_of_value v) (repr_of_value v')*)
  | Clos _ | VDb _ | VLocation _ -> raise (Invalid_argument (Printf.sprintf "repr_of_value %s: Unsupported representation for closures and databases." (string_of_value v1)))
(** [value_of_repr sv] decodes the string-encoded value [sv] : [value_of_repr (repr_of_value v) = v] *)
let rec value_of_repr (sv : string) : value * ml_type =
  let kind, value = match String.split_on_char ':' sv with
    | [t; v] -> t, v
    | _ -> raise (Invalid_argument (Printf.sprintf "value_of_repr %s: is not of the form kind:value_representation" sv))
  in
  match kind with
    | "int" -> VInt (int_of_string value), TypeInt
    | "string" -> VString (Scanf.unescaped value), TypeString
    | "bool" -> if value = "t" then (VBool true, TypeBool) else (VBool false, TypeBool)
    | "unit" -> (VUnit, TypeUnit)
    | "pure" -> (VPure value, TypeHtml)
    | "content" -> failwith "TODO implement actual parser for complex type (content & couples)"
    | "couple" ->failwith "TODO implement actual parser for complex type (content & couples)"
    (* begin match (String.split_on_char ';' value) with
      | [v_repr1; v_repr2] -> VCouple (value_of_repr v_repr1, value_of_repr v_repr2)
      | _ -> raise (Invalid_argument "value_of_repr %s: Malformed couple representation." sv)
    end *)
    | _ -> raise (Invalid_argument (Printf.sprintf "value_of_repr %s: Unrecognized kind of representation." sv))
  (* Printf.fprintf stderr "\n\n\nvalue_of_repr %s = %s\n\n\n" sv new_str; *)