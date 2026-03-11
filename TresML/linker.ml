(** The goal (in this file) is to replace every import with the corresponding page, embedded in the toplevel dynamic page. *)
open Utils
open Lexer
open Syntax
open Parser

exception LinkingError of string

let is_valid_module_name (potential_mod_name : string) : bool =
  match String.fold_left (fun _Id_state c -> eat_letter_lex_Identifier _Id_state c) (CouldBe ([], [])) potential_mod_name with
    | Is ((), _) -> true
    | _ -> false

(** [link_dynpage] and [link_expr] are similar to [linker]. Both assume [root_path] is a directory and that the path ends by a ['/'] (can also end by a ['\\'] on Windows). *)
let rec link_dynpage (root_path : string) (page : dynml_webpage) : dynml_webpage = List.map begin function
    | Pure s -> Pure s
    | Script e -> Script (link_expr root_path e)
    | Decl g -> begin match g with
      | ImportModule path ->
        if not (is_subfolder path) then
          raise (LinkingError (Printf.sprintf "%s: Access denied, cannot load file outside of project root. See --root option." (string_of_global_declaration (ImportModule path))))
        else begin
          let filename = filename_of_path path in
          let filename_without_ext = List.hd (String.split_on_char '.' filename) in
          let file_module_name = String.mapi (fun i c -> if i = 0 then Char.uppercase_ascii c else c) filename_without_ext in
          let imported_in = open_in (root_path ^ path) in
          let loaded_raw_page = read_whole_file_str imported_in in
          close_in imported_in;
          let loaded_lexed = lexer loaded_raw_page in
          let loaded_parsed = parser loaded_lexed in
          if is_valid_module_name file_module_name then
            Decl (Inserted ({module_name = file_module_name ; reset_environment = true ; final_env_available = true ; content_available = true}, link_dynpage root_path loaded_parsed))
          else
            raise (LinkingError "Invalid file name: should always start with an uppercase or lowercase ASCII letter and otherwise match identifier's lexic.")
        end
      | ExprDecl (x, e) -> Decl (ExprDecl (x, link_expr root_path e))
      | ModuleExprDecl (modu, x, e) -> Decl (ModuleExprDecl (modu, x, link_expr root_path e))
      | Inserted (mode, page) -> Printf.fprintf stderr "Inserted encoutered during linking"; Decl (Inserted (mode, link_dynpage root_path page))
      | _ -> Decl g
    end
  end page
and link_expr (root_path : string) (e : expr) : expr = match e with
  | Html page -> Html (link_dynpage root_path page)
  | Empty -> Empty
  | Let (x, e, in_body) -> Let (x, link_expr root_path e, link_expr root_path in_body)
  | Fun (x, body) -> Fun (x, link_expr root_path body)
  | Fix (f, x, body) -> Fix (f, x, link_expr root_path body)
  | App (e, e') -> App (link_expr root_path e, link_expr root_path e')
  | If (c, t, e) -> If (link_expr root_path c, link_expr root_path t, link_expr root_path e)
  | Seq (e, e') -> Seq (link_expr root_path e, link_expr root_path e')
  | Var x -> Var x
  | WithModule (modu, e) -> WithModule (modu, link_expr root_path e)
  | Couple (e, e') -> Couple (link_expr root_path e, link_expr root_path e')
  | Unit | String _ -> e (* TODO do that for other cases *)
  | Plus (e, e') -> Plus (link_expr root_path e, link_expr root_path e')
  | Minus (e, e') -> Minus (link_expr root_path e, link_expr root_path e')
  | Neg e -> Neg (link_expr root_path e)
  | Mult (e, e') -> Mult (link_expr root_path e, link_expr root_path e')
  | Div (e, e') -> Div (link_expr root_path e, link_expr root_path e')
  | Pow (e, e') -> Pow (link_expr root_path e, link_expr root_path e')
  | Int n -> Int n
  | Gt (e, e') -> Gt (link_expr root_path e, link_expr root_path e')
  | Lt (e, e') -> Lt (link_expr root_path e, link_expr root_path e')
  | Geq (e, e') -> Geq (link_expr root_path e, link_expr root_path e')
  | Leq (e, e') -> Leq (link_expr root_path e, link_expr root_path e')
  | Eq (e, e') -> Eq (link_expr root_path e, link_expr root_path e')
  | Neq (e, e') -> Neq (link_expr root_path e, link_expr root_path e')
  | And (e, e') -> And (link_expr root_path e, link_expr root_path e')
  | Or (e, e') -> Or (link_expr root_path e, link_expr root_path e')
  | Not e -> Not (link_expr root_path e)
  | Bool b -> Bool b
  | Concat (e, e') -> Concat (link_expr root_path e, link_expr root_path e')
  | Fstring lst -> Fstring ((List.map begin function
      | FstrString s -> FstrString s
      | FstrExpr e -> FstrExpr (link_expr root_path e)
    end) lst)

(** [linker root_path page = linked_page] links [page] with root directory of project [root_path].
  [linked_page] does not contain any [ImportModule] subexpression. *)
let linker (root_path : string) (page : dynml_webpage) : dynml_webpage = match separator_ended_dir_path_opt root_path with
  | None -> raise (LinkingError (Printf.sprintf "%s: Root path is not a directory." root_path))
  | Some dir_root_path -> link_dynpage dir_root_path page