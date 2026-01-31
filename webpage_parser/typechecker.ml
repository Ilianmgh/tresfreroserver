open Utils
open Lexic
open Lexer
open Syntax
open TypeSyntax

let debug = false

type unification_error_type = Incompatible | Recursive

exception UnificationError of ml_type * ml_type * unification_error_type

exception TypingError of string

type type_substitution = ml_type StringMap.t

(** [apply_substitution alpha theta] applies the substitution [theta] to the type [alpha] *)
let rec apply_substitution (alpha : ml_type) (theta : type_substitution) : ml_type = match alpha with
  | Arr (alpha, beta) -> Arr (apply_substitution alpha theta, apply_substitution beta theta)
  | Prod (alpha, beta) -> Prod (apply_substitution alpha theta, apply_substitution beta theta)
  | TypeInt -> TypeInt
  | TypeBool -> TypeBool
  | TypeString -> TypeString
  | TypeDb -> TypeDb
  | TypeUnit -> TypeUnit
  | TypeHtml -> TypeHtml
  | TypeVar s -> begin match StringMap.find_opt s theta with
    | Some tau -> tau
    | None -> TypeVar s
  end

(** [occurs x tau = true] iff [x] occurs in [tau theta] *)
let rec occurs (var : type_variable) (tau : ml_type) (theta : type_substitution) : bool =
  let rec occurs_no_substitution (var : type_variable) (tau : ml_type) : bool = match tau with
    | Arr (alpha, beta) | Prod (alpha, beta) -> occurs_no_substitution var alpha || occurs_no_substitution var beta
    | TypeInt | TypeBool | TypeString | TypeUnit | TypeHtml | TypeDb -> false
    | TypeVar s -> (s = var)
  in occurs_no_substitution var (apply_substitution tau theta)

(** Type unification. Returns a minimal substitution [theta] s.t. [alpha theta = beta theta] *)
let unify (alpha : ml_type) (beta : ml_type) : type_substitution =
  if debug then Printf.fprintf stderr "Unifying %s --- %s\n%!" (string_of_ml_type alpha) (string_of_ml_type beta); (* TODO replace by breadth-first exploration of the types to unify *)
  let rec update_substitution (x : variable) (tau : ml_type) (theta : type_substitution) = match StringMap.find_opt x theta with
    | None -> StringMap.add x tau theta
    | Some tau' -> let theta_taus = unify_aux tau tau' theta in StringMap.add x (apply_substitution tau theta_taus) theta_taus
  and unify_aux (alpha : ml_type) (beta : ml_type) (theta : type_substitution) : type_substitution = match alpha, beta with
    | Arr (alpha, beta), Arr (alpha', beta') ->
      let theta' = unify_aux alpha alpha' theta in
      unify_aux beta beta' theta'
    | Prod (alpha, beta), Prod (alpha', beta') ->
      let theta' = unify_aux alpha alpha' theta in
      unify_aux beta beta' theta'
    | TypeInt, TypeInt | TypeBool, TypeBool | TypeString, TypeString | TypeUnit, TypeUnit | TypeHtml, TypeHtml | TypeDb, TypeDb -> theta
    | TypeVar s1, TypeVar s2 -> if s1 = s2 then theta else begin update_substitution s1 (TypeVar s2) theta end
    | TypeVar s, tau | tau, TypeVar s -> if occurs s tau theta then raise (UnificationError (alpha, beta, Recursive)) else update_substitution s tau theta
    | _, _ -> raise (UnificationError (alpha, beta, Incompatible)) (* TODO replace by hoisting the error to display "thingy should have type stuff but is of type otherstuff"*)
  in unify_aux alpha beta StringMap.empty

(** [update_typing_env gamma theta] returns [gamma'] updating [gamma] s.t. [gamma'(x) = gamma(x) theta]
Remark: If type variable ['a] is bound in [theta], then there are no more occurences of ['a] in the image of [gamma'] *)
let update_typing_env (gamma : typing_environment) (theta : type_substitution) : typing_environment = StringMap.map (fun alpha -> apply_substitution alpha theta) gamma

(** [update_typing_env_equalizing gamma alpha beta] updates [gamma] to a minimal typing environment compatible with [alpha = beta] *)
let update_typing_env_equalizing (gamma : typing_environment) (alpha : ml_type) (beta : ml_type) : typing_environment = update_typing_env gamma (unify alpha beta)

(** [type_inferer_one_expr Γ e = α] iff types variables in Γ can be refined to obtain a Γ' such that Γ' ⊢ e : α *)
let rec type_inferer_one_expr (gamma : typing_environment) (e1 : expr) : typing_environment * ml_type = match e1 with
  | Empty -> assert false
  | Let (x, e, e') ->
    let gamma', alpha = type_inferer_one_expr gamma e in
    type_inferer_one_expr (StringMap.add x alpha gamma') e'
  | Fun (x, e) -> let gamma', beta = type_inferer_one_expr (StringMap.add x (TypeVar (fresh ())) gamma) e in
    (gamma', Arr (StringMap.find x gamma', beta))
  | Fix (f, x, e) ->
    let vartype_x = fresh () in
    let vartype_ret = fresh () in
    let gamma_x = StringMap.add x (TypeVar vartype_x) gamma in
    let gamma_x_f =  StringMap.add f (Arr (TypeVar vartype_x, TypeVar vartype_ret)) gamma_x in
    let final_gamma, beta = type_inferer_one_expr gamma_x_f e in
    (final_gamma, Arr (StringMap.find x final_gamma, beta))
  | App (e, e') ->
    let gamma', func_type = type_inferer_one_expr gamma e in
    let gamma'', func_arg = type_inferer_one_expr gamma e' in
    begin match func_type, func_arg with
      | Arr (alpha, beta), alpha' ->
        let theta = unify alpha alpha' in
        assert ((apply_substitution alpha theta) = (apply_substitution alpha' theta)); (* TODO: erase this line once convinced it's working *)
        (update_typing_env gamma'' theta, apply_substitution beta theta)
      | _, _ -> raise (TypingError (Printf.sprintf "%s: this is not a function, it cannot be applied." (string_of_expr (App (e, e'))))) (* TODO underline the function *)
    end
  | If (c, t, e) -> begin
      let gamma', tau = type_inferer_one_expr gamma c in
      let theta = unify TypeBool tau in
      let gamma'', t_type = type_inferer_one_expr (update_typing_env gamma' theta) t in
      let gamma''', e_type = type_inferer_one_expr gamma'' e in
      let theta' = unify t_type e_type in
      assert ((apply_substitution t_type theta') = (apply_substitution e_type theta')); (* TODO: erase this line once convinced it's working *)
      (update_typing_env gamma''' theta', apply_substitution t_type theta')
  end
  | Seq (e, e') -> begin
    let gamma', tau = type_inferer_one_expr gamma e in
    try
      let theta = unify TypeUnit tau in
      type_inferer_one_expr (update_typing_env gamma' theta) e'
    with
      UnificationError _ -> Printf.fprintf stderr "%s: is expected to have type unit." (string_of_expr (Seq (e, e'))); type_inferer_one_expr gamma' e'
  end
  | Html h -> type_inferer gamma h; (gamma, TypeHtml) (* TODO see how to update only the non-globals (a global declaration should not affect in any way outside the code of [h]) *)
  | Var x -> begin match StringMap.find_opt x gamma with
    | Some t -> gamma, t
    | None -> raise (TypingError (Printf.sprintf "%s: undefined variable." (string_of_expr (Var x)))) (* TODO actually, shouldn't be a _typing_ error per se *)
  end 
  | Couple (e, e') ->
    let gamma', alpha = type_inferer_one_expr gamma e in
    let gamma'', beta = type_inferer_one_expr gamma' e' in
    (gamma'', Prod (alpha, beta))
  | Fst -> let alpha, beta = fresh (), fresh () in (gamma, Arr (Prod (TypeVar alpha, TypeVar beta), TypeVar alpha))
  | Snd -> let alpha, beta = fresh (), fresh () in (gamma, Arr (Prod (TypeVar alpha, TypeVar beta), TypeVar beta))
  | SqliteOpenDb -> (gamma, Arr (TypeString, TypeDb))
  | SqliteCloseDb -> (gamma, Arr (TypeDb, TypeBool))
  (* Type of sqlite_exec: db -> (html -> html -> html) -> (html -> string -> string -> html) -> string -> html *)
  | SqliteExec -> (gamma,
    Arr (
      TypeDb,
      Arr (
        Arr (TypeHtml, Arr (TypeHtml, TypeHtml)),
        Arr (
          Arr (TypeHtml, Arr (TypeString, Arr (TypeString, TypeHtml))),
          Arr (TypeString,
            TypeHtml
          )
        )
      )
    )
  ) (* FIXME FOR NOW, RETURN VALUE IS A STRING, BUT IS ACTUALLY A DEDICATED TYPE FOR ERRORS *)
  | Neg e -> begin
    let gamma', alpha = type_inferer_one_expr gamma e in
    let theta = unify TypeInt alpha in
    (update_typing_env gamma' theta, TypeInt)
  end
  | Plus (e, e') | Minus (e, e') | Mult (e, e') | Div (e, e') | Pow (e, e') -> begin
    let gamma', alpha = type_inferer_one_expr gamma e in
    let gamma'', beta = type_inferer_one_expr gamma' e' in
    let theta = unify alpha beta in
    let t_int = apply_substitution alpha theta in 
    assert (t_int = (apply_substitution beta theta)); (* TODO: erase this line once convinced it's working *)
    match t_int with
      | TypeInt -> (update_typing_env gamma'' theta, TypeInt)
      | _ -> raise (TypingError (Printf.sprintf "%s: This expression has type %s but is expected to have type %s." (string_of_expr e) (string_of_ml_type t_int) (string_of_ml_type TypeInt))) (* TODO keep replacing those tests by unification *)
  end
  | Int n -> (gamma, TypeInt)
  | Gt (e, e') | Lt (e, e') | Geq (e, e') | Leq (e, e') | Eq (e, e') | Neq (e, e') -> begin
    let gamma', alpha = type_inferer_one_expr gamma e in
    let gamma'', beta = type_inferer_one_expr gamma' e' in
    let theta = unify alpha beta in
    assert ((apply_substitution alpha theta) = (apply_substitution beta theta)); (* TODO: erase this line once convinced it's working *)
    (update_typing_env gamma'' theta, TypeBool)
  end
  | Not e -> begin
    let gamma', alpha = type_inferer_one_expr gamma e in
    let theta = unify TypeBool alpha in
    (update_typing_env gamma' theta, TypeBool)
  end
  | And (e, e') | Or (e, e') -> begin
    let gamma', alpha = type_inferer_one_expr gamma e in
    let gamma'', beta = type_inferer_one_expr gamma' e' in
    let theta = unify alpha beta in
    let t_bool = apply_substitution alpha theta in 
    assert (t_bool = (apply_substitution beta theta)); (* TODO: erase this line once convinced it's working *)
    match t_bool with
      | TypeBool -> (update_typing_env gamma'' theta, TypeBool)
      | _ -> raise (TypingError (Printf.sprintf "%s: This expression has type %s but is expected to have type %s." (string_of_expr e) (string_of_ml_type t_bool) (string_of_ml_type TypeBool)))
  end
  | Bool b -> gamma, TypeBool
  | Concat (e, e') -> begin
    let gamma', alpha = type_inferer_one_expr gamma e in
    let gamma'', beta = type_inferer_one_expr gamma' e' in
    let theta = unify alpha beta in
    let t_unif = apply_substitution alpha theta in 
    assert (t_unif = (apply_substitution beta theta)); (* TODO: erase this line once convinced it's working *)
    let theta' = unify t_unif TypeString in (* FIXME check if it works + if it does apply it verywhere else (&&, ||, +, * and so on...) *)
    let t_str = apply_substitution t_unif theta' in 
    match t_str with
      | TypeString -> (update_typing_env gamma'' theta, TypeString)
      | _ -> raise (TypingError (Printf.sprintf "%s: This expression has type %s but is expected to have type %s." (string_of_expr e) (string_of_ml_type t_str) (string_of_ml_type TypeString))) (* TODO keep replacing those tests by unification *)
  end
  | String s | Fstring s -> gamma, TypeString

and type_inferer (gamma : typing_environment) (page : dynml_webpage) : (typing_environment * ml_type) list =
  let types_and_env = List.fold_left begin fun already_typed element -> begin match already_typed with
      | [] -> assert false
      | (cur_gamma, last_gamma, tau) :: already_typed' -> begin match element with
        | Script e -> let gamma_e, tau_e = type_inferer_one_expr cur_gamma e in (cur_gamma, gamma_e, tau_e) :: (cur_gamma, last_gamma, tau) :: already_typed'
        | Pure s -> (cur_gamma, StringMap.empty, TypeHtml) :: (cur_gamma, last_gamma, tau) :: already_typed'
        | Decl (ExprDecl (x, e)) -> let gamma_e, tau_e = type_inferer_one_expr cur_gamma e in (StringMap.add x tau_e cur_gamma, gamma_e, tau_e) :: (cur_gamma, last_gamma, tau) :: already_typed'
        | Decl (TypeDecl (x, e)) -> failwith "TODO"
      end
    end
  end [(gamma, gamma, TypeBool)] page (* TODO remove this first one *)
  in
  List.map (fun (x, y, z) -> (y, z)) types_and_env

(** Pretty-printing *)

let string_of_typing_env (gamma : typing_environment) =
  let string_of_type_binding (x : variable) (alpha : ml_type) (acc : string) : string =
    if acc = "" then
      Printf.sprintf "%s : %s" x (string_of_ml_type alpha)
    else
      Printf.sprintf "%s : %s, %s" x (string_of_ml_type alpha) acc
  in
  StringMap.fold string_of_type_binding gamma ""