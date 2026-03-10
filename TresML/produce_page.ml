open Utils
open Lexer
open Syntax
open Parser
open TypeSyntax
open Typechecker
open Interpreter

(** Defining extern functions *)

type extern_symbol = {namespaces : module_name list ; name : string ; v : extern_function ; tau : ml_type}

let ml_type_of_sqlite_exec =
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

(** [straightforward_ml_function (Argsi f)] (with [i = 1,2,3,4]) is a value [v] corresponding to the ml function that reflects [f : value -> value]*)
let straigthforward_ml_function (name : string) (f : extern_function) : value =
  Clos (Environment.empty, VExternFunction (name, f))

(** List of predefined symbols, pre-loaded in the environment at execution. *)
let predefined_symbols = [
      {namespaces = [sqlite_module_name] ; name = "exec"    ; v = Args4 extern_sqlite_exec ; tau = ml_type_of_sqlite_exec}
    ; {namespaces = [sqlite_module_name] ; name = "opendb"  ; v = Args1 extern_sqlite_open_db ; tau = Arr (TypeString, TypeDb)}
    ; {namespaces = [sqlite_module_name] ; name = "closedb" ; v = Args1 extern_sqlite_close_db ; tau = Arr (TypeDb, TypeBool)}
    ; {namespaces = [] ; name = "fst" ; v = Args1 ml_fst ; tau = TypeForall ("'fst", TypeForall ("'snd", Arr (Prod (TypeVar "'fst", TypeVar "'snd"), TypeVar "'fst")))}
    ; {namespaces = [] ; name = "snd" ; v = Args1 ml_snd ; tau = TypeForall ("'fst", TypeForall ("'snd", Arr (Prod (TypeVar "'fst", TypeVar "'snd"), TypeVar "'snd")))}
    ; {namespaces = [] ; name = "not" ; v = Args1 ml_not ; tau = Arr (TypeBool, TypeBool)}
    ; {namespaces = ["String"] ; name = "replace" ; v = Args3 ml_string_replace ; tau = Arr (TypeString, Arr (TypeString, Arr (TypeString, TypeString)))}
    ; {namespaces = ["String"] ; name = "get" ; v = Args2 ml_string_get ; tau = Arr (TypeString, Arr (TypeInt, TypeString))} (* FIXME add char type *)
    ; {namespaces = ["Http"] ; name = "redirect" ; v = Args1 ml_redirect ; tau = Arr (TypeString, TypeUnit)} (* FIXME GIVE AN ACTUAL TYPE TO LOCATIONS !!!! *)
  ]

let pre_included_environment : environment =
  List.fold_left
    (fun acc entry -> Environment.add_and_path entry.namespaces entry.name (straigthforward_ml_function entry.name entry.v) acc)
    Environment.empty
    predefined_symbols

let pre_included_typing_env : modular_typing_environment =
  List.fold_left
    (fun acc entry -> Environment.add_and_path entry.namespaces entry.name entry.tau acc)
    Environment.empty
    predefined_symbols

(** Producing a page *)

let debug = true

let displayed = ["raw"; "lexed"; "parsed"; "typed"; "eval'd"]

(** [intepret_tml_page init_env source handle_output output_first_line] reads a TresML webpage from [source], computes the resulting html webpage and progressively pass the resulting page to the function [handle_ouput]. A canonical use case is providing [Printf.fprintf a_channel] as [handle_ouput].
  [init_env] is the initial evaluation environment /!\ TODO for now, can only contain string values.
  [output_first_line] is a boolean indicating whether to indicate information destined to the server in the first (e.g. session variables, redirection, ...). *)
let intepret_tml_page (arguments : environment) (source : in_channel) (handle_output : string -> unit) (output_first_line : bool) : unit =
  failwith "TODO"

(** [output_page init_env source dest output_first_line] reads a TresML webpage from [source], computes the resulting html webpage and writes it in [dest].
  [init_env] is the initial evaluation environment /!\ TODO for now, can only contain string values.
  [output_first_line] is a boolean indicating whether to indicate information destined to the server in the first (e.g. session variables, redirection, ...). *)
let rec output_page (arguments : environment) (source : in_channel) (dest : out_channel) (output_first_line : bool) : unit =
  let f_in = source in
  let code : string = read_whole_file_str f_in in
  close_in f_in;
  try
    let lexed = lexer code in
    let parsed = parser lexed in
    (* Printf.fprintf stderr "AAAAAAAAAHHHHHHHHH - A\n"; *)
    (* {namespaces = [] ; name = "include" ; v = Args1 extern_include ; tau = Arr (TypeString, TypeHtml)} *)
    let typ_env = Environment.add "include" (Arr (TypeString, TypeHtml)) (Environment.disjoint_union (Environment.map (fun _ -> TypeString) arguments) pre_included_typing_env) in
    let eval_env = Environment.add "include" begin
        straigthforward_ml_function "include"
        (Args1 begin
          fun v -> match v with
            | VString path -> begin
              (if not (try Sys.is_directory ".tresml_cache" with Sys_error _ -> false) then
                Sys.mkdir ".tresml_cache" 0b111_000_000);
              let included_channel_in = open_in path in
              let included_channel_out = open_out ".tresml_cache/tmp.html" in
              output_page Environment.empty included_channel_in included_channel_out false;(* for now, Environment.empty is passed; see in uses cases if it should be given the same original pre-loaded environment*)
              close_in included_channel_in;
              close_out included_channel_out;
              let included_res_in = open_in ".tresml_cache/tmp.html" in
              let evald_included = read_whole_file_str included_res_in in
              close_in included_res_in;
              Sys.remove ".tresml_cache/tmp.html";
              (try
                Sys.remove ".tresml_cache"
              with
                Sys_error _ -> failwith "tried to delete .tresml_cache while it's not empty during execution of an include"
              );
              VPure evald_included
            end
            | _ -> raise (InvalidMlArgument (Printf.sprintf "%s: string expected." (string_of_value v)))
        end) 
      end (Environment.disjoint_union arguments pre_included_environment) in
    (* Printf.fprintf stderr "AAAAAAAAAHHHHHHHHH - B\n"; *)
    let _ = type_inferer typ_env parsed in
    (* Printf.fprintf stderr "ENTERING EVALUATION\n"; *)
    let final_env, location, values = eval eval_env parsed in
    (* Printf.fprintf stderr "EXITING EVALUATION\n"; *)
    let f_out = dest in
    (* The first line contains information we want to send to the server, but that won't be sent to the client. *)
    if output_first_line then begin
      (* TODO slight optimization: do not re-send session variables that were not modified; but simply mention to the server to keep them *)
      let session_bindings = match Environment.submap_opt session_module_name final_env with
        | None -> ""
        | Some session_env -> Environment.fold
          (fun prefixes x v acc ->
            if List.is_empty prefixes then begin (* for now, only top-level within module Session, could change for further needs *)
              (if debug then Printf.fprintf stderr "trying to repr: %s" (string_of_value v));
              Printf.sprintf "%s&%s=%s" acc x (repr_of_value v)
            end else
              acc
          )
          session_env ""
      in
      Printf.fprintf f_out "session%s;" session_bindings; (* FIXME may lead to issues if ';' occurs in a session variable *)
      begin match location with
        | None -> ()
        | Some target -> Printf.fprintf f_out "redirect=%s" target
      end;
      Printf.fprintf f_out "\n"
    end;
    List.iter (fun v -> fprintf_value f_out v) values;
    close_out f_out
  with
    | PrelexingError s -> let f_out = dest in Printf.fprintf f_out ";\nPrelexingError: %s\n" s; close_out f_out
    | LexingError s -> let f_out = dest in Printf.fprintf f_out ";\nLexingError: %s\n" s; close_out f_out
    | ParsingError s -> let f_out = dest in Printf.fprintf f_out ";\nParsingError: %s\n" s; close_out f_out
    | TypingError s -> let f_out = dest in Printf.fprintf f_out ";\nTypingError: %s\n" s; close_out f_out
    | UnificationError (alpha, beta, Recursive) -> let f_out = dest in Printf.fprintf f_out ";\nUnificationError: %s and %s recursive.\n" (string_of_ml_type alpha) (string_of_ml_type beta); close_out f_out
    | UnificationError (alpha, beta, Incompatible) -> let f_out = dest in Printf.fprintf f_out ";\nUnificationError: %s and %s incompatible.\n" (string_of_ml_type alpha) (string_of_ml_type beta); close_out f_out
    | InterpreterError s -> let f_out = dest in Printf.fprintf f_out ";\nInterpreterError: %s\n" s; close_out f_out
    | UnsupportedError s -> let f_out = dest in Printf.fprintf f_out ";\nUnsupportedError: %s\n" s; close_out f_out
  
let test_file (source : string) : unit =
  let f_in = open_in source in
  let code : string = read_whole_file_str f_in in
  close_in f_in;
  Test.test (-1, code)

exception MalformedCommandLine

(** Organized and named arguments obtained from the command line. *)
type command_line_args = {
    source : in_channel (* The source code *)
  ; target : out_channel (* Where to output the resulting webpage *)
  ; initial_environment : environment (* The environment initially obtained directly from the command line *)
  ; output_first_line : bool (* Whether or not to output the first line, indicating information to the server e.g. session variables. *)
}

(** [parse_command_line command_line = (source, dest, env)] parses the command line and retrieves the path to the source file [source], the path to the destination [dest] and the environment passed in argument [env] e.g. Post/Get/Session variables.
  Raises [MalformedCommandLine] if [command_line] is ill-formed. *)
let parse_command_line (command_line : string array) : command_line_args =
  let n = Array.length command_line in
  if n > 2 then
    let source_path = command_line.(1) in
    let dest_path = command_line.(2) in
    let source_fd =
      if source_path = "-stdin" then
        stdin
      else
        open_in source_path
    in
    let dest_fd =
      if dest_path = "-stdout" then
        stdout
      else
        open_out dest_path
    in
    let rec parse_options (command_line : string array) (i : int) (env_acc : environment) (output_first_line_acc : bool) : environment * bool =
      if i < n then
        match String.lowercase_ascii command_line.(i) with
          | "-argrepr" -> begin if i + 1 >= n then (* The remaining of the command line is supposed to be of the form `<argoption> <arguments> ...` *)
              raise MalformedCommandLine
            else
              parse_options command_line (i + 2) (Parse_url_dictionary.parse_url_dictionary value_of_repr command_line.(i + 1) env_acc) output_first_line_acc
          end
          | "-argstr" -> begin if i + 1 >= n then (* The remaining of the command line is supposed to be of the form `<argoption> <arguments> ...` *)
              raise MalformedCommandLine
            else
              parse_options command_line (i + 2) (Parse_url_dictionary.parse_url_dictionary (fun s -> VString s) command_line.(i + 1) env_acc) output_first_line_acc
          end
          | "-noserverdata" -> parse_options command_line (i + 1) env_acc false
          | _ -> raise MalformedCommandLine
      else
        env_acc, output_first_line_acc
    in
    let initial_env, output_first_line = parse_options command_line 3 Environment.empty true in
    { source =  source_fd ; target = dest_fd ; initial_environment = initial_env ; output_first_line = output_first_line }
  else
    raise MalformedCommandLine

let () =
  try
    let cl_args = parse_command_line Sys.argv in
    output_page cl_args.initial_environment cl_args.source cl_args.target cl_args.output_first_line;
    close_in cl_args.source;
    close_out cl_args.target
  with
    MalformedCommandLine -> begin
      let help_message_path = "./help_message.txt" in
      let f_help_msg = open_in help_message_path in
      let help_message = read_whole_file_str f_help_msg in
      Printf.printf "%s\n" help_message
    end