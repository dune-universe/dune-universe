type t = 
  | Bool of bool
  | Int31 of int
  | Int63 of int64
  | Int32 of int32
  | Int64 of int64
  | Nativeint32 of int32
  | Nativeint64 of int64
  | Float of float
  | Char of char
  | String of string
  | List of t list
  | Array of t list
  | Variant of string * t option
  | Poly_variant of string * t option
  | Record of (string * t) list
  | Object of (string * t) list
  | Tuple of t list
  | Unit
  | IntGen of string * char

  | Var of Longident.t
  | App of t * ((bool * string) option * t) list
  | Seq of t list (** t; t; t; .. *)

type ocaml = t

open Format

let format_sprintf fmt =
  let buf = Buffer.create 100 in
  let ppf = formatter_of_buffer buf in
  kfprintf (fun ppf -> pp_print_flush ppf (); Buffer.contents buf) ppf fmt

let rec format_list (sep : (unit, formatter, unit) format)  f ppf = function
  | [] -> ()
  | [x] -> f ppf x
  | x::xs -> 
      fprintf ppf "@[%a@]%t%a" 
        f x
        (fun ppf -> fprintf ppf sep)
        (format_list sep f) xs

let escape_string s =
  let b = Buffer.create @@ String.length s * 2 in
  let needed_escape = ref false in
  String.iter (fun c ->
      let code = Char.code c in
      if code >= 128 || (32 <= code && code <= 126 && c <> '"' && c <> '\\') then Buffer.add_char b c
      else 
        let escape s = needed_escape := true; Buffer.add_string b s in
        match c with
        | '\008' ->  escape "\\b"
        | '\009' ->  escape "\\t"
        | '\010' ->  escape "\\n"
        | '\013' ->  escape "\\r"
        | '"' ->     escape "\\\""
        | '\\' ->    escape "\\\\"
        | _ -> Buffer.add_char b c) s;
  if !needed_escape then Buffer.contents b else s

let classify_symbol = function
  | "" -> `none
  | "asr" | "land" | "lor" | "lsl" | "lsr" | "lxor" | "mod" | "or" -> `infix
  | s -> 
      match s.[0] with
      | '=' | '<' | '>' | '@' | '^' | '|' | '&' | '+' | '-' | '*' | '/' | '$' | '%' | '#' -> `infix
      | '!' | '?' | '~' -> `prefix
      | 'A' .. 'Z' -> `alpha
      | 'a' .. 'z' -> `alpha
      | '_' -> `alpha
      | _ -> `none
    
let format_lident ppf lid =
  let format_name ppf s = match classify_symbol s with
    | `prefix | `infix -> Format.fprintf ppf "(%s)" s
    | `alpha -> Format.pp_print_string ppf s
    | `none -> assert false
  in
  let rec f ppf = function
    | Longident.Lident s -> format_name ppf s
    | Ldot (lid, s) -> Format.fprintf ppf "%a.%a" f lid format_name s
    | Lapply (l1, l2) -> Format.fprintf ppf "%a(%a)" f l1 f l2
  in
  f ppf lid

let gen_format no_poly ppf v =
  let f = Format.fprintf in
  let string = pp_print_string in
  let rec format ppf = function
    | Bool b -> f ppf "%b" b
    | Int31 i -> f ppf "%d (* int *)" i
    | Int63 i -> f ppf "%Ld" i
    | Int32 i -> f ppf "(* int32 *) %ldl (* int32 *)" i
    | Int64 i -> f ppf "(* int64 *) %LdL" i
    | Nativeint32 i -> f ppf "%ldn" i
    | Nativeint64 i -> f ppf "%Ldn" i
    | Float fl -> f ppf "%.20G" fl
    | Char c -> f ppf "%C" c
    | String s -> f ppf "\"%s\"" @@ escape_string s
    | List [] -> f ppf "[]"
    | List ts -> f ppf "[ @[<hv>%a@] ]" (format_list ";@ " format) ts
    | Array [] -> f ppf "[||]"
    | Array ts -> f ppf "[| @[%a@] |]" (format_list ";@ " format) ts
    | Variant ("::", Some (Tuple [hd;tl])) -> f ppf "@[<2>(%a@ :: %a)@]" format hd format tl
    | Variant (tag, None) -> f ppf "%s" tag
    | Variant (tag, Some t) -> f ppf "@[<2>%s@ (@[%a@])@]" tag format t
    | Poly_variant (tag, None)  when no_poly -> f ppf "%s" tag
    | Poly_variant (tag, Some t) when no_poly -> f ppf "@[<2>%s@ (@[%a@])@]" tag format t
    | Poly_variant (tag, None) -> f ppf "`%s" tag
    | Poly_variant (tag, Some t) -> f ppf "@[<2>`%s@ (@[%a@])@]" tag format t
    | Record [] -> string ppf "{ (* empty record *) }"
    | Record fields -> f ppf "@[<2>{ @[<hv>%a@] }@]" (format_list ";@ " (fun ppf (fi, v) -> f ppf "@[<2>%s=@ %a@]" fi format v)) fields
    | Object [] when no_poly -> string ppf "{ (* empty object as record *) }"
    | Object fields when no_poly -> f ppf "@[<2>{ @[<hv>%a@] }@]" (format_list ";@ " (fun ppf (fi, v) -> f ppf "@[<2>%s=@ %a@]" fi format v)) fields
    | Object fields -> f ppf "@[@[<2>object@ @[<hv>%a@]@]@ end@]" (format_list "@ " (fun ppf (fi, v) -> f ppf "method %s= %a" fi format v)) fields
    | Tuple [] -> string ppf "( (* empty tuple *) )"
    | Tuple ts -> f ppf "(@[<hv>%a@])" (format_list ",@ " format) ts
    | Unit -> f ppf "()"
    | IntGen (s, (('l'|'L'|'n') as c)) -> f ppf "%s%c (* strange intgen *)" s c
    | IntGen (s, (('g'..'z' | 'G'..'Z') as c)) -> f ppf "%s%c" s c
    | IntGen (s, c) -> f ppf "%s%c (* strange intgen *)" s c
    | Var lid -> format_lident ppf lid
    | App (fn, []) -> f ppf "(%a (* empty app *))" format fn
    | App (f, args) -> format_app ppf f args
    | Seq [] -> string ppf "(* empty seq *)"
    | Seq ts -> f ppf "(@[<hv>%a@])" (format_list ";@ " format) ts

  and format_app ppf fn args =
    assert (args <> []);
    let arg ppf = function
      | None, t -> f ppf "(%a)" format t
      | Some (false, l), t -> f ppf "~%s:(%a)" l format t
      | Some (true, l), t -> f ppf "?%s:(%a)" l format t
    in
    let default () =
      match fn with
      | Variant (_, None) 
      | Poly_variant (_, None) 
      | App (_, _) 
      | Seq _ ->
          f ppf "@[<2>(%a)@ @[<hv>%a@]@]"
            format fn
            (format_list "@ " arg) args
      | _ -> 
          f ppf "@[<2>%a@ @[<hv>%a@]@]"
            format fn
            (format_list "@ " arg) args
    in
    match fn with
    | Var (Lident s) ->
        begin match classify_symbol s with
          | `alpha -> default ()
          | `none -> assert false
          | `prefix -> 
              begin match args with
                | (None,_) :: _ ->
                    f ppf "@[<2>%s%a@]" s (format_list "@ " arg) args
                | _ -> default ()
              end
          | `infix -> 
              begin match args with 
                | (None,a1) :: (None,a2) :: rest ->
                    f ppf "@[<2>%a@ %s@ %a@]" 
                      arg (None,a1) 
                      s
                      (format_list "@ " arg) ((None,a2)  :: rest)
                | _ -> default ()
              end
        end
    | _ -> default ()
  in
  format ppf v

let format = gen_format false
let format_no_poly = gen_format true

let gen_format_with no_poly f ppf v = gen_format no_poly ppf (f v)
let format_with x = gen_format_with false x
let format_no_poly_with x = gen_format_with true x
    
(** { 6 Parsing by compiler-libs } *)

module Parser = struct
  type error = [ `Invalid_construct of Location.t
               | `Lexer of Location.t * Lexer.error
               | `Parser of Syntaxerr.error
               | `Syntax_error of Location.t
               | `Exn of exn ]

  exception Error of error
  
  open Parsetree
  open Longident
  open Asttypes

  let loc_of_error = function
    | `Invalid_construct loc -> loc
    | `Lexer (loc, _) -> loc
    | `Parser e -> Syntaxerr.location_of_error e
    | `Syntax_error loc -> loc
    | `Exn _ -> Location.none

  let format_error ppf e = 
    let open Format in
    let f = fprintf in
    let loc = loc_of_error e in
    if not (loc = Location.none) then f ppf "%a " Location.print loc;
    match e with
    | `Invalid_construct _ -> f ppf "Invalid construct for simple ocaml value"
    | `Exn (Failure s) -> f ppf "Failure: %s" s
    | `Exn exn  -> f ppf "Exn: %s" (Printexc.to_string exn)
    | `Lexer (_loc, e)  -> f ppf "Lexer error: %a"  Lexer.report_error e
    | `Parser e -> f ppf "Parser error: %a" Syntaxerr.report_error e
    | `Syntax_error _loc -> f ppf "Syntax error"

  let () = Printexc.register_printer (function
    | Error e -> Some (format_sprintf "%a" format_error e)
    | _ -> None)

  let exn ex = raise (Error (`Exn ex))
  let invalid loc = raise (Error (`Invalid_construct loc))

  (* We simply discard module paths *)
  let strip loc = function
    | Lident s -> s
    | Ldot (_, s) -> s
    | Lapply _ -> invalid loc

  let rec structure sitems = List.map structure_item sitems

  and structure_item s = 
    match s.pstr_desc with
    | Pstr_eval (e, _) -> expression e
    (* | Pstr_value of rec_flag * (pattern * expression) list *)
    | _ -> invalid s.pstr_loc

  and expression e =
    let open Location in
    match e.pexp_desc with
    | Pexp_constant c -> constant c
    | Pexp_tuple es -> tuple es
    | Pexp_construct ({txt=(Lident "::" as txt); loc={loc_ghost=true}}, Some arg) ->
        begin match expression arg with
        | Tuple [x; List xs] -> List (x :: xs)
        | _ -> construct e.pexp_loc txt (Some arg)
        end
    | Pexp_construct ({txt=Lident "[]"; loc={loc_ghost=true}}, None) -> List []
    | Pexp_construct ({txt; _}, argopt) -> construct e.pexp_loc txt argopt
    | Pexp_variant (l, expopt) -> variant l expopt
    | Pexp_record (fields, None) -> record fields
    | Pexp_array es -> array es
    | Pexp_object class_str ->
        (* Ignores class_str.pcstr_pat *)
        object_ class_str.pcstr_fields
    | Pexp_apply (e, args) -> apply e args
    | Pexp_ident {txt} -> Var txt
    | Pexp_sequence (e1, e2) -> 
        let e1' = match expression e1 with 
          | Seq es -> es
          | e -> [e]
        in
        let e2' = match expression e2 with 
          | Seq es -> es
          | e -> [e]
        in
        Seq (e1' @ e2')
  (*
      | Pexp_let (Recursive, [ {ppat_desc = Ppat_var {txt = s}}, e1], e2) -> Let_rec (s, expression e1, expression e2)
      | Pexp_ident {txt = Longident.Lident s } -> Var s
  *)
    | _ ->
        (* debug *) Format.eprintf "ERROR: %a@." Pprintast.expression e;
        invalid e.pexp_loc
  
  and constant = function
    | Pconst_char c -> Char c
    | Pconst_string (s, _) -> String s
    | Pconst_float (s, None) -> Float (float_of_string s)
    | Pconst_float (_, Some _) -> assert false
    (* Arch dependent int is coerced to int63 *)
    | Pconst_integer (s, None) -> Int63 (Int64.of_string s)
    | Pconst_integer (s, Some 'l') -> Int32 (Int32.of_string s)
    | Pconst_integer (s, Some 'L') -> Int64 (Int64.of_string s)
    | Pconst_integer (s, Some 'n') -> Nativeint64 (Int64.of_nativeint (Nativeint.of_string s))
    | Pconst_integer (s, Some x) -> IntGen (s, x)

  and tuple es = Tuple (List.map expression es)
  and array es = Array (List.map expression es)

  and variant l = function
    | None -> Poly_variant (l, None)
    | Some {pexp_desc= Pexp_tuple es; _} -> Poly_variant (l, Some (Tuple (List.map expression es)))
    | Some e -> Poly_variant (l, Some (expression e))

  and record fields =
    Record (List.map (fun ({txt = txt; loc}, e) ->
      let e = expression e in
      strip loc txt, e) fields)

  and object_ fields =
    Object (List.map (fun { pcf_desc; pcf_loc } -> match pcf_desc with
    | Pcf_method ({txt; _}, _, Cfk_concrete(_, { pexp_desc= Pexp_poly (e, _)} )) -> txt, expression e
    | Pcf_method ({txt; _}, _, Cfk_concrete(_, e)) -> txt, expression e
    | _ -> invalid pcf_loc) fields)

  and construct loc lident argopt =
    let name = strip loc lident in
    match argopt, name with
    | None, "true" -> Bool true
    | None, "false" -> Bool false
    | None, "()" -> Unit
    | None, "[]" -> List []
    | None, _ -> Variant (name, None)
    | Some e, _ -> Variant (name, Some (expression e))

  and apply e args =
    let args = List.map (fun (l,e) ->
        let l = match l with
          | Nolabel -> None
          | Labelled s -> Some (false, s)
          | Optional s -> Some (true, s)
        in
        (l, expression e)) args
    in
    App (expression e, args)

  let from_lexbuf ?(source="<camlon input>") lexbuf = 
    Location.init lexbuf source;
    try
      Lexer.init (); (* not re-entrant *)
      let str = Parser.implementation Lexer.token lexbuf in
      structure str
    with
    | (Error _ as exn) -> raise exn
    | Lexer.Error (e, loc) -> raise (Error (`Lexer (loc, e)))
    | Syntaxerr.Error e -> raise (Error (`Parser e))

    | Parsing.Parse_error | Syntaxerr.Escape_error ->
      let loc = Location.curr lexbuf in
      raise (Error (`Syntax_error loc))
    | e -> exn e

  let from ?source f d = from_lexbuf ?source (f d)

  let from_channel  ?source d = from ?source Lexing.from_channel d
  let from_string   ?source d = from ?source Lexing.from_string d
  let from_function ?source d = from ?source Lexing.from_function d
end

type 'a load_error = [ `Conv of 'a | Parser.error ]

let load path = 
  let ic = open_in path in
  let res =
    Location.input_name := path;
    match Parser.from_channel ~source:path ic with
    | exception (Parser.Error e) -> Error e
    | res -> Ok res
  in
  close_in ic;
  res
         
let load_with decoder path =
  match load path with
  | Error e -> Error (e :> _ load_error)
  | Ok res ->
     let rec f acc = function
       | [] -> Ok (List.rev acc)
       | x::xs ->
          match decoder x with
          | Error e -> Error (`Conv e)
          | Ok y -> f (y::acc) xs
     in
     f [] res

let save_with encoder ~perm ?(no_poly=false) path ts = 
  let oc = open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] perm path in
  try
    (* Double semis are reqiured to parse back by [Parser.implementation] *)
    let ppf = Format.formatter_of_out_channel oc in
    List.iter (fun t -> 
      Format.fprintf ppf "%a;;@." (gen_format_with no_poly encoder) t;
    ) ts;
    close_out oc
  with
  | e -> 
      close_out oc;
      raise e

let save = save_with (fun x -> x)
