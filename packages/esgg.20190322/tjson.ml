(** Template JSON *)

open Printf
open ExtLib
open Prelude

let debug_dump = false

type var = { optional : bool; list : bool; name : string } [@@deriving show]
type group = { label : string; vars : string list } [@@deriving show]

type t = [
| `Assoc of (string * t) list
| `Bool of bool
| `Int of int
| `Float of float
| `List of t list
| `Null
| `String of string
| `Var of var
| `Optional of (group * t)
]

let pp_string f x =
  let b = Buffer.create 10 in
  let pf = Format.formatter_of_buffer b in
  f pf x;
  Format.pp_print_flush pf ();
  Buffer.contents b

let show_error = pp_string Jsonm.pp_error
let show_lexeme = pp_string Jsonm.pp_lexeme

let sub_decoded d s =
  let ((l1,c1),(l2,c2)) = Jsonm.decoded_range d in
  let find_line l =
    let rec loop i = function
    | 0 -> assert false
    | 1 -> i
    | l -> loop (String.index_from s i '\n' + 1) (l - 1)
    in
    loop 0 l
  in
  let first = find_line l1 + c1 - 1 in
  let last = find_line l2 + c2 in
  String.slice ~first ~last s

let var_name s =
  match Scanf.sscanf s "%_[a-zA-Z]%_[0-9_a-zA-Z]%!" () with
  | exception _ -> fail "bad var name %S" s
  | () -> s

let test_optional s = if String.ends_with s "?" then String.slice ~last:(-1) s, true else s, false

let make_var s =
  try
    let s = try Scanf.sscanf s "$(%s@)%!" id with _ -> Scanf.sscanf s "$%s%!" id in
    let (s,optional) = test_optional s in
    let (name,list) =
      match String.split s ":" with
      | (n,"list") -> n, true
      | (_,hint) -> fail "unrecognized part %S after colon, only 'list' hint is allowed" hint
      | exception _ -> s, false
    in
    { optional; name = var_name name; list; }
  with
    exn -> fail ~exn "unrecognized variable %S" s

let show_decoded_range ((l1,c1),(l2,c2)) = sprintf "%d,%d-%d,%d" l1 c1 l2 c2

exception Escape of ((int * int) * (int * int)) * Jsonm.error

let parse s : t =
  let rec lexeme d =
    match Jsonm.Uncut.decode d with
    | `Lexeme l -> (l :> [Jsonm.lexeme|`Var of var])
    | `Comment _ | `White _ -> lexeme d (* skip *)
    | `Error (`Expected `Value) -> `Var (make_var @@ String.strip @@ sub_decoded d s)
    | `Error e -> raise (Escape (Jsonm.decoded_range d, e))
    | `End | `Await -> assert false
  in
  let rec finish d =
    match Jsonm.Uncut.decode d with
    | `End -> ()
    | `Comment _ | `White _ -> finish d
    | `Await -> assert false
    | _  -> fail "expected End"
  in
  let rec value v k d =
    match v with
    | `Os -> obj [] k d
    | `As -> arr [] k d
    | `Float _ as v -> let v = match int_of_string (sub_decoded d s) with exception _ -> v | i -> `Int i in k v d
    | `Null | `Bool _ | `String _ | `Var _ as v -> k v d
    | `Ae | `Oe | `Name _ -> assert false
  and arr vs k d =
    match lexeme d with
    | `Ae -> k (`List (List.rev vs)) d
    | v -> value v (fun v -> arr (v :: vs) k) d
  and obj ms k d =
    match lexeme d with
    | `Oe -> k (`Assoc (List.rev ms)) d
    | `Name n -> value (lexeme d) (fun v -> obj ((n, v) :: ms) k) d
    | _ -> assert false
  in
  let d = Jsonm.decoder @@ `String s in
  try
    let v = value (lexeme d) (fun v _ -> v) d in
    finish d;
    v
  with
    Escape (range,e) -> fail "E: %s %s" (show_decoded_range range) (show_error e)

let intersperse sep l = match l with [] -> [] | x::xs -> x :: List.concat (List.map (fun x -> [sep; x]) xs)

let lift_to_string map (v:t) =
  let module J = Yojson.Basic in
  let quote x = `Quote x in
  let list x = `List x in
  let splice x = `Splice x in
  let stringify x =
    let rec fold f acc = function
    | `List l -> List.fold_left (fun acc x -> fold f acc x) acc l
    | `Quote _ | `Splice _ as x -> f acc x
    in
    let out = Buffer.create 10 in
    let printer acc x =
      match acc, x with
      | `Quote a, `Quote b -> `Quote (a^b)
      | `Splice a, `Splice b -> `Splice (a^b)
      | `Nothing, x -> x
      | `Quote a, `Splice b -> bprintf out "%S ^\n  " a; `Splice b
      | `Splice a, `Quote b -> bprintf out "%s\n  ^ " a; `Quote b
      | `Quote a, `Nothing -> bprintf out "%S\n  " a; `Nothing
      | `Splice a, `Nothing -> bprintf out "%s\n  " a; `Nothing
    in
    match printer (fold printer `Nothing x) `Nothing with
    | `Nothing -> Buffer.contents out
    | _ -> assert false
  in
  let quote_val f x = let b = Bi_outbuf.create 1 in f b x; quote @@ Bi_outbuf.contents b in
  let rec output_list l =
    let elem = function
    | `Optional ({label;vars},x) ->
      let match_vars =
        match vars with
        | [] -> assert false
        | [x] -> x
        | l -> sprintf "{%s}" (String.concat ";" l)
      in
      list [
        splice @@ sprintf "begin match %s with None -> None | Some %s -> Some (" label match_vars;
        splice @@ stringify @@ output x;
        splice ") end;";
      ]
    | x -> list [splice "Some ("; splice @@ stringify @@ output x; splice ");"]
    in
    list [
      splice "(String.concat \",\" @@ List.map (function Some x -> x | None -> assert false) @@ List.filter (function Some _ -> true | None -> false) [";
      splice @@ stringify @@ list @@ List.map elem l;
      splice "])"
    ]
  and output = function
  | `Null -> quote_val J.write_null ()
  | `Bool b -> quote_val J.write_bool b
  | `String s -> quote_val J.write_string s
  | `Float f -> quote_val J.write_float f
  | `Int i -> quote_val J.write_int i
  | `Optional (g,_) -> fail "Error: optional group %S not as list element" g.label
  | `Var { optional=_; list=_; name } -> splice @@ map name (* TODO assert scope for optional=true? *)
  | `List l when List.exists (function `Optional _ -> true | _ -> false) l ->
    list [
      quote "[";
      output_list l;
      quote "]";
    ]
  | `List l -> (* regular *)
    list [
      quote "[";
      list @@ intersperse (quote ",") (List.map output l);
      quote "]";
    ]
  | `Assoc a ->
    list [
      quote "{";
      list @@ intersperse (quote ",") (List.map (fun (k,v) -> list [quote_val J.write_string k; quote ":"; output v]) a);
      quote "}"
    ]
  in
  stringify @@ output v

let rec fold_ (f:'a->t->'a) acc = function
| `Null | `Bool _ | `String _ | `Float _ | `Int _ | `Var _ | `Optional _ as x -> f acc x
| `List l -> List.fold_left (fold_ f) acc l
| `Assoc a -> List.fold_left (fun acc (_,v) -> fold_ f acc v) acc a

let rec fold ~optional (f:'a->t->'a) acc json =
  let f acc = function
  | `Optional (_,x) when optional -> fold ~optional f acc x
  | `Optional _ -> acc
  | x -> f acc x
  in
  fold_ f acc json

let var_equal v1 v2 =
  match String.equal v1.name v2.name with
  | false -> false
  | true ->
    if (v1.list:bool) <> v2.list then fail "var %S list or not, huh?" v1.name;
    if (v1.optional:bool) <> v2.optional then fail "var %S optional or not, huh?" v1.name;
    true

module SS = Set.Make(String)

let group_equal g1 g2 =
  match String.equal g1.label g2.label with
  | false -> false
  | true ->
    if not @@ SS.equal (SS.of_list g1.vars) (SS.of_list g2.vars) then fail "group %S inconsistent" g1.label;
    true

let get_vars ~optional (v:t) =
  List.unique ~cmp:var_equal (fold ~optional (fun acc x -> match x with `Var var -> var::acc | _ -> acc) [] v)

let vars (v:t) =
  let groups = List.unique ~cmp:group_equal @@ fold_ (fun acc x -> match x with `Optional (g,_) -> g::acc | _ -> acc) [] v in
  let optional_vars = List.fold_left (fun acc g -> List.fold_left (fun acc v -> SS.add v acc) acc g.vars) SS.empty groups in
  let all_vars = get_vars ~optional:true v in
  if debug_dump then
  begin
    printfn "optional groups:";
    List.iter (fun g -> printfn "  %s" (show_group g)) groups;
    printfn "optional_vars:";
    SS.iter (fun s -> printfn "  %s" s) optional_vars;
    printfn "all_vars:";
    List.iter (fun v -> printfn "  %s" (show_var v)) all_vars;
  end;
  all_vars |> List.iter (fun v -> if SS.mem v.name optional_vars <> v.optional then fail "%S optional or not?" v.name);
  let not_optional_vars = List.filter (fun v -> not @@ SS.mem v.name optional_vars) all_vars in
  not_optional_vars, groups

let vars_names v =
  let (not_optional_vars, optional_groups) = vars v in
  List.map (fun v -> true, v.name) not_optional_vars @ List.map (fun g -> false, g.label) optional_groups

let lift_ map v =
  let b = Buffer.create 10 in
  bprintf b "fun ";
  List.iter (fun (req,pat) -> bprintf b "%c%s " (if req then '~' else '?') pat) (vars_names v);
  bprintf b "() ->\n  %s" (lift_to_string map v);
  bprintf b "\n";
  Buffer.contents b

let lift = lift_ id

let print_parse_json s =
  let rec show d =
    match Jsonm.decode d with
    | `Await -> assert false
    | `End -> printfn "end"
    | `Error e -> printfn "error %s %s" (show_decoded_range @@ Jsonm.decoded_range d) (show_error e); show d
    | `Lexeme x -> printfn "%s" (show_lexeme x); show d
  in
  show @@ Jsonm.decoder @@ `String s

let rec to_yojson_exn : t -> Yojson.t = function
| `Var v -> fail "to_yojson_exn `Var %s" (show_var v)
| `Optional (g, _) -> fail "to_yojson_exn `Optional %S" g.label
| `Assoc l -> `Assoc (List.map (fun (k,v) -> k, to_yojson_exn v) l)
| `List l -> `List (List.map to_yojson_exn l)
| `String _ | `Float _ | `Int _ | `Bool _ | `Null as x -> x

let replace json k v =
  match json with
  | `Assoc l -> `Assoc (List.map (function (k',_) when String.equal k k' -> k, v | x -> x) l)
  | _ -> assert false
