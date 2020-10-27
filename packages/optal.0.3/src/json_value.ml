open Value

type json =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of json list
  | `O of (string * json) list ]

type escape = ((int * int) * (int * int)) * Jsonm.error

exception Escape of escape

type result =
  [ `JSON of json
  | `Error of escape ]

let json_of_src ?encoding (src : [`Channel of in_channel | `String of string]) : result =
  let dec d = match Jsonm.decode d with
    | `Lexeme l -> l
    | `Error e -> raise (Escape (Jsonm.decoded_range d, e))
    | `End | `Await -> assert false
  in
  let rec value v k d = match v with
    | `Os -> obj [] k d  | `As -> arr [] k d
    | `Null | `Bool _ | `String _ | `Float _ as v -> k v d
    | _ -> assert false
  and arr vs k d = match dec d with
    | `Ae -> k (`A (List.rev vs)) d
    | v -> value v (fun v -> arr (v :: vs) k) d
  and obj ms k d = match dec d with
    | `Oe -> k (`O (List.rev ms)) d
    | `Name n -> value (dec d) (fun v -> obj ((n, v) :: ms) k) d
    | _ -> assert false
  in
  let d = Jsonm.decoder ?encoding src in
  try `JSON (value (dec d) (fun v _ -> v) d) with
  | Escape (r, e) -> `Error (r, e)

type error =
  | Loading of escape
  | Not_an_object

exception Error of (error * string)

let rec value_of_json : json -> value = function
  | `Null
  | `Bool _ -> failwith "conversion error: todo real error"
  | `String s -> Vstring s
  | `Float f ->
    let i = int_of_float f in
    if float_of_int i = f
    then Vint i
    else Vlp_exp (Expr.cst f)
  | `A l ->
    Varray (Array.map value_of_json (Array.of_list l))
  | `O l ->
    Vobj (List.fold_left
          (fun o (k,j) -> Object.add (Vstring k) (value_of_json j) o (Loc.dummy_loc))
          (* TODO: real json file location *)
          Object.empty l)

let load_string ?(filename="input") s init_env =
  match json_of_src (`String s) with
  | `JSON json ->
    begin match json with
      | `O l ->
        List.fold_left (fun env (k,j) ->
            Env.bind_var env k (value_of_json j) Loc.dummy_loc)
            (* TODO: add real loc to json file *)
          init_env l
      | _ -> raise (Error (Not_an_object, filename)) end
  | `Error (r,e) ->
    raise (Error (Loading (r,e),filename))

let load_file (ic,filename) init_env =
  match json_of_src (`Channel ic) with
  | `JSON json ->
    begin match json with
      | `O l ->
        List.fold_left (fun env (k,j) ->
            Env.bind_var env k (value_of_json j) Loc.dummy_loc)
            (* TODO: add real loc to json file *)
          init_env l
      | _ -> raise (Error (Not_an_object, filename)) end
  | `Error (r,e) ->
    raise (Error (Loading (r,e),filename))

open Format

let report_error ppf filename = function
  | Loading (((sline,schar),(_,echar)),error) ->
    fprintf ppf "File \"%s\", line %i, characters %i-%i:@.Json loading error %a@."
      filename sline schar echar Jsonm.pp_error error
  | Not_an_object ->
    fprintf ppf "json file %s does not represent an object value" filename
