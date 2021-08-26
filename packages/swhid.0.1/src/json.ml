type json =
  | Unit
  | Bool of bool
  | Float of float
  | String of string
  | Array of json list
  | Object of (string * json) list

exception Escape of ((int * int) * (int * int)) * Jsonm.error

let json_of_src ?encoding (src : [ `Channel of in_channel | `String of string ])
    =
  let dec d =
    match Jsonm.decode d with
    | `Lexeme l -> l
    | `Error e -> raise (Escape (Jsonm.decoded_range d, e))
    | `End
    | `Await ->
      assert false
  in
  let rec value v k d =
    match v with
    | `Os -> obj [] k d
    | `As -> arr [] k d
    | `Null -> k Unit d
    | `Bool b -> k (Bool b) d
    | `String s -> k (String s) d
    | `Float f -> k (Float f) d
    | _ -> assert false
  and arr vs k d =
    match dec d with
    | `Ae -> k (Array (List.rev vs)) d
    | v -> value v (fun v -> arr (v :: vs) k) d
  and obj ms k d =
    match dec d with
    | `Oe -> k (Object (List.rev ms)) d
    | `Name n -> value (dec d) (fun v -> obj ((n, v) :: ms) k) d
    | _ -> assert false
  in
  let d = Jsonm.decoder ?encoding src in
  try Ok (value (dec d) (fun v _ -> v) d) with
  | Escape (r, e) -> Error (r, e)

exception Found of json

let rec find key = function
  | Unit
  | Bool _
  | Float _
  | String _ ->
    None
  | Array l -> begin
    try
      List.iter
        (fun v ->
          match find key v with
          | None -> ()
          | Some v -> raise (Found v) )
        l;
      None
    with
    | Found v -> Some v
  end
  | Object l -> (
    try
      List.iter (fun (k, v) -> if k = key then raise (Found v)) l;
      None
    with
    | Found v -> Some v )

let find_string key v =
  match find key v with
  | Some (String f) -> Some f
  | _ -> None

let find_obj key v =
  match find key v with
  | Some (Object o) -> Some o
  | _ -> None
