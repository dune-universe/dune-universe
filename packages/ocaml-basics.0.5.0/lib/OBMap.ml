module Result = OBResult

include Map.Make(String)

let find_exn = find

let find key map =
  try
    Some (find_exn key map)
  with Not_found -> None

let find_or key substitute map =
  try
    find_exn key map
  with Not_found -> substitute

let has key value map =
  let open OBOption.Infix in
  find key map >>= fun x ->
  if x = value then
    Some ()
  else
    None

let traverse
  (type a)
  (type b)
  (type err)
  (f: key -> a -> (b, err) Result.t)
  (map: a t)
  : (b t, err) Result.t
=
  let module M = struct
    exception Exception of err
  end in
  try
    Ok (fold (fun key value accu ->
      match f key value with
      | Error x -> raise (M.Exception x)
      | Ok value' -> add key value' accu) map empty)
  with M.Exception x -> Error x

let traverse'
  (type a)
  (type b)
  (f: key -> a -> (b, string list) Result.t)
  (map: a t)
  : (b t, string list) Result.t
=
  fold (fun key value accu ->
    match accu, f key value with
    | Error err1, Error err2 -> Error (Result.Accu.add err1 err2)
    | Error err, Ok _
    | Ok _, Error err -> Error err
    | Ok accu, Ok value' -> Ok (add key value' accu)) map (Ok empty)

let sequence map = traverse (fun _ x -> x) map
let sequence' map = traverse' (fun _ x -> x) map
