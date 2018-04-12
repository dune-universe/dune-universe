let maximum = int_of_float (36.0 ** 4.0)
let prefix  = "c"
let state   = ref 0

let alphabet = [
  "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9";
  "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m";
  "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"
]

let floor_to_int number =
  number
  |> Core.Float.round_down
  |> int_of_float

let rec loop result number =
  if number <= 0. then result else
    let index   = Core.Float.mod_float number 36. in
    let number' = Core.Float.(number / 36.) in
    let digit   = List.nth alphabet (floor_to_int index) in
    let result' = digit ^ result in
    loop result' number'

let base36 number =
  let number' = Core.Float.abs number in
  if Core.Float.(number' < 36.) then
    List.nth alphabet (floor_to_int number')
  else
    loop "" number'

let adjust fill text =
  let length = String.length text in
  let size   = max 0 (fill - length) in
  let buffer = String.make size '0' in
  buffer ^ text

let padding fill count text =
  let adjusted = adjust fill text in
  let length   = Core.String.length adjusted in
  let offset   = length - count in
  Core.String.sub adjusted ~pos:offset ~len:count

let padding4 = padding 8 4
let padding8 = padding 8 8

let call   lambda = lambda ( )
let digest text   = Digest.to_hex (Digest.string text)

let sum text =
  let number = text
  |> Core.String.to_list
  |> (Core.List.map ~f:int_of_char)
  |> (Core.List.fold_left ~init:0 ~f:(+))
  in number / (String.length text + 1)

let timestamp ( ) =
  ( )
  |> Unix.time
  |> base36
  |> padding8

let counter ( ) =
  state := (if !state < maximum then !state else 0);
  incr state;
  !state
  |> pred
  |> float_of_int
  |> base36
  |> padding4

let fingerprint =
  let number = ( )
  |> Unix.gethostname
  |> digest
  |> sum
  in (number + Unix.getpid ( ))
  |> float_of_int
  |> base36
  |> padding4

let random ( ) =
  maximum
  |> Core.Random.int
  |> float_of_int
  |> base36
  |> padding4

let __fields ( ) =
  (call timestamp),
  (call counter),
  fingerprint,
  (call random ^ call random)

let generate ( ) =
  prefix ^
  (call timestamp) ^ (call counter) ^
  fingerprint ^
  (call random) ^ (call random)

let _ =
  Core.Random.self_init ( )
