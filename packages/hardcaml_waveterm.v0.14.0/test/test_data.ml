open! Import

let clock = Wave.Clock "clock"

let random_string ~max_length =
  String.init
    (Random.int max_length + 1)
    ~f:(fun _ -> Random.int 26 + Char.to_int 'a' |> Char.of_int_exn)
;;

let random_bits ~length ~width =
  { Data.data = Array.init length ~f:(fun _ -> Bits.random ~width); length }
;;

let random_format () =
  match Random.int 4 with
  | 0 -> Wave_format.Unsigned_int
  | 1 -> Wave_format.Int
  | 2 -> Wave_format.Hex
  | _ -> Wave_format.Binary
;;

let random_data ~prefix ~length =
  if Random.int 2 = 0
  then Wave.Binary (prefix ^ random_string ~max_length:20, random_bits ~length ~width:1)
  else
    Wave.Data
      ( prefix ^ random_string ~max_length:20
      , random_bits ~length ~width:(Random.int 64 + 1)
      , random_format ()
      , Left )
;;

let create ~prefix ~length ~num_signals =
  { Waves.cfg = Waves.Config.default
  ; waves =
      clock :: List.init num_signals ~f:(fun i -> random_data ~prefix:(prefix i) ~length)
      |> Array.of_list
  }
;;
