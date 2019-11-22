open! Import

module Config = struct
  type t =
    { mutable wave_width : int
    ; mutable wave_height : int
    ; mutable start_cycle : int
    ; mutable start_signal : int
    ; mutable wave_cursor : int
    ; mutable signal_cursor : int
    ; mutable signal_scroll : int
    ; mutable value_scroll : int
    }
  [@@deriving sexp_of]

  let default =
    { wave_width = 3
    ; wave_height = 1
    ; start_cycle = 0
    ; start_signal = 0
    ; wave_cursor = -1
    ; signal_cursor = -1
    ; signal_scroll = 0
    ; value_scroll = 0
    }
  ;;
end

type t =
  { cfg : Config.t
  ; waves : Wave.t array
  }
[@@deriving sexp_of]

let write ch w =
  let w =
    { w with
      waves =
        Array.map
          ~f:(fun (d : Wave.t) ->
            (match d with
             | Empty _ | Clock _ -> d
             | Binary (n, d) ->
               Binary
                 (n, { d with Data.data = Array.init d.length ~f:(Array.get d.data) })
             | Data (n, d, ts, alignment) ->
               let ts =
                 match ts with
                 | Custom _ -> Wave_format.Binary
                 | _ -> ts
               in
               (* cant marshal functions *)
               Data
                 ( n
                 , { d with data = Array.init d.length ~f:(Array.get d.data) }
                 , ts
                 , alignment )
               : Wave.t))
          w.waves
    }
  in
  Caml.Marshal.to_channel ch w []
;;

let read ch : t = Caml.Marshal.from_channel ch
