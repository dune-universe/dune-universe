open Base

let format_rate rate =
  match Float.classify rate with
  | Infinite | Nan | Zero -> "n/a"
  | Normal | Subnormal ->
    if Float.( < ) rate 1e-3
    then Printf.sprintf "s/%5.0fit" (1. /. rate)
    else if Float.( < ) rate 1.
    then Printf.sprintf "s/%5.2fit" (1. /. rate)
    else if Float.( < ) rate 1e3
    then Printf.sprintf "%5.2fit/s" rate
    else if Float.( < ) rate 1e5
    then Printf.sprintf "%5.0fit/s" rate
    else Printf.sprintf "%5.0fkit/s" (rate *. 1e-3)

module Time = struct
  module Span = struct
    type t = float

    let divmod n m = n / m, n % m

    let format seconds =
      match Float.classify seconds with
      | Infinite -> "inf"
      | Nan -> "nan"
      | Subnormal | Normal | Zero ->
        let sign, seconds =
          if Float.( < ) seconds 0. then "-", -.seconds else "", seconds
        in
        let seconds = Int.of_float seconds in
        let minutes, seconds = divmod seconds 60 in
        let hours, minutes = divmod minutes 60 in
        if hours <> 0
        then Printf.sprintf "%s%dh%0.2dm%0.2ds" sign hours minutes seconds
        else Printf.sprintf "%s%0.2dm%0.2ds" sign minutes seconds

    let of_secs = Fn.id
    let to_secs = Fn.id
  end

  type t = float

  let now () = Unix.gettimeofday ()
  let diff = ( -. )
end
