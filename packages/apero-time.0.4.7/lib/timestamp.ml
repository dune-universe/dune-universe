open Apero
open Time

module Timestamp = struct
  
  module type S = sig
    module Time: Time

    module T: sig
      type t
      val compare: t -> t -> int
      val equal: t -> t -> bool
    end

    include (module type of Ordered.Make (T))

    val create: Uuid.t -> Time.t -> t
    val get_source: t -> Uuid.t
    val get_time: t -> Time.t

    val to_string: t -> string
    val of_string: string -> t option

    val encode: t -> Abuf.t -> unit
    val decode: Abuf.t -> t

    val pp: Format.formatter -> t -> unit
end

  module Make (T: Time) = struct

    module Time = T

    module T = struct
      type t = {
          id: Uuid.t;
          time: Time.t;
      }
      let compare t t' =
        let time_compare = T.compare t.time t'.time in
        if time_compare != 0 then time_compare else Uuid.compare t.id t'.id
      let equal t t' = T.equal t.time t'.time && Uuid.equal t.id t'.id

      let create (id:Uuid.t) (time:Time.t) = { id; time; }
      let get_source t = t.id
      let get_time t = t.time
      let to_string t = T.to_string t.time ^"/"^ Uuid.to_string t.id

      let of_string s =
        match String.split_on_char '/' s with
        | t::i::[] ->
          (match (T.of_string t, Uuid.of_string i) with
          | Some time, Some id -> Some { id; time }
          | _ -> None)
        | _ -> None

      let encode t buf =
        Time.encode t.time buf;
        Uuid.encode t.id buf

      let decode buf =
        Time.decode buf |> fun time ->
        Uuid.decode buf |> fun id ->
        { id; time; }

      let pp ppf t =
        Time.pp ppf t.time;
        Format.pp_print_string ppf " / ";
        Format.pp_print_string ppf @@ Uuid.to_string t.id

    end

    include Ordered.Make (T)

    let create = T.create
    let get_source = T.get_source
    let get_time = T.get_time
    let to_string = T.to_string
    let of_string = T.of_string
    let encode = T.encode
    let decode = T.decode
    let pp = T.pp
  end
end