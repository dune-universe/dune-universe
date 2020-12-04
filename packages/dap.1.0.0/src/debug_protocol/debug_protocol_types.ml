open Util

module type JSONABLE = sig
  type t

  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  val to_yojson : t -> Yojson.Safe.t
end

module type EVENT = sig
  val type_ : string

  module Payload : sig
    type t
    [@@deriving yojson]
  end
end

module type COMMAND = sig
  val type_ : string

  module Arguments : sig
    type t
    [@@deriving yojson]
  end

  module Result : sig
    type t
    [@@deriving yojson]
  end
end

module Any = struct
  type t = Yojson.Safe.t

  let of_yojson x = Ok x

  let to_yojson x = x
end

module Empty_dict = struct
  type t = unit

  let of_yojson = function
    | `Assoc [] -> Ok ()
    | _ -> Error (print_exn_at_loc [%here])

  let to_yojson () = `Assoc []
end

module Int_or_string = struct
  type t =
    | Int of int
    | String of string

  let of_yojson = function
    | `Int value -> Ok (Int value)
    | `String value -> Ok (String value)
    | _ -> Error (print_exn_at_loc [%here])

  let to_yojson = function
    | Int value -> `Int value
    | String value -> `String value
end

module Dict = struct
  module Make (P : sig
    type t

    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    val to_yojson : t -> Yojson.Safe.t
  end) = struct
    type t = P.t String_map.t

    let empty = String_map.empty
    let is_empty = String_map.is_empty
    let mem = String_map.mem
    let add = String_map.add
    let singleton = String_map.singleton
    let remove = String_map.remove
    let merge = String_map.merge
    let union = String_map.union
    let compare = String_map.compare
    let equal = String_map.equal
    let iter = String_map.iter
    let fold = String_map.fold
    let for_all = String_map.for_all
    let exists = String_map.exists
    let filter = String_map.filter
    let partition = String_map.partition
    let cardinal = String_map.cardinal
    let bindings = String_map.bindings
    let min_binding = String_map.min_binding
    let max_binding = String_map.max_binding
    let choose = String_map.choose
    let split = String_map.split
    let find = String_map.find
    let map = String_map.map
    let mapi = String_map.mapi

    let of_yojson json =
      match (
        match json with
        | `Assoc l ->
          let rec build map = function
            | [] -> map
            | (k, v) :: tl -> (
                match P.of_yojson v with
                | Ok value -> build (add k value map) tl
                | Error msg -> failwith msg
              )
          in build empty l
        | _ -> failwith (print_exn_at_loc [%here])
      ) with
      | exception Failure msg -> Error msg
      | t -> Ok t

    let to_yojson dict =
      `Assoc (fold (fun k v l -> (k, P.to_yojson v) :: l) dict [])
  end
end

module String_dict = Dict.Make (struct
    type t = string [@@deriving yojson]
  end)

module String_opt_dict = Dict.Make (struct
    type t = string option [@@deriving yojson]
  end)
