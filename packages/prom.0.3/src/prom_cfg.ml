type ezjsonm =
  [ `O of (string * ezjsonm) list
  | `A of ezjsonm list
  | `Bool of bool
  | `Float of float
  | `String of string
  | `Null ]

type yojson =
  [ `Bool of bool
  | `Assoc of (string * yojson) list
  | `Float of float
  | `Int of int
  | `Intlit of string
  | `List of yojson list
  | `Null
  | `String of string
  | `Tuple of yojson list
  | `Variant of string * yojson option ]

module File_sd_config = struct
  type t = { targets : host list; labels : (string * string) list }

  and host = string * int option

  let string_of_host = function
    | host, None -> host
    | host, Some port -> host ^ ":" ^ string_of_int port

  let create ?(labels = []) targets = { targets; labels }

  let to_yojson cfgs =
    let to_yojson { targets; labels } =
      `Assoc
        [
          ( "targets",
            `List (List.map (fun host -> `String (string_of_host host)) targets)
          );
          ("labels", `Assoc (List.map (fun (k, v) -> (k, `String v)) labels));
        ]
    in
    `List (List.map to_yojson cfgs)

  let to_ezjsonm cfgs =
    let to_yojson { targets; labels } =
      `O
        [
          ( "targets",
            `A (List.map (fun host -> `String (string_of_host host)) targets) );
          ("labels", `O (List.map (fun (k, v) -> (k, `String v)) labels));
        ]
    in
    `A (List.map to_yojson cfgs)
end
