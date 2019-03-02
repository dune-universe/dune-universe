open! Import

module Rule = struct
  type t =
    | Default
    | Regexp of { re : Re.re sexp_opaque; wave_format : Wave_format.t }
    | Names of { names : Port_name.t list; wave_format : Wave_format.t }
    | Custom of (Port.t -> Wave_format.t option)
  [@@deriving sexp_of]

  let default = Default

  let port_name_is_one_of names ~wave_format =
    Names { names = List.map names ~f:Port_name.of_string; wave_format }
  ;;

  let port_name_is name ~wave_format = port_name_is_one_of [ name ] ~wave_format
  let port_name_matches re ~wave_format = Regexp { re; wave_format }
  let custom ~f = Custom f

  let run t (port : Port.t) : Wave_format.t option =
    match t with
    | Default -> if port.width = 1 then Some Bit else Some Hex
    | Regexp { re; wave_format } ->
      Option.map
        (Re.exec_opt re (port.port_name |> Port_name.to_string))
        ~f:(fun _ -> wave_format)
    | Names { names; wave_format } ->
      if List.mem names port.port_name ~equal:Port_name.equal
      then Some wave_format
      else None
    | Custom f -> f port
  ;;
end

type t = Rule.t list [@@deriving sexp_of]

let empty = []
let add_above t rule = rule :: t
let add_below t rule = t @ [ rule ]
let of_list rules = rules
let combine ~above ~below = above @ below

let rec sort (t : Rule.t list) ~unmatched =
  match t with
  | [] -> []
  | Default :: _ ->
    let defaults =
      List.sort unmatched ~compare:Port.compare
      |> List.filter_map ~f:(fun port ->
        Rule.run Rule.Default port |> Option.map ~f:(fun fmt -> port, fmt))
    in
    [ defaults ]
  | rule :: t ->
    let matched, unmatched =
      List.partition_map unmatched ~f:(fun port ->
        match Rule.run rule port with
        | Some fmt -> `Fst (port, fmt)
        | None -> `Snd port)
    in
    matched :: sort t ~unmatched
;;

let sort_ports_and_formats t ports = sort t ~unmatched:ports |> List.concat
