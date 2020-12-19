open! Core_kernel
include Json_object.Utils

let bail_on_json json ~module_name =
  let message = sprintf "Unexpected [Subreddit_traffic.%s.t] JSON" module_name in
  raise_s [%message message (json : Json.t)]
;;

let get_time_and_rest json ~module_name =
  match Json.get_list ident json with
  | time_json :: rest ->
    let time = time time_json in
    let rest = List.map rest ~f:int in
    time, rest
  | _ -> bail_on_json json ~module_name
;;

module By_date = struct
  type t =
    { date : Date.t
    ; uniques : int
    ; pageviews : int
    ; subscriptions : int
    }
  [@@deriving sexp]

  let of_json json =
    let module_name = "By_date" in
    match get_time_and_rest json ~module_name with
    | time, [ uniques; pageviews; subscriptions ] ->
      let date = Time_ns.to_date time ~zone:Time.Zone.utc in
      { date; uniques; pageviews; subscriptions }
    | _ -> bail_on_json json ~module_name
  ;;
end

module By_month = struct
  type t =
    { year : int
    ; month : Month.t
    ; uniques : int
    ; pageviews : int
    }
  [@@deriving sexp]

  let of_json json =
    let module_name = "By_month" in
    match get_time_and_rest json ~module_name with
    | time, [ uniques; pageviews ] ->
      let date = Time_ns.to_date time ~zone:Time.Zone.utc in
      let month = Date.month date in
      let year = Date.year date in
      { year; month; uniques; pageviews }
    | _ -> bail_on_json json ~module_name
  ;;
end

module By_hour = struct
  type t =
    { hour : Time_ns.Alternate_sexp.t
    ; uniques : int
    ; pageviews : int
    }
  [@@deriving sexp]

  let of_json json =
    let module_name = "By_hour" in
    match get_time_and_rest json ~module_name with
    | hour, [ uniques; pageviews ] -> { hour; uniques; pageviews }
    | _ -> bail_on_json json ~module_name
  ;;
end

let by_date = required_field "day" (Json.get_list By_date.of_json)
let by_month = required_field "month" (Json.get_list By_month.of_json)
let by_hour = required_field "hour" (Json.get_list By_hour.of_json)
