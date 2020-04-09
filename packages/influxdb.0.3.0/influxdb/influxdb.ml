open Base

module Precision = struct
  type t = Nanosecond | Microsecond | Millisecond | Second | Minute | Hour

  let to_string = function
    | Nanosecond -> "ns"
    | Microsecond -> "u"
    | Millisecond -> "ms"
    | Second -> "s"
    | Minute -> "m"
    | Hour -> "h"

  let of_string = function
    | "ns" -> Some Nanosecond
    | "u" -> Some Microsecond
    | "ms" -> Some Millisecond
    | "s" -> Some Second
    | "m" -> Some Minute
    | "h" -> Some Hour
    | _ -> None
end

module TimestampNS = struct
  type t = int64

  let of_float_seconds f =
    let ns_per_sec = 1_000_000_000L in
    f *. Float.of_int64 ns_per_sec |> Int64.of_float

  let to_string_precision p t =
    let open Int64 in
    ( match p with
    | Precision.Nanosecond -> t
    | Precision.Microsecond -> t / 1000L
    | Precision.Millisecond -> t / 1_000_000L
    | Precision.Second -> t / 1_000_000_000L
    | Precision.Minute -> t / 60_000_000_000L
    | Precision.Hour -> t / 3600_000_000_000L )
    |> Int64.to_string

  (* Tests *)

  let%expect_test "of_float_seconds" =
    let ts = of_float_seconds 1529414438.82391405 in
    Stdio.print_endline (to_string_precision Precision.Nanosecond ts);
    [%expect {| 1529414438823913984 |}]

  let%expect_test "to_string_precision_nanoseconds" =
    Stdio.print_endline
      (to_string_precision Precision.Nanosecond 1529349109966270847L);
    [%expect {| 1529349109966270847 |}]

  let%expect_test "to_string_precision_microsecond" =
    Stdio.print_endline
      (to_string_precision Precision.Microsecond 1529349109966270847L);
    [%expect {| 1529349109966270 |}]

  let%expect_test "to_string_precision_millisecond" =
    Stdio.print_endline
      (to_string_precision Precision.Millisecond 1529349109966270847L);
    [%expect {| 1529349109966 |}]

  let%expect_test "to_string_precision_second" =
    Stdio.print_endline
      (to_string_precision Precision.Second 1529349109966270847L);
    [%expect {| 1529349109 |}]

  let%expect_test "to_string_precision_minute" =
    Stdio.print_endline
      (to_string_precision Precision.Minute 1529349109966270847L);
    [%expect {| 25489151 |}]

  let%expect_test "to_string_precision_hour" =
    Stdio.print_endline
      (to_string_precision Precision.Hour 1529349109966270847L);
    [%expect {| 424819 |}]
end

module Field = struct
  type field_key = string

  type field_value =
    | Float of float
    | Int of int
    | String of string
    | Bool of bool

  type t = field_key * field_value

  let v_to_string = function
    | Float f -> Float.to_string f
    | Int i -> Int.to_string i
    | String s -> s
    | Bool b -> if b then "t" else "f"

  let to_string (k, v) = k ^ "=" ^ v_to_string v

  let float ?(name = "value") value = (name, Float value)

  let int ?(name = "value") value = (name, Int value)

  let string ?(name = "value") value = (name, String value)

  let bool ?(name = "value") value = (name, Bool value)
end

module Point = struct
  type t = {
    name : string;
    field : Field.t;
    tags : (string * string) list;
    extra_fields : Field.t list;
    (* If None, a timestamp will be assigned by InfluxDB  *)
    timestamp : TimestampNS.t option;
  }

  (* Returns the line format representation of [t] *)
  let to_line ?(precision = Precision.Nanosecond) t =
    let fields =
      List.map ~f:Field.to_string (t.field :: t.extra_fields)
      |> String.concat ~sep:","
    in
    let tags =
      match t.tags with
      | [] -> ""
      | tt ->
          ","
          ^ ( List.map ~f:(fun (k, v) -> k ^ "=" ^ v) tt
            |> String.concat ~sep:"," )
    in
    let line = Printf.sprintf "%s%s %s" t.name tags fields in
    match t.timestamp with
    | None -> line
    | Some ts -> line ^ " " ^ TimestampNS.to_string_precision precision ts

  let create ?(tags = []) ?(extra_fields = []) ?timestamp ~field name =
    { name; field; tags; extra_fields; timestamp }

  (* TESTS  *)

  let%expect_test "to_line" =
    Stdio.print_endline
      (to_line
         {
           name = "count";
           field = ("value", Int 100);
           tags = [ ("tag1", "val1"); ("tag2", "val2") ];
           extra_fields =
             [
               ("bool", Bool true);
               ("float", Float 1.23);
               ("int", Int 123);
               ("string", String "string");
             ];
           timestamp = Some 1529349109966270847L;
         });
    [%expect
      {| count,tag1=val1,tag2=val2 value=100,bool=t,float=1.23,int=123,string=string 1529349109966270847 |}]

  let%expect_test "to_line_seconds" =
    Stdio.print_endline
      (to_line ~precision:Precision.Second
         {
           name = "count";
           field = ("value", Int 100);
           tags = [ ("tag1", "val1"); ("tag2", "val2") ];
           extra_fields = [ ("bool", Bool true) ];
           timestamp = Some 1529349109966270847L;
         });
    [%expect {| count,tag1=val1,tag2=val2 value=100,bool=t 1529349109 |}]

  let%expect_test "to_line_no_timestamp" =
    Stdio.print_endline
      (to_line
         {
           name = "count";
           field = ("value", Int 100);
           tags = [ ("tag1", "val1"); ("tag2", "val2") ];
           extra_fields = [ ("bool", Bool true) ];
           timestamp = None;
         });
    [%expect {| count,tag1=val1,tag2=val2 value=100,bool=t |}]

  let%expect_test "to_line_no_tags" =
    Stdio.print_endline
      (to_line
         {
           name = "count";
           field = ("value", Int 100);
           tags = [];
           extra_fields = [ ("bool", Bool true) ];
           timestamp = None;
         });
    [%expect {| count value=100,bool=t |}]

  let%expect_test "create" =
    let m = create "count" ~field:("value", Int 100) in
    Stdio.print_endline (to_line m);
    [%expect {| count value=100 |}]
end

module Protocol = struct
  let header_build = "X-Influxdb-Build"

  let header_version = "X-Influxdb-Version"

  type ping_response = { build : string; version : string }
end
