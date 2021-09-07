open Angstrom

let defer f = Fun.protect ~finally:f

(** redefine & as bitwise operation, && is still logical and *)
let ( & ) = Int.logand

let ( >> ) = Int.shift_right

let fail_with fmt = Printf.kprintf (fun msg -> fail msg) fmt

type arch = BE  (** Big Endian *) | LE  (** Little Endian *)

module Dict = Map.Make (Int)

module Type = struct
  type sign = Signed | Unsigned

  type invalid = ZZ | FF

  type base =
    | Enum
    | Bytes
    | String
    | Int of sign * int * invalid
    | Float of int

  type field = {
      slot : int  (** position within record - defines purpose *)
    ; size : int  (** in bytes *)
    ; ty : base  (** representation *)
  }

  type record = {
      msg : int
    ; arch : arch
    ; fields : field list
    ; dev_fields : int  (** total size of dev fields *)
  }

  let sum = List.fold_left ( + ) 0

  let size { size; _ } = size

  let total fs = fs |> List.map size |> sum

  let json { msg; arch; fields; dev_fields } =
    let f { slot; size; _ } =
      `O
        [
          ("slot", `Float (float_of_int slot))
        ; ("size", `Float (float_of_int size))
        ]
    in
    `O
      [
        ("msg", `Float (float_of_int msg))
      ; ("arch", `String (match arch with LE -> "LE" | BE -> "BE"))
      ; ("fields", `A (List.map f fields))
      ; ("dev_fields", `Float (float_of_int dev_fields))
      ; ("size", `Float (total fields + dev_fields |> float_of_int))
      ]

  (** parse a [field] definition from a FIT file *)
  let field =
    any_uint8 >>= fun slot ->
    any_uint8 >>= fun size ->
    any_uint8 >>= fun ty' ->
    let ty =
      match ty' & 0b1111 with
      | 0  -> Enum
      | 1  -> Int (Signed, 8, FF)
      | 2  -> Int (Unsigned, 8, FF)
      | 3  -> Int (Signed, 16, FF)
      | 4  -> Int (Unsigned, 16, FF)
      | 5  -> Int (Signed, 32, FF)
      | 6  -> Int (Unsigned, 32, FF)
      | 7  -> String
      | 8  -> Float 32
      | 9  -> Float 64
      | 10 -> Int (Unsigned, 8, ZZ)
      | 11 -> Int (Unsigned, 16, ZZ)
      | 12 -> Int (Unsigned, 32, ZZ)
      | 13 -> Bytes
      | _  -> failwith "unknown field base type"
    in
    return { slot; size; ty }

  let record ~dev =
    (int8 0 *> any_int8 >>= function
     | 0 -> return LE
     | 1 -> return BE
     | n -> fail_with "expected 0 or 1 in byte for endianness, found %d" n)
    >>= fun arch ->
    (match arch with LE -> LE.any_uint16 | BE -> BE.any_uint16) >>= fun msg ->
    any_int8 >>= fun n ->
    count n field >>= fun fields ->
    let dev_fields =
      if dev then
        any_int8 >>= fun n ->
        count n field >>= fun dev_fields -> return dev_fields
      else return []
    in
    dev_fields >>= fun dev_fields ->
    return { msg; arch; fields; dev_fields = total dev_fields }
end

type header = { protocol : int; profile : int; length : int }

type value =
  | Enum of int
  | String of string
  | Int of int
  | Int32 of int32
  | Float of float
  | Unknown

type record = { msg : int; fields : (int * value) list }

type t = { header : header; records : record list }

let base arch ty =
  let value =
    match (arch, ty.Type.ty) with
    | _, Type.Bytes -> take ty.Type.size >>= fun x -> return (String x)
    | _, Type.String -> take ty.Type.size >>= fun x -> return (String x)
    | _, Type.Enum -> any_uint8 >>= fun x -> return (Enum x)
    | _, Type.Int (Signed, 8, _) -> any_int8 >>= fun x -> return (Int x)
    | BE, Type.Int (Signed, 16, _) -> BE.any_int16 >>= fun x -> return (Int x)
    | LE, Type.Int (Signed, 16, _) -> LE.any_int16 >>= fun x -> return (Int x)
    | BE, Type.Int (Signed, 32, _) -> BE.any_int32 >>= fun x -> return (Int32 x)
    | LE, Type.Int (Signed, 32, _) -> LE.any_int32 >>= fun x -> return (Int32 x)
    | _, Type.Int (Unsigned, 8, ZZ) ->
        any_uint8 >>= fun x -> return (if x = 0 then Unknown else Int x)
    | LE, Type.Int (Unsigned, 16, ZZ) ->
        LE.any_uint16 >>= fun x -> return (if x = 0 then Unknown else Int x)
    | BE, Type.Int (Unsigned, 16, ZZ) ->
        BE.any_uint16 >>= fun x -> return (if x = 0 then Unknown else Int x)
    | LE, Type.Int (Unsigned, 32, ZZ) ->
        LE.any_int32 >>= fun x -> return (if x = 0l then Unknown else Int32 x)
    | BE, Type.Int (Unsigned, 32, ZZ) ->
        BE.any_int32 >>= fun x -> return (if x = 0l then Unknown else Int32 x)
    | _, Type.Int (Unsigned, 8, FF) ->
        any_uint8 >>= fun x -> return (if x = 0xff then Unknown else Int x)
    | LE, Type.Int (Unsigned, 16, FF) ->
        LE.any_uint16 >>= fun x ->
        return (if x = 0xffff then Unknown else Int x)
    | BE, Type.Int (Unsigned, 16, FF) ->
        BE.any_uint16 >>= fun x ->
        return (if x = 0xffff then Unknown else Int x)
    | LE, Type.Int (Unsigned, 32, FF) ->
        LE.any_int32 >>= fun x ->
        return (if x = 0xffffffffl then Unknown else Int32 x)
    | BE, Type.Int (Unsigned, 32, FF) ->
        BE.any_int32 >>= fun x ->
        return (if x = 0xffffffffl then Unknown else Int32 x)
    | BE, Type.Float 32 -> BE.any_float >>= fun x -> return (Float x)
    | LE, Type.Float 32 -> LE.any_float >>= fun x -> return (Float x)
    | BE, Type.Float 64 -> BE.any_double >>= fun x -> return (Float x)
    | LE, Type.Float 64 -> LE.any_double >>= fun x -> return (Float x)
    | _, _ -> advance ty.Type.size *> return Unknown
  in

  pos >>= fun before ->
  value >>= fun v ->
  pos >>= fun after ->
  let size = after - before in
  if size < ty.Type.size then
    advance (ty.Type.size - size) >>= fun _ -> return v
  else return v

let record arch ty =
  let rec loop vs = function
    | []      -> return { msg = ty.Type.msg; fields = List.rev vs }
    | t :: ts -> base arch t >>= fun v -> loop ((t.Type.slot, v) :: vs) ts
  in
  loop [] ty.Type.fields >>= fun result ->
  (* skip over developer fields *)
  advance ty.Type.dev_fields *> return result

module File = struct
  let _dump dict =
    Dict.bindings dict
    |> List.rev_map (fun (k, v) -> (string_of_int k, Type.json v))
    |> fun x -> `O x |> Ezjsonm.to_channel ~minify:false stderr

  let header =
    any_int8 >>= function
    | (12 | 14) as size ->
        any_int8 >>= fun protocol ->
        LE.any_int16 >>= fun profile ->
        LE.any_int32 >>= fun length ->
        string ".FIT"
        *> (if size = 14 then advance 2 (* skip CRC *) else advance 0)
        *> return { protocol; profile; length = Int32.to_int length }
    | n                 -> fail_with "found unexpected header of size %d" n

  let block (dict, rs) =
    pos >>= fun p ->
    any_int8 >>= fun byte ->
    let key = byte & 0b0000_1111 in
    let tag = byte & 0b1111_0000 in
    match tag with
    | 0b0100_0000 ->
        (* Printf.eprintf "%06x tag=0x%x key=%d\n" p tag key; *)
        (* This is a block that defines a type - add it to the dict *)
        Type.record ~dev:false >>= fun d -> return (Dict.add key d dict, rs)
    | 0b0110_0000 ->
        (* Printf.eprintf "%06x tag=0x%x key=%d\n" p tag key; *)
        (* This is a block that defines a type - add it to the dict *)
        Type.record ~dev:true >>= fun d -> return (Dict.add key d dict, rs)
    | 0b0000_0000 -> (
        (* Printf.eprintf "%06x tag=0x%x key=%d\n" p tag key; *)
        (* This is a block holding values. Its shape is defined by the
           type it refers to and which we must have read earlier and
           should find in the dictionary *)
        match Dict.find_opt key dict with
        | Some ty ->
            let arch = ty.arch in
            record arch ty >>= fun r -> return (dict, r :: rs)
        | None    ->
            pos >>= fun p ->
            fail_with "corrupted file? No type for key=%d offset=%d at %s" key p
              __LOC__)
    | _ when (tag & 0b1000_0000) <> 0 -> (
        (* Printf.eprintf "%06x tag=0x%x key=%d\n" p tag key; *)
        (* this is a compressed header for a value block that includes a
           timestamp. We ignore the timestamp and only read the other
           fields. *)
        let key = (tag & 0b0110_0000) >> 5 in
        match Dict.find_opt key dict with
        | Some ty ->
            let arch = ty.arch in
            record arch ty >>= fun r -> return (dict, r :: rs)
        | None    ->
            pos >>= fun p ->
            fail_with "corrupted file? No type for key=%d offset=%d at %s" key p
              __LOC__)
    | n ->
        (* Printf.eprintf "%06x tag=0x%x key=%d\n" p tag key; *)
        (* _dump dict; *)
        fail_with "unexpected block with tag 0x%x at offset %d" n p

  let rec blocks xx finish =
    pos >>= fun p ->
    if p >= finish then return xx else block xx >>= fun xx -> blocks xx finish

  let read =
    let xx = (Dict.empty, []) in
    header >>= fun header ->
    pos >>= fun offset ->
    blocks xx (header.length + offset) >>= fun (_, records) ->
    return { header; records }
end

module MSG = struct
  let add map (k, v) = Dict.add k v map

  let msgs =
    [
      (0, "file_id")
    ; (1, "capabilities")
    ; (2, "device_settings")
    ; (3, "user_profile")
    ; (4, "hrm_profile")
    ; (5, "sdm_profile")
    ; (6, "bike_profile")
    ; (7, "zones_target")
    ; (8, "hr_zone")
    ; (9, "power_zone")
    ; (10, "met_zone")
    ; (12, "sport")
    ; (15, "goal")
    ; (18, "session")
    ; (19, "lap")
    ; (20, "record")
    ; (21, "event")
    ; (23, "device_info")
    ; (26, "workout")
    ; (27, "workout_step")
    ; (30, "weight_scale")
    ; (31, "course")
    ; (32, "course_point")
    ; (33, "totals")
    ; (34, "activity")
    ; (35, "software")
    ; (37, "file_capabilities")
    ; (38, "mesg_capabilities")
    ; (39, "field_capabilities")
    ; (49, "file_creator")
    ; (51, "blood_pressure")
    ]
    |> List.fold_left add Dict.empty

  let lookup key =
    match Dict.find_opt key msgs with
    | Some name -> name
    | None      -> string_of_int key
end

module Decode = struct
  let timestamp v =
    let offset = 631065600.0 in
    match v with
    | Int32 n -> Int32.to_float n +. offset
    | _       -> failwith "unexpected value"

  let scale scale offset v =
    let scale = Float.of_int scale in
    let offset = Float.of_int offset in
    match v with
    | Int x   -> (Float.of_int x /. scale) -. offset
    | Float x -> (x /. scale) -. offset
    | Int32 x -> (Int32.to_float x /. scale) -. offset
    | _       -> failwith "unexpected value"

  let latlon = function
    | Int32 x -> Int32.to_float x *. 180.0 /. 2147483648.0
    | _       -> failwith "unexpected value"
end

module JSON = struct
  let timestamp v =
    let offset = 631065600.0 in
    match v with
    | Int32 n ->
        `String
          (Int32.to_float n +. offset |> ISO8601.Permissive.string_of_datetime)
    | _       -> `Null

  let scale scale offset v =
    try `Float (Decode.scale scale offset v) with _ -> `Null

  let latlon v = try `Float (Decode.latlon v) with _ -> `Null

  let value msg pos v =
    match (msg, pos, v) with
    | 20, 0, v        -> ("latitude", latlon v)
    | 20, 1, v        -> ("longitude", latlon v)
    | 20, 2, v        -> ("altitude", scale 5 500 v)
    | 20, 3, v        -> ("heartrate", scale 1 0 v)
    | 20, 4, v        -> ("cadence", scale 1 0 v)
    | 20, 5, v        -> ("distance", scale 100 0 v)
    | 20, 6, v        -> ("speed", scale 1000 0 v)
    | 20, 13, v       -> ("temperature", scale 1 0 v)
    | _, 253, v       -> ("timestamp", timestamp v)
    | _, _, Enum n    -> (string_of_int pos, `Float (Float.of_int n))
    | _, _, String s  -> (string_of_int pos, `String s)
    | _, _, Int i     -> (string_of_int pos, `Float (Float.of_int i))
    | _, _, Int32 i32 -> (string_of_int pos, `Float (Int32.to_float i32))
    | _, _, Float f   -> (string_of_int pos, `Float f)
    | _, _, Unknown   -> (string_of_int pos, `Null)

  let field msg (pos, v) = value msg pos v

  let record r =
    `O (("msg", `String (MSG.lookup r.msg)) :: List.map (field r.msg) r.fields)
end

module Record = struct
  (** The messages with tag 20 (called "record") are at the heat of all
      FIT files as they contain the measurements. These records may
      contain many different values their presence cannot be expected.
      This module provides a representation for such records but covers
      only the most common values and is not comprehensive *)

  type t = {
      latitude : float option
    ; longitude : float option
    ; timestamp : float option
    ; altitude : float option
    ; heartrate : float option
    ; cadence : float option
    ; speed : float option
    ; distance : float option
    ; temperature : float option
  }

  (** if decoding fails, we record the field as not present *)
  let get slot fields decoder =
    List.assoc_opt slot fields |> function
    | Some x -> ( try Some (decoder x) with _ -> None)
    | None   -> None

  let record = function
    | { msg = 20; fields } -> (
        try
          Some
            {
              latitude = get 0 fields Decode.latlon
            ; longitude = get 1 fields Decode.latlon
            ; timestamp = get 253 fields Decode.timestamp
            ; altitude = get 2 fields (Decode.scale 5 500)
            ; heartrate = get 3 fields (Decode.scale 1 0)
            ; cadence = get 4 fields (Decode.scale 1 0)
            ; distance = get 5 fields (Decode.scale 100 0)
            ; temperature = get 13 fields (Decode.scale 1 0)
            ; speed = get 6 fields (Decode.scale 1000 0)
            }
        with _ -> None)
    | _                    -> None
end

let to_json fit = `A (List.rev_map JSON.record fit.records)

let records fit =
  List.fold_left
    (fun xs r -> match Record.record r with Some x -> x :: xs | None -> xs)
    [] fit.records

let read_file max_size path =
  let ic = open_in path in
  defer (fun () -> close_in ic) @@ fun () ->
  let size = in_channel_length ic in
  if size > max_size then failwith "input file exceeds maxium size"
  else really_input_string ic size

let read ?(max_size = 100 * 1024) path =
  let consume = Consume.Prefix in
  try read_file max_size path |> parse_string ~consume File.read
  with e ->
    Error (Printf.sprintf "Can't process %s: %s" path (Printexc.to_string e))
