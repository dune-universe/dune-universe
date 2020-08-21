open Base
(* prevent Format being shadowed by Base.Format, which is deprecated *)
module Format = Caml.Format

module Parser = struct
  module Rule = struct
    type t = {
      regex: Re.re;
      replacements: (string * string) list;
    }

    let of_yojson: Yojson.Safe.t -> (t, _) Result.t = function
      | `Assoc assoc -> 
        let open Result.Let_syntax in
        let%bind pattern =
          List.Assoc.find assoc ~equal:String.equal "regex"
          |> function
            | Some (`String s) -> Ok s
            | Some _
            | None -> Error "invalid json"
        in
        let flags =
          List.Assoc.find assoc ~equal:String.equal "regex_flag"
          |> function Some `String "i" -> [`CASELESS] | _ -> []
        in
        let%bind regex =
          Result.try_with (fun () -> Re.Pcre.regexp ~flags pattern)
          |> Result.map_error ~f:(function _ -> "invalid json")
        in
        let%bind replacements = Result.all @@ List.fold_right assoc ~init:[] ~f:(fun o acc ->
          match o with
          | ("regex", _) -> acc 
          | ("regex_flag", _) -> acc 
          | (k, `String v) -> (Ok (k, v)) :: acc
          | _ -> (Error "invalid json") :: acc)
        in
        return { regex; replacements }
      | _ -> failwith "invalid json"

    let apply t s =
      match Re.exec_opt t.regex s with
      | Some groups -> Some groups
      | None -> None
  end

  module Match = struct
    type groups = string list
    type replacers = (string * string) list
    type t = groups * replacers

    let replace_regex = Re.Pcre.regexp "\\$\\d+"

    let replace s matches =
      Re.replace replace_regex s ~all:true ~f:(fun m ->
        Option.value ~default:"" @@ match (Re.Group.get m 0) with
        | "$1" -> List.nth matches 1
        | "$2" -> List.nth matches 2
        | "$3" -> List.nth matches 3
        | "$4" -> List.nth matches 4
        | "$5" -> List.nth matches 5
        | "$6" -> List.nth matches 6
        | "$7" -> List.nth matches 7
        | "$8" -> List.nth matches 8
        | "$9" -> List.nth matches 9
        | _ -> None)

    let get (groups, replacers) (s, idx) =
      let r = match List.Assoc.find replacers  ~equal:String.equal s with
        | Some rep -> Some (replace rep groups)
        | None -> List.nth groups idx
      in
      Option.map r ~f:String.strip |> function Some "" -> None | x -> x
  end

  type t = Rule.t list

  let of_yojson: Yojson.Safe.t -> (t, _) Result.t = function
    | `List seq -> List.map seq ~f:Rule.of_yojson |> Result.all
    | _ -> Error "invalid json"
  
  let apply: t -> string -> Match.t = fun t s ->
    List.find_map t ~f:(fun rule ->
      match Rule.apply rule s with
      | Some groups -> Some (Re.Group.all groups |> Array.to_list, rule.replacements)
      | None -> None)
    |> Option.value ~default:([ "Other" ], [])
end


module UAParser = struct
  type t = Parser.t

  type result = {
    family: string;
    major: string option;
    minor: string option;
    patch: string option;
  } [@@deriving eq, show]

  let init () =
    (match User_agent_regexes.json with
    | Ok (`Assoc assoc) ->
      let open Result in
      List.Assoc.find assoc ~equal:String.equal "user_agent_parsers"
      |> Result.of_option ~error:"user_agent_parsers"
      >>= Parser.of_yojson
    | Ok _ -> Error "invalid json"
    | Error (`Msg s) -> Error (Printf.sprintf "invalid json: %s" s))
    |> Result.ok_or_failwith


  let parse t ua =
    let result = Parser.apply t ua in
    {
      family = Parser.Match.get result ("family_replacement", 1) |> Option.value ~default:"Other";
      major = Parser.Match.get result ("v1_replacement", 2);
      minor = Parser.Match.get result ("v2_replacement", 3);
      patch = Parser.Match.get result ("v3_replacement", 4);
    }
end

module OSParser = struct
  type t = Parser.t

  type result = {
    family: string;
    major: string option;
    minor: string option;
    patch: string option;
    patch_minor: string option;
  } [@@deriving eq, show]

  let init () =
    (match Os_regexes.json with
    | Ok `Assoc assoc ->
      let open Result in
      List.Assoc.find assoc ~equal:String.equal "os_parsers"
      |> Result.of_option ~error:"os_parsers"
      >>= Parser.of_yojson
    | Ok _ -> Error "invalid json"
    | Error (`Msg s) -> Error (Printf.sprintf "invalid json: %s" s))
    |> Result.ok_or_failwith

  let parse t ua =
    let result = Parser.apply t ua in
    {
      family = Parser.Match.get result ("os_replacement", 1) |> Option.value ~default:"Other";
      major = Parser.Match.get result ("os_v1_replacement", 2);
      minor = Parser.Match.get result ("os_v2_replacement", 3);
      patch = Parser.Match.get result ("os_v3_replacement", 4);
      patch_minor = Parser.Match.get result ("os_v4_replacement", 5);
    }
end

module DeviceParser = struct
  type t = Parser.t

  type result = {
    family: string;
    brand: string option;
    model: string option;
  } [@@deriving eq, show]

  let init () =
    (match Device_regexes.json with
    | Ok `Assoc assoc ->
      let open Result in
      List.Assoc.find assoc ~equal:String.equal "device_parsers"
      |> Result.of_option ~error:"device_parsers"
      >>= Parser.of_yojson
    | Ok _ -> Error "invalid json"
    | Error (`Msg s) -> Error (Printf.sprintf "invalid json: %s" s))
    |> Result.ok_or_failwith

  let parse t ua =
    let result = Parser.apply t ua in
    {
      family = Parser.Match.get result ("device_replacement", 1) |> Option.value ~default:"Other";
      brand = Parser.Match.get result ("brand_replacement", 2);
      (* Not a bug, model replacement is supposed to be 1, see JS ref implementation *)
      model = Parser.Match.get result ("model_replacement", 1);
    }
end

type t = {
  ua_parser: UAParser.t;
  os_parser: OSParser.t;
  device_parser: DeviceParser.t;
}

type result = {
  ua: UAParser.result;
  os: OSParser.result;
  device: DeviceParser.result;
} [@@deriving eq, show]

let init () = {
  ua_parser = UAParser.init ();
  os_parser = OSParser.init ();
  device_parser = DeviceParser.init();
}

let parse t s = {
  ua = UAParser.parse t.ua_parser s;
  os = OSParser.parse t.os_parser s;
  device = DeviceParser.parse t.device_parser s;
}
