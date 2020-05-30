(* command line argument spec *)

open Base
module Hashtbl = struct
  include Hashtbl
  include Xhashtbl
end
module List = struct
  include List
  include Xlist
end

type ('a, 'err) opt = { 
  short : char option;
  long  : string option;
  arg : [ `Nullary of 'a 
        | `Unary of (string -> ('a, 'err) Vresult.t) ]
}

(*
let opt_to_string opt =
  match opt.short, opt.long with
  | None, None -> assert false
  (* nullary *)
  | Some short, None ->      "  -%c                         %s"
  | Some short, Some long -> "  -%c, --%s                   %s"
  | None, Some long ->       "       --%s                   %s"
  | Some short, None ->      "  -%c XXX                     %s"
  | Some short, Some long -> "  -%c, --%s=XXX               %s"
  | None, Some long ->       "       --%s=XXX               %s"
*)

module Error = struct
  type t = 
    [ `Ambiguous of string * string * string
    | `Requires_argument of string
    | `Nullary_takes_argument of string
    | `Unknown of string 
    ]

  let to_string = function
    | `Ambiguous (sw, k1, k2) -> !% "Switch --%s is ambigous: it may be --%s or --%s" sw k1 k2
    | `Requires_argument sw -> !% "Switch %s requires an argument" sw
    | `Nullary_takes_argument sw -> !% "Switch %s does not take an argument" sw
    | `Unknown sw -> !% "Unknown switch %s" sw
end

let nullary short long arg = 
  match short, long with
  | None, None -> assert false
  |_ -> { short; long; arg = `Nullary arg }

let unary short long arg = 
  match short, long with
  | None, None -> assert false
  |_ -> { short; long; arg = `Unary arg }

type ('a, 'err) t = {
  shorts : (char,    ('a, 'err) opt) Hashtbl.t;
  longs  : (string * ('a, 'err) opt) list;
}

let make opts =
  let shorts = Hashtbl.create 107 in
  List.iter (function 
    | { short= None } -> ()
    | ({ short= Some c } as o) ->
        Hashtbl.alter shorts c (function
          | Some _ -> assert false
          | None -> Some o)) opts;
  let longs = List.filter_map (function
    | { long= None } -> None
    | ({ long=Some s } as o) -> Some (s, o)) opts
  in
  let keys = List.map fst longs in
  let rec check st = function
    | [] -> ()
    | x::_ when List.mem x st -> assert false
    | x::xs -> check (x::st) xs
  in
  check [] keys;
  { shorts; longs }
    

let string_tail s from = String.sub s from (String.length s - from)
    
let rec parse t st = function
  | [] -> Ok (List.rev st)
  | arg::args -> 
      match arg with
      | _ when String.length arg = 1 -> parse t (`Anon arg :: st) args
      | "--" -> Ok (List.rev_append st (List.map (fun x -> `Anon x) args))
      | _ ->
          match arg.[0], arg.[1] with
          | '\\', '-' -> parse t (`Anon (string_tail arg 1) :: st) args
          | '-', '-' -> parse_long_switch t st (string_tail arg 2) args
          | '-', _ -> parse_short_switch t st (string_tail arg 1) args
          | _ -> parse t (`Anon arg :: st) args
      
and parse_short_switch t st sw args =
  let len = String.length sw in
  let rec parse_sw st char_pos =
    if len <= char_pos then parse t st args
    else
      let sw_char = sw.[char_pos] in
      try
        let switch = Hashtbl.find t.shorts sw_char in
        match switch.arg with
        | `Unary f when len = char_pos + 1 ->
            get_parameter t st f (!% "-%c" sw_char) args
        | `Unary _ -> Error (`Requires_argument (!% "-%c" sw_char))
        | `Nullary v -> parse_sw (v :: st) (char_pos+1)
      with
      | Not_found -> Error (`Unknown (!% "-%c" sw_char))
  in
  parse_sw st 0
      
and get_parameter t st f name = function
  | [] -> Error (`Requires_argument name)
  | arg::args -> 
      match f arg with
      | Ok v -> parse t (v :: st) args
      | Error e -> Error e

and parse_long_switch t st sw args =
  let sw, param =
    try
      let pos = String.index sw '=' in
      String.sub sw 0 pos, Some (string_tail sw (pos + 1))
    with
    | Not_found -> sw, None
  in
  let do_found = function
    | None -> Error (`Unknown ("--" ^ sw))
    | Some (`Match switch | `Partial (_, switch)) ->
        match switch.arg, param with
        | `Unary f, Some param -> 
            begin match f param with
            | Ok v -> parse t (v :: st) args
            | Error e -> Error e
            end
        | `Nullary v, None -> parse t (v :: st) args
        | `Nullary _, Some _ -> Error (`Nullary_takes_argument ("--" ^ sw))
        | `Unary _,   None -> Error (`Requires_argument ("--" ^ sw))
  in
  let rec find found = function
    | [] -> do_found found
    | (k,switch) :: kss ->
        let match_ = 
          if sw = k then `Match
          else 
            try 
              if sw = String.sub k 0 (String.length sw) then `Partial else `No_match
            with _ -> `No_match
        in
        match match_, found with
        | `Match, _ -> do_found (Some (`Match switch))
        | `Partial, None -> find (Some (`Partial (k, switch))) kss
        | `Partial, (Some (`Match _)) -> find found kss
        | `Partial, (Some (`Partial (k', _))) -> Error (`Ambiguous (sw, k, k'))
        | `No_match, _ -> find found kss
  in
  find None t.longs

let parse opts args = parse (make opts) [] args

let %TEST long_amb_nullary_ = 
  let long1 = nullary None (Some "long") `Long in
  let long2 = nullary None (Some "lo")   `Lo in
  match parse [ long1; long2 ] [ "--long"; "--lon"; "--lo" ] with
  | Ok [ `Long; `Long; `Lo ] -> ()
  | Ok _ -> assert false
  | Error _ -> assert false

let %TEST long_amb_unary_ =
    let long1 = unary None (Some "long") (fun x -> Ok (`Long x)) in
    let long2 = unary None (Some "lo")   (fun x -> Ok (`Lo x)) in
    match parse [ long1; long2 ] [ "--long=x"; "--lon=x"; "--lo=x" ] with
    | Ok [ `Long "x"; `Long "x"; `Lo "x" ] -> ()
    | Ok _ -> assert false
    | Error _ -> assert false

let %TEST long_unary_without_arg_ =
    let long = unary None (Some "long") (fun x -> Ok (`Long x)) in
    match parse [ long ] [ "--long" ] with
    | Error (`Requires_argument "--long") -> ()
    | _ -> assert false

let %TEST long_nullary_with_arg_ =
  let long = nullary None (Some "long") `Long in
  match parse [ long ] [ "--long=x" ] with
  | Error (`Nullary_takes_argument "--long") -> ()
  | _ -> assert false

let %TEST long_amb_unary_error_ =
  let long1 = unary None (Some "long") (fun x -> Ok (`Long x)) in
  let long2 = unary None (Some "lo")   (fun x -> Ok (`Lo x)) in
  match parse [ long1; long2 ] [ "--l=x" ] with
  | Error (`Ambiguous ("l", _, _)) -> ()
  | _ -> assert false

let %TEST long_amb_nullary_error_ =
  let long1 = nullary None (Some "long") `Long in
  let long2 = nullary None (Some "lo")   `Lo in
  match parse [ long1; long2 ] [ "--l" ] with
  | Error (`Ambiguous ("l", _, _)) -> ()
  | _ -> assert false

let %TEST short_unary_without_arg_ =
  let short1 = unary (Some 'x') None (fun x -> Ok (`X x)) in
  match parse [ short1 ] [ "-x" ] with
  | Error (`Requires_argument "-x") -> ()
  | _ -> assert false

let %TEST short_unary_without_arg2_ =
    let short1 = unary (Some 'x') None (fun x -> Ok (`X x)) in
    let short2 = unary (Some 'z') None (fun x -> Ok (`Z x)) in
    match parse [ short1; short2 ] [ "-xz"; "hello" ] with
    | Error (`Requires_argument "-x") -> ()
    | _ -> assert false

let %TEST short_nullary_many_ =
  let short_a = nullary (Some 'a') None `a in
  let short_b = nullary (Some 'b') None `b in
  let short_c = nullary (Some 'c') None `c in
  match parse [ short_a; short_b; short_c ] [ "-abcba"; "hello" ] with
  | Ok [ `a; `b; `c; `b; `a; `Anon "hello" ] -> ()
  | _ -> assert false
