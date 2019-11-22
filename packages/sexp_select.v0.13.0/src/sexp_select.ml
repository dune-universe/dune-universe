open! Base

module Action = struct
  type ident =
    [ `star
    | `string of string
    | `one_of of Set.M(String).t
    ]

  type t =
    [ `descendants of ident (* All descendants matching [ident] *)
    | `children of ident (* All direct children matching [ident] *)
    ]
end

module Eval = struct
  let matches (atom : string) = function
    | `star -> true
    | `string s -> String.( = ) atom s
    | `one_of set -> Set.mem set atom
  ;;

  let rec descendants name : Sexp.t -> _ = function
    | List [ Atom key; value ] when matches key name -> value :: descendants name value
    | List l -> List.bind l ~f:(descendants name)
    | Atom _ -> []
  ;;

  let children name : Sexp.t -> _ = function
    | Atom _ -> []
    | List l ->
      List.filter_map l ~f:(function
        | List [ Atom key; value ] -> if matches key name then Some value else None
        | List ([] | [ _ ] | [ List _; _ ] | _ :: _ :: _ :: _) | Atom _ -> None)
  ;;
end

module Parse = struct
  let parse_ident tokens =
    match tokens with
    | "*" :: rest -> `star, rest
    | "(" :: rest ->
      let idents, rest =
        List.split_while rest ~f:(function
          | "(" | ")" -> false
          | _ -> true)
      in
      (match rest with
       | ")" :: rest -> `one_of (Set.of_list (module String) idents), rest
       | "(" :: _ ->
         Printf.failwithf
           "nested parens are not supported: '%s'"
           (String.concat ~sep:" " tokens)
           ()
       | _ -> Printf.failwithf "unterminated ( in '%s'" (String.concat ~sep:" " tokens) ())
    | ident :: rest -> `string ident, rest
    | [] -> assert false
  ;;

  let parse_one = function
    (* This actually needn't return option, since we always have a valid parse. But we
       leave it as-is for future language extensions that might be more restrictive. *)
    | [] -> None
    | ">" :: rest ->
      let ident, rest = parse_ident rest in
      Some (`children ident, rest)
    | rest ->
      let ident, rest = parse_ident rest in
      Some (`descendants ident, rest)
  ;;

  let parse s =
    let rec loop tokens =
      match parse_one tokens with
      | Some (action, rest) -> action :: loop rest
      | None -> []
    in
    loop (String.split s ~on:' ')
  ;;
end

let select program_string sexp =
  let rec loop actions sexp =
    match (actions : Action.t list) with
    | [] -> [ sexp ]
    | `descendants ident :: rest ->
      List.bind (Eval.descendants ident sexp) ~f:(loop rest)
    | `children ident :: rest -> List.bind (Eval.children ident sexp) ~f:(loop rest)
  in
  loop (Parse.parse program_string) sexp
;;
