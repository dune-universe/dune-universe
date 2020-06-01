module Stable = struct
  open Core.Core_stable

  module Name = struct
    module V1 = struct
      type t = string [@@deriving bin_io, compare, hash, sexp]
    end
  end

  module Value = struct
    module V1 = struct
      type t = string [@@deriving bin_io, compare, hash, sexp]
    end
  end

  module V1 = struct
    type t = (Name.V1.t * string) list [@@deriving bin_io, compare, hash, sexp]
  end
end

open Core

module Normalize = struct
  type encode =
    [ `None (* Leave whitespace unchanged *)
    | `Whitespace (* Cleanup leading and trailing whitespace on each line *)
    ]
  [@@deriving sexp_of]

  type decode =
    [ encode
    | `Whitespace_and_encoded_words
    ]
  [@@deriving sexp_of]

  let default : [> `Whitespace ] = `Whitespace
end

module Name : sig
  type t = string [@@deriving sexp_of, compare, hash]

  val of_string : string -> t
  val to_string : t -> string

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t

  val is : t -> string -> bool
end = struct
  include Mimestring.Case_insensitive

  let to_string str = str
  let is = equal_string
end

module Value : sig
  type t = string [@@deriving sexp_of, compare, hash]

  val of_string : ?normalize:Normalize.decode -> string -> t
  val to_string : ?normalize:Normalize.encode -> t -> string
  val to_string' : ?normalize:Normalize.decode -> t -> string

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
end = struct
  include String

  let normalize_string str =
    String.split_lines str |> List.map ~f:String.strip |> String.concat ~sep:"\n"
  ;;

  let of_string ?(normalize = Normalize.default) str =
    match normalize with
    | `None -> str
    | `Whitespace -> normalize_string str
    | `Whitespace_and_encoded_words ->
      (match Encoded_word.decode str with
       | Ok str -> str
       | Error _ -> normalize_string str)
  ;;

  let to_string ?(normalize = Normalize.default) str =
    match normalize with
    | `None -> str
    | `Whitespace ->
      " "
      ^ (String.split_lines str |> List.map ~f:String.strip |> String.concat ~sep:"\n\t")
  ;;

  let to_string' ?(normalize = Normalize.default) str =
    match normalize with
    | #Normalize.encode as normalize -> to_string ~normalize str
    | `Whitespace_and_encoded_words -> to_string ~normalize:`Whitespace str
  ;;
end

module Common = struct
  let subject = "Subject"
  let to_ = "To"
  let cc = "Cc"
  let bcc = "Bcc"
  let from = "From"
  let date = "Date"
  let message_id = "Message-ID"
end

type t = (Name.t * string) list [@@deriving sexp_of, compare, hash]

let to_string_monoid ?(eol = `LF) t =
  List.map t ~f:(fun (name, value) ->
    String_monoid.concat_string
      [ (name :> string); ":"; value; Lf_or_crlf.to_string eol ])
  |> String_monoid.concat
;;

let to_string ?eol t = String_monoid.to_string (to_string_monoid ?eol t)
let empty = []
let append = List.append

(* Accessors *)
let last ?normalize t name =
  let name = Name.of_string name in
  List.fold t ~init:None ~f:(fun r (k, v) -> if Name.equal name k then Some v else r)
  |> Option.map ~f:(Value.of_string ?normalize)
;;

let find_all ?normalize t name =
  let name = Name.of_string name in
  List.filter_map t ~f:(fun (name', value) ->
    if Name.equal name name' then Some (Value.of_string ?normalize value) else None)
;;

(* Modify *)
let of_list ~normalize : _ -> t =
  List.map ~f:(fun (name, value) ->
    let name = Name.of_string name in
    let value = Value.to_string ~normalize value in
    name, value)
;;

let to_list ?normalize : t -> _ =
  List.map ~f:(fun (name, value) -> name, Value.of_string ?normalize value)
;;

let add ?normalize t ~name ~value =
  let name = Name.of_string name in
  let value = Value.to_string ?normalize value in
  let rec add acc = function
    | (name', _) :: _ as fields when Name.equal name name' ->
      List.rev acc @ [ name, value ] @ fields
    | field :: fields -> add (field :: acc) fields
    | [] -> (name, value) :: t
  in
  add [] t
;;

let add_if_missing ?normalize t ~name ~value =
  if List.Assoc.mem t ~equal:Name.equal name then t else add ?normalize t ~name ~value
;;

let set ?normalize t ~name ~value =
  let name = Name.of_string name in
  let value = Value.to_string ?normalize value in
  let rec set acc = function
    | (name', _) :: fields when Name.equal name name' ->
      List.rev acc @ [ name, value ] @ fields
    | field :: fields -> set (field :: acc) fields
    | [] -> (name, value) :: t
  in
  set [] t
;;

let add_at_bottom ?normalize t ~name ~value =
  List.rev (add ?normalize (List.rev t) ~name ~value)
;;

let add_at_bottom_if_missing ?normalize t ~name ~value =
  if List.Assoc.mem t ~equal:Name.equal name
  then t
  else add_at_bottom ?normalize t ~name ~value
;;

let set_at_bottom ?normalize t ~name ~value =
  List.rev (set ?normalize (List.rev t) ~name ~value)
;;

let add_all ?normalize t ts : t =
  List.fold
    ~init:t
    ~f:(fun t (name, value) -> add ?normalize t ~name ~value)
    (List.rev ts)
;;

let add_all_at_bottom ?normalize t ts =
  List.fold
    ~init:t
    ~f:(fun t (name, value) -> add_at_bottom ?normalize t ~name ~value)
    ts
;;

let filter ?normalize t ~f =
  List.filter t ~f:(fun (name, value) ->
    f ~name ~value:(Value.of_string ?normalize value))
;;

let map' ?normalize t ~f =
  List.map t ~f:(fun ((name : Name.t), (value_raw : string)) ->
    let value = Value.of_string ?normalize value_raw in
    let name', value' = f ~name ~value in
    let value =
      if String.equal (value :> string) value'
      then value_raw
      else Value.to_string' ?normalize value'
    in
    name', value)
;;

let map ?normalize t ~f = map' ?normalize t ~f:(fun ~name ~value -> name, f ~name ~value)

let smash_and_add ?normalize t ~name ~value =
  let values =
    find_all
      ?normalize:((normalize : Normalize.encode option) :> Normalize.decode option)
      t
      name
  in
  let t = filter t ~f:(fun ~name:name' ~value:_ -> Name.(name <> name')) in
  let value = String.concat (values @ [ value ]) ~sep:", " in
  add_at_bottom ?normalize t ~name ~value
;;

let names = List.map ~f:fst

let%test_module _ =
  (module struct
    let t = of_list ~normalize:`None [ "A", "a1"; "B", "b1"; "B", "b2" ]

    let%test_unit _ = [%test_result: string] (to_string t) ~expect:"A:a1\nB:b1\nB:b2\n"

    let%test_unit _ =
      [%test_result: string]
        (add ~normalize:`None t ~name:"B" ~value:"b3" |> to_string)
        ~expect:"A:a1\nB:b3\nB:b1\nB:b2\n"
    ;;

    let%test_unit _ =
      [%test_result: string]
        (add ~normalize:`None t ~name:"B" ~value:"b3\nb3" |> to_string)
        ~expect:"A:a1\nB:b3\nb3\nB:b1\nB:b2\n"
    ;;

    let%test_unit _ =
      [%test_result: string]
        (add t ~name:"B" ~value:"b3" |> to_string)
        ~expect:"A:a1\nB: b3\nB:b1\nB:b2\n"
    ;;

    let%test_unit _ =
      [%test_result: string]
        (add t ~name:"B" ~value:"b3\nb3" |> to_string)
        ~expect:"A:a1\nB: b3\n\tb3\nB:b1\nB:b2\n"
    ;;

    let%test_unit _ =
      [%test_result: string]
        (add ~normalize:`None t ~name:"C" ~value:"c1" |> to_string)
        ~expect:"C:c1\nA:a1\nB:b1\nB:b2\n"
    ;;

    let%test_unit _ =
      [%test_result: string]
        (set ~normalize:`None t ~name:"B" ~value:"b3" |> to_string)
        ~expect:"A:a1\nB:b3\nB:b2\n"
    ;;

    let%test_unit _ =
      [%test_result: string]
        (set ~normalize:`None t ~name:"b" ~value:"b3" |> to_string)
        ~expect:"A:a1\nb:b3\nB:b2\n"
    ;;

    let%test_unit _ =
      [%test_result: string]
        (set ~normalize:`None t ~name:"C" ~value:"c1" |> to_string)
        ~expect:"C:c1\nA:a1\nB:b1\nB:b2\n"
    ;;

    let%test_unit _ =
      [%test_result: string]
        (set ~normalize:`None t ~name:"c" ~value:"c1" |> to_string)
        ~expect:"c:c1\nA:a1\nB:b1\nB:b2\n"
    ;;

    let%test_unit _ =
      [%test_result: string]
        (add_at_bottom ~normalize:`None t ~name:"A" ~value:"a2" |> to_string)
        ~expect:"A:a1\nA:a2\nB:b1\nB:b2\n"
    ;;

    let%test_unit _ =
      [%test_result: string]
        (add_at_bottom ~normalize:`None t ~name:"B" ~value:"b3" |> to_string)
        ~expect:"A:a1\nB:b1\nB:b2\nB:b3\n"
    ;;

    let%test_unit _ =
      [%test_result: string]
        (add_at_bottom ~normalize:`None t ~name:"C" ~value:"c1" |> to_string)
        ~expect:"A:a1\nB:b1\nB:b2\nC:c1\n"
    ;;

    let%test_unit _ =
      [%test_result: string]
        (set_at_bottom ~normalize:`None t ~name:"B" ~value:"b3" |> to_string)
        ~expect:"A:a1\nB:b1\nB:b3\n"
    ;;

    let%test_unit _ =
      [%test_result: string]
        (set_at_bottom ~normalize:`None t ~name:"C" ~value:"c1" |> to_string)
        ~expect:"A:a1\nB:b1\nB:b2\nC:c1\n"
    ;;
  end)
;;
