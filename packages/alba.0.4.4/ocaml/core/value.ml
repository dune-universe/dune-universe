type t =
  | Int of int (* int32? *)
  | Char of int
  | String of string
  | Unary of (t -> t)
  | Binary of (t -> t -> t)


let int_value (str: string): t option =
  let len = String.length str
  in
  let rec value i v =
    if i = len then
      Some (Int v)
    else
      let v1 = v * 10 + (Char.code str.[i] - Char.code '0') in
      if v1 < v then
        None
      else
        value (i+1) v1
  in
  value 0 0


let number_values (str:string): t list =
  match int_value str with
  | None ->
     []
  | Some v ->
     [v]


let int_binary (f:int -> int -> int): t =
  Binary
    (fun a b ->
      match a, b with
      | Int a, Int b ->
         Int (f a b)
      | _ ->
         assert false (* Illegal call *)
    )


let int_unary (f:int -> int): t =
  Unary
    (fun a ->
      match a with
      | Int a ->
         Int (f a)
      | _ ->
         assert false (* Illegal call *)
    )


let string_binary (f:string -> string -> string): t =
  Binary
    (fun a b ->
      match a, b with
      | String a, String b ->
         String (f a b)
      | _ ->
         assert false (* Illegal call *)
    )


let int_plus: t =
  int_binary (+)


let int_minus: t =
  int_binary (-)


let int_negate: t =
  int_unary (~-)


let int_times: t =
  int_binary ( * )


let string_concat: t =
  string_binary (^)


let apply (f:t) (a:t): t =
  match f with
  | Unary f ->
     f a
  | Binary f ->
     Unary (f a)
  | _ ->
     assert false


let is_equal (a: t) (b: t): bool =
    match a, b with
    | Int a, Int b ->
        a = b
    | Char a, Char b ->
        a = b
    | String a, String b ->
        a = b
    | _ ->
        false


let compare (a: t) (b: t): int =
    match a, b with
    | Int a, Int b ->
        Stdlib.compare a b

    | Int _, _ ->
        -1

    | Char a, Char b ->
        Stdlib.compare a b

    | Char _, _ ->
        -1

    | String a, String b ->
        Stdlib.compare a b

    | _, _ ->
        assert false (* Illegal call! One of the values is a function. *)
