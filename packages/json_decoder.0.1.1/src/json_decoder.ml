module Dict = Map.Make(String)

type 'a dict = 'a Dict.t

let always x _ = x

module Results = struct
  let map f = function
    | Result.Ok v -> Result.Ok (f v)
    | Result.Error _ as err -> err

  let bind f = function
    | Result.Ok v -> f v
    | Result.Error _ as err -> err

  let is_ok = function
    | Result.Ok _ -> true
    | Result.Error _ -> false

  let (>>=) t f = bind f t

  let (>>|) t f = map f t

  let (<*>) f t = f >>= fun f -> t >>| f

  let (<$>) f t = t >>| f
end

type value = Yojson.Basic.json

type 'a t =
  | Decoder of ( value -> ('a, string) Result.result )

let describe_value = function
  | `String _ -> "string"
  | `Float _ -> "float"
  | `Int _ -> "int"
  | `Bool _ -> "bool"
  | `Null -> "null"
  | `Assoc _ -> "object"
  | `List _ -> "list"

let value_error s value =
  describe_value value
  |> Printf.sprintf s
  |> ( fun s -> Result.Error s )

let decode ( Decoder f ) value = f value

let string =
  Decoder begin function
    | `String s -> Result.Ok s
    | otherwise -> value_error "%s is not a string" otherwise
  end

let float =
  Decoder begin function
    | `Float f -> Result.Ok f
    | otherwise -> value_error "%s is not a float" otherwise
  end

let int =
  Decoder begin function
    | `Int i -> Result.Ok i
    | otherwise -> value_error "%s is not a int" otherwise
  end

let bool =
  Decoder begin function
    | `Bool b -> Result.Ok b
    | otherwise -> value_error "%s is not a bool" otherwise
  end

let null a =
  Decoder begin function
    | `Null -> Result.Ok a
    | otherwise -> value_error "%s is not null" otherwise
  end

let map f decoder =
  let open Results in
  Decoder begin fun value ->
    decode decoder value >>| f
  end

let list decoder =
  Decoder begin function
    | `List values ->
      let open Results in
      let push x xs = x :: xs in

      List.fold_right begin fun value acc ->
        push <$> decode decoder value <*> acc
      end
        values
        ( Result.Ok [] )
    | otherwise -> value_error "%s is not a list" otherwise
  end

let dict decoder =
  Decoder begin function
    | `Assoc values ->
      let open Results in
      let add key value dct = Dict.add key value dct in

      List.fold_left begin fun dct (key, value) ->
        add key <$> decode decoder value <*> dct
      end
        ( Result.Ok Dict.empty )
        values
    | otherwise -> value_error "%s is not a dict" otherwise
  end

let pairs decoder =
  Decoder begin function
    | `Assoc values ->
      let open Results in
      let push key value sofar = (key, value) :: sofar in

      List.fold_right begin fun (key, value) acc ->
        push key <$> decode decoder value <*> acc
      end
        values
        ( Result.Ok [] )
    | otherwise -> value_error "%s is not a dict" otherwise
  end

let array decoder =
  map Array.of_list ( list decoder )

let field key decoder =
  Decoder begin function
    | `Assoc a ->
      let value = try
          decode decoder @@ List.assoc key a
        with
          Not_found ->
          let keys =
            a
            |> List.map fst
            |> String.concat ", "
          in
          Result.Error ( Printf.sprintf "key %s does not exist in object %s " key keys)
      in
      value
    | otherwise -> value_error "%s is not an object" otherwise
  end

let (@=) = field

let at keys decoder =
  List.fold_right field keys decoder

let index i decoder =
  Decoder begin function
    | `List values ->
      let value = try
          decode decoder @@ List.nth values i
        with Failure _ ->
          Result.Error "index out of bounds"
      in
      value
    | otherwise -> value_error "%s is not a list" otherwise
  end


let one_of decoders =
  Decoder begin fun value ->
    let values =
      List.map ( fun decoder -> decode decoder value ) decoders in
    try
      List.find Results.is_ok values
    with
      Not_found -> Result.Error "no suitable decoder chosen"
  end

let succeed a = Decoder ( always @@ Result.Ok a )

let fail s = Decoder ( always @@ Result.Error s )

let option decoder =
  Decoder begin fun value ->
    let opt =
      match decode decoder value with
      | Result.Ok c -> Some c
      | Result.Error _ -> None
    in
    Result.Ok opt
  end

let value =
  Decoder begin fun value ->
    Result.Ok value
  end

let and_then fn decoder =
  Decoder begin fun value ->
    let ( Decoder callback ) =
      match decode decoder value with
      | Result.Ok c -> fn c
      | Result.Error e -> fail e
    in
    callback value
  end

let (>>=) decoder fn = and_then fn decoder

let (>>|) t f = map f t

let (<*>) f t = f >>= fun f -> t >>| f

let mapN f = succeed f

let (||>) f t = f <*> t

let apply f t = f <*> t

let value_of_string s = Yojson.Basic.from_string s

let value_to_string v = Yojson.Basic.to_string v

let value_to_yojson v = v

let value_of_yojson v = v

let decode_string t s =
  decode t @@ value_of_string s
