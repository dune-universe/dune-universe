type 'err errors = 'err * 'err list

type ('input, 'output) check = 'input -> 'output option

type ('output, 'err) validator_result =
  ('output, 'err errors) result

type ('input, 'output, 'err) validator =
  'input -> ('output, 'err) validator_result

type ('input, 'output, 'err) validator_builder =
  'err -> ('input, 'output, 'err) validator

let custom
    (check : ('i, 'o) check)
    (error : 'e)
    (input : 'i) =
    match check input with
    | Some output ->
        Ok output
    | None ->
        Error (error, [ error ])


let int_min_check (min : int) (value : int) =
    if value < min then
      None
    else
      Some value


let int_min min = custom (int_min_check min)

let int_max_check (max : int) (value : int) =
    if value > max then
      None
    else
      Some value


let int_max max = custom (int_max_check max)

let list_is_not_empty_check (list : 'a list) :
    'a list option =
    if List.length list = 0 then
      None
    else
      Some list


let list_is_not_empty :
    ('a list, 'a list, 'err) validator_builder =
   fun err list -> custom list_is_not_empty_check err list


let list_has_min_length_check n list =
    if List.length list < n then
      None
    else
      Some list


let list_has_min_length :
    int -> ('a list, 'a list, 'err) validator_builder =
   fun len err list ->
    custom (list_has_min_length_check len) err list


let list_has_max_length_check n list =
    if List.length list > n then
      None
    else
      Some list


let list_has_max_length :
    int -> ('a list, 'a list, 'err) validator_builder =
   fun len err list ->
    custom (list_has_max_length_check len) err list


let list_every
    (validator : ('i, 'o, 'err) validator)
    (items : 'i list) =
    let results = List.map validator items in
    let errors =
        results
        |> List.map (fun result ->
               match result with
               | Ok _ ->
                   []
               | Error (_first, rest) ->
                   rest)
        |> List.flatten
    in
    let ok_items =
        List.filter_map Result.to_option results
    in
    match List.nth_opt errors 0 with
    | None ->
        Ok ok_items
    | Some head ->
        Error (head, errors)


let option_is_some : ('a option, 'a, 'err) validator_builder
    =
   fun err opt -> custom Fun.id err opt


let optional
    (validator : ('i, 'o, 'err) validator)
    (option : 'i option) =
    match option with
    | None ->
        Ok option
    | Some value -> (
        match validator value with
        | Ok _ ->
            Ok option
        | Error err ->
            Error err)


let string_is_not_empty_check (value : string) :
    string option =
    if value = "" then
      None
    else
      Some value


let string_is_not_empty :
    (string, string, 'err) validator_builder =
   fun err -> custom string_is_not_empty_check err


let string_is_int_check value =
    try Some (int_of_string value) with _ -> None


let string_is_int : (string, int, 'err) validator_builder =
   fun err -> custom string_is_int_check err


let string_has_min_length_check min value =
    if String.length value < min then
      None
    else
      Some value


let string_has_min_length min :
    (string, string, 'err) validator_builder =
    custom (string_has_min_length_check min)


let string_has_max_length_check max value =
    if String.length value > max then
      None
    else
      Some value


let string_has_max_length max :
    (string, string, 'err) validator_builder =
    custom (string_has_max_length_check max)


let string_is_email_check value =
    let regex = Str.regexp "^.+@.+$" in
    let has_at = Str.string_match regex value 0 in
    let splitted = String.split_on_char '@' value in
    let has_only_one_at = List.length splitted = 2 in
    if has_at && has_only_one_at then
      Some value
    else
      None


let string_is_email :
    (string, string, 'err) validator_builder =
   fun err -> custom string_is_email_check err


let keep
    (value : 'a)
    (acc : ('a -> 'next_acc', 'e errors) result) =
    match acc with
    | Error err ->
        Error err
    | Ok acc ->
        Ok (acc value)


let validate
    (input : 'i)
    (validator : ('i, 'o, 'e) validator)
    (accumulator : ('o -> 'next_acc, 'e errors) result) :
    ('next_acc, 'e errors) result =
    match validator input with
    | Ok valid_value ->
        accumulator
        |> Result.map (fun acc -> acc valid_value)
    | Error (e, errors) -> (
        match accumulator with
        | Ok _ ->
            Error (e, errors)
        | Error (first_error, previous_errors) ->
            Error
              ( first_error,
                [ previous_errors; errors ] |> List.flatten
              ))


let compose
    (validator2 : ('mid, 'o, 'err) validator)
    (validator1 : ('i, 'mid, 'err) validator) :
    ('i, 'o, 'err) validator =
   fun (i : 'i) -> Result.bind (validator1 i) validator2


let all
    (validators : ('io, 'io, 'err) validator list)
    (input : 'io) =
    let results =
        List.map
          (fun validator -> validator input)
          validators
    in
    let errors =
        results
        |> List.map (fun result ->
               match result with
               | Ok _ ->
                   []
               | Error (_first, rest) ->
                   rest)
        |> List.flatten
    in
    match List.nth_opt errors 0 with
    | None ->
        Ok input
    | Some head ->
        Error (head, errors)


let whole
    (validator : 'whole -> ('whole, 'err) result)
    (validation_result : ('whole, 'err) validator_result) =
    Result.bind validation_result (fun validated ->
        validator validated
        |> Result.map_error (fun error ->
               (error, [ error ])))


let build fn = Ok fn
