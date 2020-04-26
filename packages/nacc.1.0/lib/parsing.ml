(**************************************************************************)
(*                           _   _    _    ____ ____                      *)
(*                          | \ | |  / \  / ___/ ___|                     *)
(*                          |  \| | / _ \| |  | |                         *)
(*                          | |\  |/ ___ \ |__| |___                      *)
(*                          |_| \_/_/   \_\____\____|                     *)
(*                                                                        *)
(*                                                                        *)
(*                        Copyright (c) 2020 - CodeAnon                   *)
(**************************************************************************)

type 'a state = 'a option * int * int * string

type error = int * int * string

let state v o l r = (v, o, l, r)

let result_of_state = function
  | Some v, _, _, _ -> Ok v
  | None, o, l, r -> Error (o, l, r)


let state_value (x, _, _, _) = x

let state_offset (_, o, _, _) = o

let state_rest (_, _, _, r) = r

let state_line (_, _, l, _) = l

let update_stats (o1, l1) (v, o, l, r) = (v, o + o1, l + l1, r)

let report (o, l, _) =
  Printf.fprintf stdout "Parse error at line %d offset %d\n" o l


type 'a parser = P of (string -> 'a state)

let do_parse (P p) input =
  match p input with
  | Some x, _, _, "" -> Result.ok x
  | _, o, l, inp -> Result.error (o, l, inp)


let ( --> ) inp (P p) = p inp

let ( <-- ) (P p) inp = p inp

let pure x = P (fun input -> (Some x, 0, 0, input))

let ( <*> ) p1 p2 =
  let inner input =
    match p1 <-- input with
    | None, o, l, input -> (None, o, l, input)
    | Some f, o, l, input ->
      ( match p2 <-- input with
      | Some x, o', l', rest -> update_stats (o, l) (Some (f x), o', l', rest)
      | None, o', l', rest -> update_stats (o, l) (None, o', l', rest) )
  in
  P inner


let ( <$> ) f p = pure f <*> p

let ( *> ) p1 p2 = (fun _ y -> y) <$> p1 <*> p2

let ( <* ) p1 p2 = (fun x _ -> x) <$> p1 <*> p2

let ( <|> ) p1 p2 =
  let inner input =
    match p1 <-- input with
    | None, _, _, _ -> p2 <-- input
    | x -> x
  in
  P inner


let rec many p = P (fun inp -> List.cons <$> p <*> many p <|> pure [] <-- inp)

let some p = P (fun inp -> List.cons <$> p <*> many p <-- inp)

let check pred =
  let inner input =
    match input with
    | s when s <> "" && pred s.[0] ->
      ( Some s.[0]
      , 1
      , (if s.[0] = '\n' then 1 else 0)
      , String.(sub s 1 (length s - 1)) )
    | _ -> (None, 0, 0, input)
  in
  P inner


let ( ~~ ) f = P f

let ( let* ) p f =
  let inner input =
    match input --> p with
    | None, o, l, r -> (None, o, l, r)
    | Some v, o, l, r -> update_stats (o, l) (r --> f v)
  in
  P inner


let chainl op term =
  let rec loop v =
    (let* f = op in
     let* y = term in
     loop (f v y))
    <|> pure v
  in
  let* x = term in
  loop x


let chainr term op =
  let rec loop inp =
    let inner =
      (let* v = term in
       let* f = op in
       let* n = P (fun inp -> loop inp) in
       pure (f v n))
      <|> term
    in
    inp --> inner
  in
  P (fun inp -> loop inp)


let do_parse_from_file p f =
  let ic = open_in f in
  let s = ref "" in
  let ok = ref false in
  while not !ok do
    try s := !s ^ input_line ic ^ "\n" with
    | End_of_file -> ok := true
  done ;
  do_parse p !s
