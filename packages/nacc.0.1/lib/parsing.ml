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

(** Type parser *)
type 'a parser = P of (string -> ('a * string) option)

(** Pure parser (stop flux consumption, returns results) *)
let pure x = P (fun input -> Some (x, input))

(** Map *)
let ( <$> ) f (P p) =
  P
    (fun input ->
      match p input with None -> None | Some (x, next) -> Some (f x, next))

(** Apply *)
let ( <*> ) (P p1) (P p2) =
  P
    (fun input ->
      match p1 input with
      | None -> None
      | Some (f, input') -> (
          match p2 input' with
          | Some (x, input'') -> Some (f x, input'')
          | None -> None ))

(** Apply to the right *)
let ( *> ) p1 p2 = (fun _ y -> y) <$> p1 <*> p2

(** Apply to the left *)
let ( <* ) p1 p2 = (fun x _ -> x) <$> p1 <*> p2

(** Alternative *)
let ( <|> ) (P p1) (P p2) =
  P (fun input -> match p1 input with None -> p2 input | x -> x)

(** Run a parser on a string *)
let do_parse (P p) input =
  match p input with Some (x, "") -> Some x | _ -> None

(** Feed a parser with a string (from left to right)
    This is verry different from [do_parse] ! No verifications are made on the
    remaining chars. *)
let ( --> ) inp (P p) = p inp

(** Feed a parser with a string (from right to left)
    [p <-- input] is [input --> p]. This function is just for code convenience. *)
let ( <-- ) (P p) inp = p inp

(** Parse zero or more times a given pattern
    @param  p   a parser *)
let rec many p = P (fun inp -> List.cons <$> p <*> many p <|> pure [] <-- inp)

(** Parse one or more times a given pattern
    @param  p   a parser *)
let some p = P (fun inp -> List.cons <$> p <*> many p <-- inp)

(** Check a predicate on the first character of the input.
    Resolve to this character if the predicate is verified *)
let check pred =
  P
    (fun input ->
      match input with
      | s when s <> "" && pred s.[0] ->
          Some (s.[0], String.(sub s 1 (length s - 1)))
      | _ -> None)

(** Alias for constructor [P] *)
let ( ~~ ) f = P f
