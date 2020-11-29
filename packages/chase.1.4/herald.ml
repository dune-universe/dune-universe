type herald = {
    bnd : int option;
    lmt : int option;
    in_ord : unit option;
  }

(* Parse one item in the herald *)
let parse_item hrld (keyword, value) =
  match keyword, value with
  | "bound", Some num ->
     Some { hrld with bnd = Some num }
  | "limit", Some num ->
     Some { hrld with lmt = Some num }
  | "input_order", None ->
     Some { hrld with in_ord = Some () }
  | _ -> None

(* [parse_herald] parses a herald line.  In returns [None] when
   herald line is malformed. *)
let rec parse_herald = function
  | [] -> Some { bnd = None;
                 lmt = None;
                 in_ord = None }
  | x :: xs ->
     match parse_herald xs with
     | None -> None
     | Some h -> parse_item h x

let empty_herald =
  { bnd = None;
    lmt = None;
    in_ord = None }
