external id : 'a -> 'a = "%identity"
external (&) : ('a -> 'b) -> 'a -> 'b = "%apply"
external (&~) : (f:'a -> 'b) -> 'a -> 'b = "%apply"

type ('a, 'b) poly_result = [ `Ok of 'a | `Error of 'b ]

let memoize_gen f =
  let cache = Hashtbl.create 101 in
  let rec g v = 
    try Hashtbl.find cache v with Not_found ->
      let r = f g v in
      Hashtbl.replace cache v r;
      r
  in
  g, cache

let memoize f = fst & memoize_gen & fun _self -> f

let memoize_rec f = fst & memoize_gen f

let ( ** ) f g = fun x -> f (g x)
let ( *< ) = ( ** )
let ( *> ) f g = fun x -> g (f x)

external power : float -> float -> float = "caml_power_float" "pow" [@@unboxed] [@@noalloc]

let time f v =
  let start = Unix.gettimeofday () in
  let res = f v in
  let end_ = Unix.gettimeofday () in
  res, end_ -. start

let with_ref init f =
  let r = ref init in
  let res = f r in
  !r, res

let with_ref_ init f = fst (with_ref init f)
 
(* Printf *)
let sprintf = Printf.sprintf
let ksprintf = Printf.ksprintf
let (!%) = Printf.sprintf
let (!!%) = Format.eprintf

(*
let it_was_called_with_ref r v f =
  let back_v = !r in
  r := v;
  Exn.protect f () ~finally:(fun () -> r := back_v)
*)
  

let with_oc oc f = 
  Exn.protect f oc ~finally:close_out

let with_ic ic f = 
  Exn.protect f ic ~finally:close_in

let (|-) res f = f res; res

let flip f x y = f y x
let (~~) g ~f = g f
let flipf = (~~)

let flip2 f a2 a3 a1 = f a1 a2 a3
    
let (+=) r v = r := !r + v
let (-=) r v = r := !r - v

let find_by_iter iter p col =
  let result = ref None in
  try
    iter (fun e ->
      if p e then begin
        result := Some e;
        raise Exit
      end) col;
    None
  with
  | Exit -> !result

let find_in_tree visit p col =
  let rec loop = function
    | [] -> None
    | x::xs ->
        match p x with
        | Some v -> Some v
        | None -> loop (visit x @ xs)
  in
  loop [col]

let rec loop f st =
  match f st  with
  | `Continue st -> loop f st
  | `Break res -> res

let compare_on f x y = compare (f x) (f y)
let rev_compare_on f x y = compare (f y) (f x)

(** returns [true] when not found *)
let add_if_not_mem x r =
  if List.mem x !r then `AlreadyIn else begin r := x::!r; `NewlyAdded end

let (!<$) = Bytes.of_string
let (!>$) = Bytes.to_string
