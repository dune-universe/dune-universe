type ppf = Pretty.t
type 'a t = ppf -> 'a -> ppf

type ('ty, 'v) order =
  | Const : 'a t * 'a -> ('v, 'v) order
  | Atom : 'a t -> ('a -> 'v, 'v) order
  | Param : ('a t -> 'a -> 'v, 'v) order
  | New_line : ('v, 'v) order
  | Open : Pretty.box -> ('v, 'v) order
  | Close : ('v, 'v) order
  | Break : { indent : int; len : int } -> ('v, 'v) order

let keval_order : type ty v. (ppf -> v) -> ppf -> (ty, v) order -> ty =
 fun k t -> function
  | Const (encoder, v) -> k (encoder t v)
  | Atom encoder -> fun v -> k (encoder t v)
  | Param -> fun encoder v -> k (encoder t v)
  | New_line -> Pretty.kpush k Pretty.new_line t
  | Open box -> Pretty.kpush k (Pretty.o box) t
  | Close -> Pretty.kpush k Pretty.close t
  | Break { indent; len } -> Pretty.kpush k (Pretty.break ~len ~indent) t

let const encoder v = Const (encoder, v)
let atom encoder = Atom encoder
let a = Param
let ( !! ) = atom
let ( $ ) = const
let new_line = New_line
let tbox indent = Open (Pretty.tbox indent)
let bbox = Open Pretty.bbox
let box = Open Pretty.box
let close = Close
let break ~indent ~len = Break { indent; len }
let fws = Break { indent = 1; len = 1 }
let spaces len = Break { indent = 0; len }
let cut = Break { indent = 0; len = 0 }
let using : ('b -> 'a) -> 'a t -> 'b t = fun f encoder t v -> encoder t (f v)

let list ~sep:(sep, e) encoder =
  let rec go t = function
    | [] -> t
    | [ x ] -> encoder t x
    | x :: r ->
        let t = encoder t x in
        let t = sep t e in
        go t r
  in
  go

let option encoder t = function Some x -> encoder t x | None -> t

let char : char t =
 fun t x ->
  (* XXX(dinosaure): we should optimize it! *)
  let atom = Pretty.string ~breakable:false (String.make 1 x) in
  Pretty.push atom t

let string : string t =
 fun t x ->
  let atom = Pretty.string ~breakable:false x in
  Pretty.push atom t

let bytes : Bytes.t t =
 fun t x ->
  let atom = Pretty.bytes ~breakable:false x in
  Pretty.push atom t

let bigstring : Bigstringaf.t t =
 fun t x ->
  let atom = Pretty.bigstring ~breakable:false x in
  Pretty.push atom t

let breakable : string t =
 fun t x ->
  let atom = Pretty.string ~breakable:true x in
  Pretty.push atom t

type ('ty, 'v) fmt =
  | [] : ('v, 'v) fmt
  | ( :: ) : ('x, 'v) order * ('v, 'r) fmt -> ('x, 'r) fmt

let rec concat : type a b c. (a, b) fmt -> (b, c) fmt -> (a, c) fmt =
 fun l1 l2 -> match (l1, l2) with [], l -> l | h :: t, l -> h :: concat t l

let rec keval : type ty v. (ppf -> v) -> ppf -> (ty, v) fmt -> ty =
 fun k t -> function
  | [] -> k t
  | x :: r ->
      let k t = keval k t r in
      keval_order k t x

external identity : 'a -> 'a = "%identity"

let eval : type ty. ppf -> (ty, ppf) fmt -> ty =
 fun t fmt -> keval identity t fmt
