module Dict : Map.S with type key = string

type 'a dict = 'a Dict.t

type 'a t
type value

(* source *)
val value_of_string : string -> value
val value_to_string : value -> string

val value_of_yojson : Yojson.Basic.json -> value
val value_to_yojson : value -> Yojson.Basic.json

val decode : 'a t -> value -> ('a, string) Result.result
val decode_string : 'a t -> string -> ('a, string) Result.result

(* Primitives *)
val string : string t
val float : float t
val int : int t
val bool : bool t
val null : 'a -> 'a t

(* Containers *)
val list : 'a t -> 'a list t
val array : 'a t -> 'a array t
val dict : 'a t -> 'a dict t
val pairs : 'a t -> (string * 'a) list t

(* Selectors *)
val (@=) : string -> 'a t -> 'a t
val field : string -> 'a t -> 'a t
val index : int -> 'a t -> 'a t
val at : string list -> 'a t -> 'a t

(* inconsistent structure *)
val option : 'a t -> 'a option t
val one_of : 'a t list -> 'a t
val value : value t

(* combinators *)
val succeed : 'a -> 'a t
val fail : string -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val and_then : ('a -> 'b t) -> 'a t -> 'b t
val apply : ('a -> 'b) t -> 'a t -> 'b t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

(* object *)
val mapN : 'fn -> 'fn t
val (||>) : ('a -> 'b) t -> 'a t -> 'b t
