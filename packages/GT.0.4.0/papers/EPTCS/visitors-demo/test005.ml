
module Lib : sig
  type foo
  class virtual ['c] map_foo :
    object ('c)
      constraint 'c =
        < visit_Foo : 'd -> string -> int -> foo;
          visit_foo : 'd -> foo -> foo; .. >
      method visit_Foo : 'd -> string -> int -> foo
      method private visit_array :
        'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
      method private visit_bool : 'env. 'env -> bool -> bool
      method private visit_bytes : 'env. 'env -> bytes -> bytes
      method private visit_char : 'env. 'env -> char -> char
      method private visit_float : 'env. 'env -> float -> float
      method visit_foo : 'd -> foo -> foo
      method private visit_int : 'env. 'env -> int -> int
      method private visit_int32 : 'env. 'env -> int32 -> int32
      method private visit_int64 : 'env. 'env -> int64 -> int64
      method private visit_lazy_t :
        'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
      method private visit_list :
        'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
      method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
      method private visit_option :
        'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
      method private visit_ref :
        'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
      method private visit_result :
        'env 'a 'b 'e 'f.
          ('env -> 'a -> 'b) ->
          ('env -> 'e -> 'f) ->
          'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
      method private visit_string : 'env. 'env -> string -> string
      method private visit_unit : 'env. 'env -> unit -> unit
    end

  type length = L of foo
  class virtual ['c] map_length :
    object ('c)
      constraint 'c =
        < visit_L : 'd -> foo -> length;
          visit_length : 'd -> length -> length; .. >
      method visit_L : 'd -> foo -> length
      method private visit_array :
        'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
      method private visit_bool : 'env. 'env -> bool -> bool
      method private visit_bytes : 'env. 'env -> bytes -> bytes
      method private visit_char : 'env. 'env -> char -> char
      method private visit_float : 'env. 'env -> float -> float
      method private virtual visit_foo : 'd -> foo -> foo
      method private visit_int : 'env. 'env -> int -> int
      method private visit_int32 : 'env. 'env -> int32 -> int32
      method private visit_int64 : 'env. 'env -> int64 -> int64
      method private visit_lazy_t :
        'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
      method visit_length : 'd -> length -> length
      method private visit_list :
        'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
      method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
      method private visit_option :
        'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
      method private visit_ref :
        'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
      method private visit_result :
        'env 'a 'b 'e 'f.
          ('env -> 'a -> 'b) ->
          ('env -> 'e -> 'f) ->
          'env -> ('a, 'e) Result.result -> ('b, 'f) Result.result
      method private visit_string : 'env. 'env -> string -> string
      method private visit_unit : 'env. 'env -> unit -> unit
    end

  (* constructs a value with a sting inside which is a text representation of int 
     invariant: { L(Foo (s, n)) | s = string_of_int n }
  *)
  val make_length : int -> length
  val show_length : length -> string
end = struct 
  type foo = Foo of string * int
  [@@deriving visitors { variety = "map"; name="map_foo"; polymorphic = false }]

  type length = L of foo 
  [@@deriving visitors { variety = "map"; name="map_length"; polymorphic = false }]

  let make_length n = L (Foo (string_of_int n, n))
  let show_length (L (Foo (s,n))) = 
    Printf.sprintf "L (Foo (%S,%d))" s n
end

let r1 = 
  let o =
    object(self: 'self)
      inherit [_] Lib.map_length
      inherit [_] Lib.map_foo

      (* this override makes it possible to break invariant *)
      method visit_int : 'env. 'env -> int -> int = fun _ n -> (-n)

    end
  in 
  o#visit_length () (Lib.make_length 10)

let () = print_endline @@ Lib.show_length r1
