# Js_of_ocaml Ppx deriver

ppx_deriving_jsoo is a js_of_ocaml ppx deriver.
From an ocaml type it provides the jsoo type interface and the conversion functions.

## Example

The ocaml type:

    type 'a x = {
      a : int64 list;
      b : 'a option;
      c : int option; [@opt]
      d : string; [@mutable]
      e : char; [@key "js_key"]
      f : int -> string; [@callback]
      g : Unsafe.any -> int -> string; [@meth_callback]
      h : unit -> int; [@meth]
      i : js_string t; [@ignore]
      j : (nativeint * string array);
      k : [ `A | `B of float ];
    } [@@deriving jsoo]

Will produce the class type:

    class type ['a] x_jsoo = object
      method  a :
        Ezjs_min.bigInt Ezjs_min.t Ezjs_min.js_array Ezjs_min.t readonly_prop
      method  b : 'a Ezjs_min.optdef readonly_prop
      method  c : int Ezjs_min.opt readonly_prop
      method  d : Ezjs_min.js_string Ezjs_min.t prop
      method  js_key_ : Ezjs_min.js_string Ezjs_min.t readonly_prop
      method  f : (int -> Ezjs_min.js_string Ezjs_min.t) callback readonly_prop
      method  g :
        (Ezjs_min.Unsafe.any, int -> Ezjs_min.js_string Ezjs_min.t)
          meth_callback readonly_prop
      method  h : int Ezjs_min.meth
      method  i : js_string t readonly_prop
      method  j : 'a _j_tupjsoo Ezjs_min.t readonly_prop
      method  k : 'a _k_tupjsoo Ezjs_min.t readonly_prop
    end

    and ['a] _j_tupjsoo = object
      method  _0 : Ezjs_min.bigInt Ezjs_min.t Ezjs_min.readonly_prop
      method  _1 :
        Ezjs_min.js_string Ezjs_min.t Ezjs_min.js_array Ezjs_min.t
          Ezjs_min.readonly_prop
    end
    and ['a] _k_tupjsoo = object
      method  _A : unit Ezjs_min.optdef Ezjs_min.readonly_prop
      method  _B : Ezjs_min.number Ezjs_min.t Ezjs_min.optdef Ezjs_min.readonly_prop
    end

And the conversion functions:

    val x_to_jsoo :
      (('a -> 'a_jsoo) * ('a_jsoo -> 'a)) -> 'a x -> 'a_jsoo x_jsoo Ezjs_min.t)

    val x_of_jsoo :
      (('a -> 'a_jsoo) * ('a_jsoo -> 'a)) -> 'a_jsoo x_jsoo Ezjs_min.t -> 'a x)

    val x_jsoo_conv :
      (('a -> 'a_jsoo) * ('a_jsoo -> 'a)) -> 'a x -> 'a_jsoo x_jsoo Ezjs_min.t) *
      (('a -> 'a_jsoo) * ('a_jsoo -> 'a)) -> 'a_jsoo x_jsoo Ezjs_min.t -> 'a x)

Where `Ezjs_min` comes from ezjs_min library whose module includes `Js_of_ocaml.Js` and some other userful functions.

There is also a deriver for only interface:

    [@@deriving jsoo_type]

And for only conversion functions:

    [@@deriving jsoo_conv]

Most of regular types are handled.
GADT and variant inheritance are not handled yet.
Also some complicated recursive types can fail the ppx since the class types impose some constraints.
