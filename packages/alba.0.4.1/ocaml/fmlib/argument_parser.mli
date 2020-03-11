open Module_types


module type S =
  sig
    type a (* The result of the argument parser *)

    type key = string (* option key, must start with '-' *)

    type doc = string (* option description *)

    type spec (* specification of an option *)
      =
      | Unit of (a -> a)             (* option with no argument *)
      | String of (string -> a -> a) (* option with string argument *)
      | Int of (int -> a -> a)       (* option with integer argument *)

    type anon = string -> a -> a (* function taking an anonymus argument into
                                    the result *)

    type error =
      | Unknown_option of string
      | Missing_argument of key*spec*doc
      | Invalid_argument of key*spec*doc*string

    val parse: string array ->
               a ->
               (key*spec*doc) list ->
               anon ->
               (a,error) result

    val string_of_error: error -> string

    val argument_type: spec -> string
  end


module Make (A:ANY): S with type a = A.t
