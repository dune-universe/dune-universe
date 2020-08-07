open Module_types
open Common


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


module Make (A:ANY) =
  struct
    type a = A.t
    type key = string
    type doc = string
    type spec =
      | Unit of (a -> a)             (* option with no argument *)
      | String of (string -> a -> a) (* option with string argument *)
      | Int of (int -> a -> a)       (* option with integer argument *)
    type error =
      | Unknown_option of string
      | Missing_argument of key*spec*doc
      | Invalid_argument of key*spec*doc*string

    module M = Monad.Result (struct type t = error end)

    type anon = string -> a -> a

    let argument_type (spec:spec): string =
      match spec with
      | Unit _ -> ""
      | String _ -> " <string>"
      | Int _ -> " <int>"

    let parse
          (args:string array)
          (start: a)
          (options: (key*spec*doc) list)
          (anon: anon)
        : (a,error) result =
      let len = Array.length args in
      let rec parse (a: a) (i:int): a M.t =
        if i = len then
          M.return a
        else
          let arg = args.(i) in
          let n = String.length arg in
          if n = 0 then
            parse a (i+1)
          else if arg.[0] = '-' then
            match List.find (fun (k,_,_) -> k = arg) options with
            | None ->
               M.throw (Unknown_option arg)
            | Some (k,sp,doc) ->
               begin
                 match sp with
                 | Unit g ->
                    parse (g a) (i+1)
                 | String g ->
                    if i + 1 = len then
                      M.throw (Missing_argument (k,sp,doc))
                    else
                      parse (g args.(i+1) a) (i+2)
                 | Int g ->
                    if i + 1 = len then
                      M.throw (Missing_argument (k,sp,doc))
                    else
                      try
                        parse (g (int_of_string args.(i+1)) a) (i+2)
                      with Failure _ ->
                        M.throw (Invalid_argument (k,sp,doc,args.(i+1)))
               end
          else
            parse (anon arg a) (i+1)
      in
      parse start 1

    let string_of_error (e:error): string =
      let option_expect k s =
        "option '" ^ k ^ "' expects a " ^
          (match s with
           | Unit _ ->
              assert false
           | String _ ->
              "string"
           | Int _ ->
              "integer")
      in
      match e with
      | Unknown_option str ->
         "unknown option '" ^ str ^ "'"
      | Missing_argument (k,s,_) ->
         "missing argument; " ^ option_expect k s
      | Invalid_argument (k,s,_,arg) ->
         "invalid argument '" ^ arg ^ "'; " ^ option_expect k s
  end
