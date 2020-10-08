(*
 * Generic transformers: plugins.
 * Copyright (C) 2016-2019
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(** {i Foldr} plugin: fold all values in a type.

    Essentially is a stub that chains inherited attribute thorough all values
    in the value

    For type declaration [type ('a,'b,...) typ = ...] it will create a transformation
    function with type

    [('s -> 'a -> 's) ->
     ('s -> 'b -> 's) ->
     ... ->
     's -> ('a,'b,...) typ -> 's ]
*)

open Base
open Ppxlib
open GTCommon

let trait_name = "foldr"

module Make(AstHelpers : GTHELPERS_sig.S) = struct
open AstHelpers
module Foldl = Foldl.Make(AstHelpers)

let trait_name =  trait_name

class g initial_args tdecls = object(self: 'self)
  inherit Foldl.g initial_args tdecls

  method trait_name = trait_name

  method join_args ~loc do_typ ~init (xs: (string * core_type) list) =
    List.fold_left ~f:(fun acc (name,typ) ->
        Exp.app_list ~loc
          (do_typ typ)
          [ acc; Exp.sprintf ~loc "%s" name]
        )
        ~init
        (List.rev xs)

end

let create = (new g :> Foldl.P.plugin_constructor)

end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.MAKE)

let () = register ()
