(*****************************************************************************)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Valentin Chaboche                                      *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** {1 Ppx for property-based test generation} *)

(** This library intends to facilitate the writing of property-based test. The
    main purpose is to easily test redundant properties on multiple functions,
    a level of abstraction of property-based testing libraries is offered using
    properties and generators through identifiers.

    Example:

    - Addition is commutative:

    {[
      let add = (+) [@@pbt "commutative[int, int]"]

      (* ==> *)
      include struct
        let add = (+)

        let test_add_is_commutative =
          QCheck.Test.make
            ~name:"add_is_commutative"
            (QCheck.pair QCheck.int QCheck.int)
            (fun (arb_0, arb_1) -> Pbt.Properties.commutative add arb_0 arb_1)

        let () = Runner.add_tests [ test_add_is_commutative ]
      end
    ]}
*)

(** {2 Annotation-based system} *)

(** We introduce an annotation-based system to describe properties which be
    later use to generate properties. *)

(** {3 Annotation language} *)

(**
   {[
     <properties> ::= <property list>
     <property> ::= <ident> <args> <gens>
     <args> ::= { <ident list> }
     <gens> ::= [ <ident list> ]
   ]}

   The property is splitted between 3 sub-components:
   - property name
   - arguments names (can be empty)
   - generators names (can be empty)

   Example without arguments:

   {[
     let add = (+) [@@pbt "commutative[int, int]"]
   ]}

   - property name: commutative
   - arguments names: none
   - generators names: int, int

   Example with arguments:
   {[
     let zero = 0
     let add = (+) [@@pbt "absorbs{zero}[int]"]
   ]}

   - property name: absorbs
   - arguments names: zero
   - generators names: int

*)

(** {3 Specifying generators} *)

(** Generators are created using ppx_deriving_qcheck. They are for now a
    QCheck arbitrary, but for clarity we use the word generator
    (i.e arbitrary will be removed when QCheck2 is released).

    @see <https://github.com/vch9/ppx_deriving_qcheck>

    There can be 2 ways to specify generators in annotations:

    - Specify your own generators
    If you want to provide your own generators, you must introduced a QCheck
    arbitrary named [arb_[name]].

    Example:
    {[
      (** unsigned int *)
      type uint = int

      let arb_uint = QCheck.(map abs int)

      let add = (+) [@@pbt "commutative[arb_uint, arb_uint]"]
    ]}

    - Use ppx_deriving_qcheck
    Ppx_deriving_qcheck supports multiple primitive types, but if it fails
    to create a QCheck arbitrary for a given arbitrary, a [arb_[type_name]] will
    be created

    Example:
    {[
      let add = (+) [@@pbt "commutative[uint, int]"]

      (* => *)
      include struct
        let add = (+)

        let test_add_is_commutative =
          QCheck.Test.make
            ~name:"add_is_commutative"
            (QCheck.pair QCheck.arb_uint QCheck.int) (* <- see here *) 
            (fun (arb_0, arb_1) -> Pbt.Properties.commutative add arb_0 arb_1)

        let () = Runner.add_tests [ test_add_is_commutative ]
      end
    ]}

*)

(** {3 Specifying properties} *)

(** Properties must be used with an identifier, a list of properties can be found
    at {!Pbt.Properties}.

    If it fails to find a properties inside {!Pbt.Properties}, a local function
    describing a property must be found in the local scope.

    Example:

    {[
      let arb_set = QCheck.(list int)

      (** [get_and_set f s x y] tests
          mem [s] [x] ==> mem ([f] [s] [x] [y]) [y] *)

      let get_and_set f s x y =
        not (List.mem s x) || List.mem (f s x y) y

      (** [find_and_replace s x y] replaces [x] by [y] in [s] *)
      let find_and_replace s x y = ...
        [@@pbt "get_and_set[arb_set, int, int]"]

      (** => *)

      let arb_set = QCheck.(list int)

      (** [get_and_set f s x y] tests
          mem [s] [x] ==> mem ([f] [s] [x] [y]) [y] *)

      let get_and_set f s x y =
        not (List.mem x s) || List.mem y (f s x y) 

      include struct
        (** [find_and_replace s x y] replaces [x] by [y] in [s] *)
        let find_and_replace s x y = ...
          [@@pbt "get_and_set[arb_set, int, int]"]

        let test_find_and_replace_is_get_and_set =
          QCheck.Test.make ~name:"find_and_replace_is_get_and_set"
            (QCheck.pair arb_set (QCheck.pair QCheck.int QCheck.int))
            (fun (arb_0, (arb_1, arb_2)) ->
               get_and_set find_and_replace arb_0 arb_1 arb_2)

        let () = Runner.add_tests [test_find_and_replace_is_get_and_set]
     end

    ]}
*)

(** {2 Annotations in implementation files} *)

(** Properties can be directly attached with attributes inside function implementation.
    The test will be inlined directly after the function under test.

    Example:
    {[
      let add = (+) [@@pbt "commutative[int, int]"]

      (* ==> *)
      include struct
        let add = (+)

        let test_add_is_commutative =
          QCheck.Test.make
            ~name:"add_is_commutative"
            (QCheck.pair QCheck.int QCheck.int)
            (fun (arb_0, arb_1) -> Pbt.Properties.commutative add arb_0 arb_1)

        let () = Runner.add_tests [ test_add_is_commutative ]
      end
    ]}
*)

(** {2 Annotations in interface files} *)

(** Properties can also be attached to function declaration in interface files.
    Having annotation in signature allows the possibility to infer required generators
    for a function declaration.

    ppx_deriving_qcheck is used to automatically infer generators when it's possible.
    @see <https://github.com/vch9/ppx_deriving_qcheck>

    Example:
    {[
      (* foo.mli *)
      val add : int -> int -> int [@@pbt "commutative"]

      (* foo.ml *)
      include struct
        let add = (+)

        let test_add_is_commutative =
          QCheck.Test.make
            ~name:"add_is_commutative"
            (QCheck.pair QCheck.int QCheck.int)
            (fun (arb_0, arb_1) -> Pbt.Properties.commutative add arb_0 arb_1)

        let () = Runner.add_tests [ test_add_is_commutative ]
      end
    ]}

    However, annotations are only allowed in signatures outside any structure item
    and inside only modules and functors at the moment.
*)

(** {2 Alcotest runner} *)

open Ppxlib

(**/**)

let ignore () =
  Array.exists (fun x -> Filename.check_suffix x ".expected.ml") Sys.argv

(** [get_file_name_sig sigi] returns the file name where [sigi] is located *)
let get_file_name_sig sigi =
  sigi.psig_loc.loc_start.pos_fname |> Filename.remove_extension

(** [get_file_name_str stri] returns the file name where [stri] is located *)
let get_file_name_str stri =
  stri.pstr_loc.loc_start.pos_fname |> Filename.remove_extension

(**/**)

class mapper =
  object (_self)
    inherit Ast_traverse.map as super

    method! structure str =
      if not (ignore ()) then
        let file_name = get_file_name_str @@ List.hd str in
        if Interface.interface file_name then Interface.inline_impl_tests str
        else super#structure str
      else super#structure str

    method! signature sigs =
      if not (ignore ()) then
        let file_name = get_file_name_sig @@ List.hd sigs in
        Interface.intf file_name sigs
      else super#signature sigs

    method! structure_item stri =
      if not (ignore ()) then
        Implementation.structure_item ~callback:super#structure_item stri
      else super#structure_item stri
  end

let () =
  let mapper = new mapper in
  Driver.register_transformation
    "ppx_test"
    ~intf:mapper#signature
    ~impl:mapper#structure
