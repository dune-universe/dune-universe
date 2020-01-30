(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, Inc. <contact@nomadic-labs.com>          *)
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

(* NOTE: the current release of Crowbar, v0.1, is quite limited. Several
 * improvements have been made to the dev version which will make it possible to
 * simplify this file and increase coverage.
 * For now, this is a limited test-suite. *)

let char = Crowbar.map [Crowbar.uint8] Char.chr

let string = Crowbar.bytes

(* The v0.1 of Crowbar doesn't have fixed-size string generation. When we
 * update Crowbar, we can improve this generator. *)
let short_string =
  let open Crowbar in
  choose
    [
      const "";
      map [char] (fun c -> String.make 1 c);
      map [char; char; char; char] (fun c1 c2 c3 c4 ->
          let s = Bytes.make 4 c1 in
          Bytes.set s 1 c2 ;
          Bytes.set s 2 c3 ;
          Bytes.set s 3 c4 ;
          Bytes.to_string s);
    ]

let short_string1 =
  let open Crowbar in
  choose
    [
      map [char] (fun c -> String.make 1 c);
      map [char; char; char; char] (fun c1 c2 c3 c4 ->
          let s = Bytes.make 4 c1 in
          Bytes.set s 1 c2 ;
          Bytes.set s 2 c3 ;
          Bytes.set s 3 c4 ;
          Bytes.to_string s);
    ]

let mbytes = Crowbar.map [Crowbar.bytes] Bytes.of_string

let short_mbytes = Crowbar.map [short_string] Bytes.of_string

let short_mbytes1 = Crowbar.map [short_string1] Bytes.of_string

(* We need to hide the type parameter of `Encoding.t` to avoid the generator
 * combinator `choose` from complaining about different types. We use first
 * level modules (for now) to encode existentials.
 *
 * An alternative is used in https://gitlab.com/gasche/fuzz-data-encoding *)

module type TESTABLE = sig
  type t

  val v : t

  val ding : t Json_encoding.encoding

  val pp : t Crowbar.printer
end

type testable = (module TESTABLE)

let null : testable =
  ( module struct
    type t = unit

    let v = ()

    let ding = Json_encoding.null

    let pp ppf () = Crowbar.pp ppf "(null)"
  end )

let empty : testable =
  ( module struct
    type t = unit

    let v = ()

    let ding = Json_encoding.empty

    let pp ppf () = Crowbar.pp ppf "(empty)"
  end )

let unit : testable =
  ( module struct
    type t = unit

    let v = ()

    let ding = Json_encoding.unit

    let pp ppf () = Crowbar.pp ppf "(unit)"
  end )

let map_constant (s : string) : testable =
  ( module struct
    type t = unit

    let v = ()

    let ding = Json_encoding.constant s

    let pp ppf () = Crowbar.pp ppf "\"%s\"" s
  end )

let map_int32 (i : int32) : testable =
  ( module struct
    type t = int32

    let v = i

    let ding = Json_encoding.int32

    let pp = Crowbar.pp_int32
  end )

let lower_bound_53 = Int64.(neg @@ shift_left 1L 53)
let upper_bound_53 = Int64.shift_left 1L 53
let map_int53 (i : int64) : testable =
   let clipped = max lower_bound_53 (min i upper_bound_53) in
  ( module struct
    type t = int64

    let v = clipped

    let ding = Json_encoding.int53

    let pp = Crowbar.pp_int64
  end )

let map_range_int a b c : testable =
  let (small, middle, big) =
    match List.sort compare [a; b; c] with
    | [small; middle; big] ->
        assert (small <= middle) ;
        assert (middle <= big) ;
        (small, middle, big)
    | _ ->
        assert false
  in
  ( module struct
    type t = int

    let v = middle

    let name = Format.asprintf "ranged(%d-%d-%d)" small middle big

    let ding = Json_encoding.ranged_int ~minimum:small ~maximum:big name

    let pp ppf i = Crowbar.pp ppf "(%d :[%d;%d])" i small big
  end )

let map_range_float a b c : testable =
  if compare a nan = 0 || compare b nan = 0 || compare c nan = 0 then
    (* copout *)
    null
  else
    let (small, middle, big) =
      match List.sort compare [a; b; c] with
      | [small; middle; big] ->
          assert (small <= middle) ;
          assert (middle <= big) ;
          (small, middle, big)
      | _ ->
          assert false
    in
    ( module struct
      type t = float

      let v = middle

      let name = Format.asprintf "ranged(%f-%f-%f)" small middle big

      let ding = Json_encoding.ranged_float ~minimum:small ~maximum:big name

      let pp ppf i = Crowbar.pp ppf "(%f :[%f;%f])" i small big
    end )

let map_bool b : testable =
  ( module struct
    type t = bool

    let v = b

    let ding = Json_encoding.bool

    let pp = Crowbar.pp_bool
  end )

let map_string s : testable =
  ( module struct
    type t = string

    let v = s

    let ding = Json_encoding.string

    let pp = Crowbar.pp_string
  end )

let map_bytes s : testable =
  ( module struct
    type t = Bytes.t

    let v = s

    let ding = Json_encoding.bytes

    let pp fmt b = Crowbar.pp_string fmt (Bytes.to_string b)
  end )

let map_float f : testable =
  ( module struct
    type t = float

    let v = f

    let ding = Json_encoding.float

    let pp = Crowbar.pp_float
  end )

(* And now combinators *)

let map_some (t : testable) : testable =
  let module T = (val t) in
  ( module struct
    type t = T.t option

    let v = Some T.v

    let ding =
      try Json_encoding.option T.ding
      with Invalid_argument _ -> Crowbar.bad_test ()

    let pp ppf o =
      Crowbar.pp
        ppf
        "@[<hv 1>%a@]"
        (fun fmt v ->
          match v with
          | None ->
              Format.fprintf fmt "None"
          | Some v ->
              Format.fprintf fmt "Some(%a)" T.pp v)
        o
  end )

let map_none (t : testable) : testable =
  let module T = (val t) in
  ( module struct
    type t = T.t option

    let v = None

    let ding =
      try Json_encoding.option T.ding
      with Invalid_argument _ -> Crowbar.bad_test ()

    let pp ppf o =
      Crowbar.pp
        ppf
        "@[<hv 1>%a@]"
        (fun fmt v ->
          match v with
          | None ->
              Format.fprintf fmt "None"
          | Some v ->
              Format.fprintf fmt "Some(%a)" T.pp v)
        o
  end )

let map_list (t : testable) (ts : testable list) : testable =
  let module T = (val t) in
  ( module struct
    type t = T.t list

    let ding = Json_encoding.list T.ding

    let v =
      List.fold_left
        (fun acc (t : testable) ->
          let module T = (val t) in
          (* We can get rid of this Obj when we update Crowbar *)
          Obj.magic T.v :: acc)
        []
        ts

    let pp = Crowbar.pp_list T.pp
  end )

let map_array (t : testable) (ts : testable array) : testable =
  let module T = (val t) in
  ( module struct
    type t = T.t array

    let ding = Json_encoding.array T.ding

    let v =
      Array.of_list
        (Array.fold_left
           (fun acc (t : testable) ->
             let module T = (val t) in
             Obj.magic T.v :: acc)
           []
           ts)

    let pp ppf a =
      if Array.length a > 40 then
        Crowbar.pp
          ppf
          "@[<hv 1>[|%a … (%d more elements)|]@]"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
             T.pp)
          (Array.to_list (Array.sub a 0 30))
          (Array.length a)
      else
        Crowbar.pp
          ppf
          "@[<hv 1>[|%a|]@]"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
             T.pp)
          (Array.to_list a)
  end )

let map_tup1 (t1 : testable) : testable =
  let module T1 = (val t1) in
  ( module struct
    include T1

    let ding = Json_encoding.tup1 T1.ding

    let pp ppf v1 = Crowbar.pp ppf "@[<hv 1>(%a)@]" T1.pp v1
  end )

let map_tup2 (t1 : testable) (t2 : testable) : testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  ( module struct
    type t = T1.t * T2.t

    let ding = Json_encoding.tup2 T1.ding T2.ding

    let v = (T1.v, T2.v)

    let pp ppf (v1, v2) = Crowbar.pp ppf "@[<hv 1>(%a, %a)@]" T1.pp v1 T2.pp v2
  end )

let map_tup3 (t1 : testable) (t2 : testable) (t3 : testable) : testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  let module T3 = (val t3) in
  ( module struct
    type t = T1.t * T2.t * T3.t

    let ding =
      Json_encoding.tup3 T1.ding T2.ding T3.ding

    let v = (T1.v, T2.v, T3.v)

    let pp ppf (v1, v2, v3) =
      Crowbar.pp ppf "@[<hv 1>(%a, %a, %a)@]" T1.pp v1 T2.pp v2 T3.pp v3
  end )

let map_tup4 (t1 : testable) (t2 : testable) (t3 : testable) (t4 : testable) :
    testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  let module T3 = (val t3) in
  let module T4 = (val t4) in
  ( module struct
    type t = T1.t * T2.t * T3.t * T4.t

    let ding =
      Json_encoding.tup4
        T1.ding
        T2.ding
        T3.ding
        T4.ding

    let v = (T1.v, T2.v, T3.v, T4.v)

    let pp ppf (v1, v2, v3, v4) =
      Crowbar.pp
        ppf
        "@[<hv 1>(%a, %a, %a, %a)@]"
        T1.pp
        v1
        T2.pp
        v2
        T3.pp
        v3
        T4.pp
        v4
  end )

let map_tup5 (t1 : testable) (t2 : testable) (t3 : testable) (t4 : testable)
    (t5 : testable) : testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  let module T3 = (val t3) in
  let module T4 = (val t4) in
  let module T5 = (val t5) in
  ( module struct
    type t = T1.t * T2.t * T3.t * T4.t * T5.t

    let ding =
      Json_encoding.tup5
        T1.ding
        T2.ding
        T3.ding
        T4.ding
        T5.ding

    let v = (T1.v, T2.v, T3.v, T4.v, T5.v)

    let pp ppf (v1, v2, v3, v4, v5) =
      Crowbar.pp
        ppf
        "@[<hv 1>(%a, %a, %a, %a, %a)@]"
        T1.pp
        v1
        T2.pp
        v2
        T3.pp
        v3
        T4.pp
        v4
        T5.pp
        v5
  end )

let map_tup6 (t1 : testable) (t2 : testable) (t3 : testable) (t4 : testable)
    (t5 : testable) (t6 : testable) : testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  let module T3 = (val t3) in
  let module T4 = (val t4) in
  let module T5 = (val t5) in
  let module T6 = (val t6) in
  ( module struct
    type t = T1.t * T2.t * T3.t * T4.t * T5.t * T6.t

    let ding =
      Json_encoding.tup6
        T1.ding
        T2.ding
        T3.ding
        T4.ding
        T5.ding
        T6.ding

    let v = (T1.v, T2.v, T3.v, T4.v, T5.v, T6.v)

    let pp ppf (v1, v2, v3, v4, v5, v6) =
      Crowbar.pp
        ppf
        "@[<hv 1>(%a, %a, %a, %a, %a, %a)@]"
        T1.pp
        v1
        T2.pp
        v2
        T3.pp
        v3
        T4.pp
        v4
        T5.pp
        v5
        T6.pp
        v6
  end )

let map_tup7 (t1 : testable) (t2 : testable) (t3 : testable) (t4 : testable)
    (t5 : testable) (t6 : testable) (t7 : testable) : testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  let module T3 = (val t3) in
  let module T4 = (val t4) in
  let module T5 = (val t5) in
  let module T6 = (val t6) in
  let module T7 = (val t7) in
  ( module struct
    type t = T1.t * T2.t * T3.t * T4.t * T5.t * T6.t * T7.t

    let ding =
      Json_encoding.tup7
        T1.ding
        T2.ding
        T3.ding
        T4.ding
        T5.ding
        T6.ding
        T7.ding

    let v = (T1.v, T2.v, T3.v, T4.v, T5.v, T6.v, T7.v)

    let pp ppf (v1, v2, v3, v4, v5, v6, v7) =
      Crowbar.pp
        ppf
        "@[<hv 1>(%a, %a, %a, %a, %a, %a, %a)@]"
        T1.pp
        v1
        T2.pp
        v2
        T3.pp
        v3
        T4.pp
        v4
        T5.pp
        v5
        T6.pp
        v6
        T7.pp
        v7
  end )

let map_tup8 (t1 : testable) (t2 : testable) (t3 : testable) (t4 : testable)
    (t5 : testable) (t6 : testable) (t7 : testable) (t8 : testable) : testable
    =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  let module T3 = (val t3) in
  let module T4 = (val t4) in
  let module T5 = (val t5) in
  let module T6 = (val t6) in
  let module T7 = (val t7) in
  let module T8 = (val t8) in
  ( module struct
    type t = T1.t * T2.t * T3.t * T4.t * T5.t * T6.t * T7.t * T8.t

    let ding =
      Json_encoding.tup8
        T1.ding
        T2.ding
        T3.ding
        T4.ding
        T5.ding
        T6.ding
        T7.ding
        T8.ding

    let v = (T1.v, T2.v, T3.v, T4.v, T5.v, T6.v, T7.v, T8.v)

    let pp ppf (v1, v2, v3, v4, v5, v6, v7, v8) =
      Crowbar.pp
        ppf
        "@[<hv 1>(%a, %a, %a, %a, %a, %a, %a, %a)@]"
        T1.pp
        v1
        T2.pp
        v2
        T3.pp
        v3
        T4.pp
        v4
        T5.pp
        v5
        T6.pp
        v6
        T7.pp
        v7
        T8.pp
        v8
  end )

let map_tup9 (t1 : testable) (t2 : testable) (t3 : testable) (t4 : testable)
    (t5 : testable) (t6 : testable) (t7 : testable) (t8 : testable)
    (t9 : testable) : testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  let module T3 = (val t3) in
  let module T4 = (val t4) in
  let module T5 = (val t5) in
  let module T6 = (val t6) in
  let module T7 = (val t7) in
  let module T8 = (val t8) in
  let module T9 = (val t9) in
  ( module struct
    type t = T1.t * T2.t * T3.t * T4.t * T5.t * T6.t * T7.t * T8.t * T9.t

    let ding =
      Json_encoding.tup9
        T1.ding
        T2.ding
        T3.ding
        T4.ding
        T5.ding
        T6.ding
        T7.ding
        T8.ding
        T9.ding

    let v = (T1.v, T2.v, T3.v, T4.v, T5.v, T6.v, T7.v, T8.v, T9.v)

    let pp ppf (v1, v2, v3, v4, v5, v6, v7, v8, v9) =
      Crowbar.pp
        ppf
        "@[<hv 1>(%a, %a, %a, %a, %a, %a, %a, %a, %a)@]"
        T1.pp
        v1
        T2.pp
        v2
        T3.pp
        v3
        T4.pp
        v4
        T5.pp
        v5
        T6.pp
        v6
        T7.pp
        v7
        T8.pp
        v8
        T9.pp
        v9
  end )

let map_tup10 (t1 : testable) (t2 : testable) (t3 : testable) (t4 : testable)
    (t5 : testable) (t6 : testable) (t7 : testable) (t8 : testable)
    (t9 : testable) (t10 : testable) : testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  let module T3 = (val t3) in
  let module T4 = (val t4) in
  let module T5 = (val t5) in
  let module T6 = (val t6) in
  let module T7 = (val t7) in
  let module T8 = (val t8) in
  let module T9 = (val t9) in
  let module T10 = (val t10) in
  ( module struct
    type t =
      T1.t * T2.t * T3.t * T4.t * T5.t * T6.t * T7.t * T8.t * T9.t * T10.t

    let ding =
      Json_encoding.tup10
        T1.ding
        T2.ding
        T3.ding
        T4.ding
        T5.ding
        T6.ding
        T7.ding
        T8.ding
        T9.ding
        T10.ding

    let v = (T1.v, T2.v, T3.v, T4.v, T5.v, T6.v, T7.v, T8.v, T9.v, T10.v)

    let pp ppf (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) =
      Crowbar.pp
        ppf
        "@[<hv 1>(%a, %a, %a, %a, %a, %a, %a, %a, %a, %a)@]"
        T1.pp
        v1
        T2.pp
        v2
        T3.pp
        v3
        T4.pp
        v4
        T5.pp
        v5
        T6.pp
        v6
        T7.pp
        v7
        T8.pp
        v8
        T9.pp
        v9
        T10.pp
        v10
  end )

let map_merge_tups (t1 : testable) (t2 : testable) : testable =
  let module T1 = (val t1) in
  let module T2 = (val t2) in
  ( module struct
    type t = T1.t * T2.t

    let ding =
      Json_encoding.merge_tups T1.ding T2.ding

    let v = (T1.v, T2.v)

    let pp ppf (v1, v2) = Crowbar.pp ppf "@[<hv 1>(%a, %a)@]" T1.pp v1 T2.pp v2
  end )

let testable_printer : testable Crowbar.printer =
 fun ppf (t : testable) ->
  let module T = (val t) in
  T.pp ppf T.v

(* helpers to construct values tester values *)

(* Generator for testable values *)

let tup_gen (tgen : testable Crowbar.gen) : testable Crowbar.gen =
  let open Crowbar in
  (* Stack overflow if there are more levels *)
  with_printer testable_printer
  @@ choose
       [
         map [tgen] map_tup1;
         map [tgen; tgen] map_tup2;
         map [tgen; tgen; tgen] map_tup3;
         map [tgen; tgen; tgen; tgen] map_tup4;
         map [tgen; tgen; tgen; tgen; tgen] map_tup5;
         map [tgen; tgen; tgen; tgen; tgen; tgen] map_tup6;
       ]

let gen =
  let open Crowbar in
  let g : testable Crowbar.gen =
    fix (fun g ->
        choose
          [
            const null;
            const empty;
            const unit;
            map [short_string] map_constant;
            map [int32] map_int32;
            map [int64] map_int53;
            map [float; float; float] map_range_float;
            map [bool] map_bool;
            map [short_string] map_string;
            map [short_mbytes] map_bytes;
            map [float] map_float;
            map [short_string] map_string;
            map [short_mbytes] map_bytes;
            map [g] map_some;
            map [g] map_none;
            map [g] map_tup1;
            map [g; g] map_tup2;
            map [g; g; g] map_tup3;
            map [g; g; g; g] map_tup4;
            map [g; g; g; g; g] map_tup5;
            map [g; g; g; g; g; g] map_tup6;
            map [g; g] (fun t1 t2 ->
                map_merge_tups (map_tup1 t1) (map_tup1 t2));
            map [g; g; g] (fun t1 t2 t3 ->
                map_merge_tups (map_tup2 t1 t2) (map_tup1 t3));
            map [g; g; g] (fun t1 t2 t3 ->
                map_merge_tups (map_tup1 t1) (map_tup2 t2 t3));
              (* NOTE: we cannot use lists/arrays for now. They require the
           data-inside to be homogeneous (e.g., same rangedness of ranged
           numbers) which we cannot guarantee right now. This can be fixed once
           we update Crowbar and get access to the new `dynamic_bind` generator
           combinator.

           map [g; list g] map_variable_list;
           map [g; list g] (fun t ts -> map_variable_array t (Array.of_list ts));
        *)

          ])
  in
  with_printer testable_printer g

module Ezjsonm_construct = Json_encoding.Make(Json_repr.Ezjsonm)
module Yojson_construct = Json_encoding.Make(Json_repr.Yojson)

(* Basic functions for executing tests on a given input *)
let roundtrip (module M: Json_encoding.S) name pp ding v =
  let json =
    try M.construct ding v
    with Invalid_argument m ->
      Crowbar.fail (Format.asprintf "Cannot construct (%s): %a (%s)" name pp v m)
  in
  let vv =
    try M.destruct ding json
    with Invalid_argument s ->
       Format.kasprintf Crowbar.fail "Cannot destruct (%s): %s" name s
  in
  Crowbar.check_eq ~pp v vv

(* Setting up the actual tests *)
let test_testable_ezjsonm (testable : testable) =
  let module T = (val testable) in
  roundtrip (module Ezjsonm_construct) "ez" T.pp T.ding T.v

let test_testable_yojson (testable : testable) =
  let module T = (val testable) in
  roundtrip (module Yojson_construct) "yo" T.pp T.ding T.v

let () =
  Crowbar.add_test ~name:"ezjsonm roundtrips" [gen] test_testable_ezjsonm ;
  Crowbar.add_test ~name:"yojson roundtrips" [gen] test_testable_yojson ;
  ()
