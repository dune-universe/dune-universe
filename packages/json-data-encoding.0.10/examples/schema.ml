(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let e =
  let open Json_encoding in
  obj2 (req "left" string) (req "right" (option int32))

let s = Json_encoding.schema e

let () = Format.printf "string * int32 option:\n%a\n\n%!" Json_schema.pp s

let e =
  let open Json_encoding in
  obj4
    (req
       ~description:"simple int compatible with OCaml's 31-bit integers"
       "int"
       int)
    (req ~description:"32-bit integer" "int32" int32)
    (req
       ~description:"biggest int that can reliably fit in floats"
       "int53"
       int53)
    (req ~description:"float" "float" float)

let s = Json_encoding.schema e

let () = Format.printf "All numeric:\n%a\n\n%!" Json_schema.pp s

let e =
  let open Json_encoding in
  obj3
    (req "year" (ranged_int ~minimum:0 ~maximum:9999 "year"))
    (req "month" (string_enum [("jan", 1); ("feb", 2); ("mar", 3)]))
    (req "day" (ranged_int ~minimum:1 ~maximum:31 "day"))

let s = Json_encoding.schema e

let () = Format.printf "Basic YMD date:\n%a\n\n%!" Json_schema.pp s

let e =
  let open Json_encoding in
  merge_objs
    (obj1 (req "name" string))
    (union
       [
         case
           ~title:"rgb"
           (obj3 (req "r" int) (req "g" int) (req "b" int))
           (function `RGB rgb -> Some rgb | `CMYK _ -> None)
           (fun rgb -> `RGB rgb);
         case
           ~title:"cmyk"
           (obj4 (req "c" int) (req "m" int) (req "y" int) (req "k" int))
           (function `RGB _ -> None | `CMYK cmyk -> Some cmyk)
           (fun cmyk -> `CMYK cmyk);
       ])

let s = Json_encoding.schema e

let () = Format.printf "colors (RGB/CMYK):\n%a\n\n%!" Json_schema.pp s

let e =
  let open Json_encoding in
  mu "slist" @@ fun self ->
  union
    [
      case
        ~title:"nil"
        null
        (function [] -> Some () | _ :: _ -> None)
        (fun () -> []);
      case
        ~title:"cons"
        (obj2 (req "head" string) (req "tail" self))
        (function [] -> None | head :: tail -> Some (head, tail))
        (fun (head, tail) -> head :: tail);
    ]

let s = Json_encoding.schema e

let () =
  Format.printf "string list as a custom recursion:\n%a\n\n%!" Json_schema.pp s
