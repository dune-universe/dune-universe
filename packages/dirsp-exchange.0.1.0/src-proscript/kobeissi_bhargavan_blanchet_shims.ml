(* Copyright 2021 Diskuv, Inc.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. *)
include Kobeissi_bhargavan_blanchet_intf

module Make (ProScript : Dirsp_proscript.S) = struct
  (* Shims *)
  (* ----- *)

  let shim_Type_key_construct () =
    let b = ProScript.elem_of_char in
    ProScript.of_elem_list
      [
        b '\x00'; b '\x00'; b '\x00'; b '\x00';
        b '\x00'; b '\x00'; b '\x00'; b '\x00';
        b '\x00'; b '\x00'; b '\x00'; b '\x00';
        b '\x00'; b '\x00'; b '\x00'; b '\x00';
        b '\x00'; b '\x00'; b '\x00'; b '\x00';
        b '\x00'; b '\x00'; b '\x00'; b '\x00';
        b '\x00'; b '\x00'; b '\x00'; b '\x00';
        b '\x00'; b '\x00'; b '\x00'; b '\x00'
      ]
      [@@ocamlformat "disable"]

  let shim_Type_iv_construct () =
    let b = ProScript.elem_of_char in
    ProScript.of_elem_list
      [
        b '\x00'; b '\x00'; b '\x00'; b '\x00';
        b '\x00'; b '\x00'; b '\x00'; b '\x00';
        b '\x00'; b '\x00'; b '\x00'; b '\x00'
      ]
      [@@ocamlformat "disable"]

  let shim_UTIL_newKeyPair id =
    let priv =
      ProScript.Crypto.random32Bytes
        (ProScript.concat [ ProScript.of_string "aPK"; id ])
    in
    let b = ProScript.elem_of_char in
    let (result: ProScript.t record_keypair) =
    {
      priv;
      pub =
        ProScript.Crypto.xDH25519 priv
          (ProScript.of_elem_list
             [
               b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00';
               b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00';
               b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00';
               b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x09'
             ]);
    }
    in result
    [@@ocamlformat "disable"]

  let shim_UTIL_getDHPublicKey priv =
    let b = ProScript.elem_of_char in
    ProScript.Crypto.xDH25519 priv
      (ProScript.of_elem_list
         [
          b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00';
          b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00';
          b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00';
          b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x00'; b '\x09'
         ]) [@@ocamlformat "disable"]
end
