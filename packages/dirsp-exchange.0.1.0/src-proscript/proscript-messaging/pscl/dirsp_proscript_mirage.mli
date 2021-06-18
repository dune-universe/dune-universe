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
module type CUSTOM_OPTIONS = sig
  val random_bytes : int -> string -> Bytes.t
  (** [random_bytes sz id] creates [sz] random bytes with a hint [id].

      If and only if you have a unit test and need determinism, use
      the [id] to deterministically create random numbers.

      Every other implementation MUST ignore the [id] and generate a
      truly random sequence.
    *)
end

module DefaultOptions : CUSTOM_OPTIONS

(** Make a Proscript runtime that uses the mirage-crypto libraries *)
module Make () : Dirsp_proscript.S

(** Make a customized Proscript runtime that uses the mirage-crypto,
    mirage-crypto-ec and mirage-crypto-rng.lwt libraries.

    You should use {!Make} unless you are unit testing or troubleshooting.

    Since this is a highly unstable API, be sure to "include" {!DefaultOptions}
    in your custom options.
 *)
module CustomizedMake (Opts : CUSTOM_OPTIONS) : Dirsp_proscript.S
