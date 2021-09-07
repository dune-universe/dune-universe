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

module Spec = Spec

module Test = struct
  type ('a, 'b, 'c) cell = {
    path : string;
    name : string;
    spec : ('a -> 'b, 'c) Spec.t;
    f : 'a -> 'b;
    count : int;
    rand : Random.State.t option;
  }

  type t = Test : ('a, 'b, 'c) cell -> t

  let make ?(count = 10) ?rand ~path ~spec ~name f =
    Test { path; spec; f; count; name; rand }
end

module Snapshot = struct
  open Test
  module M = Memory

  let rec decode_applications :
      type a b.
      (a, b) Spec.t -> (a, b) Interpreter.args * string -> string * string =
   fun spec (args, res) ->
    let open Interpreter in
    let open Spec in
    match (args, spec) with
    | (Cons (x, xs), Arrow ({ printer; _ }, ys)) ->
        let s = printer x in
        let (x, y) = decode_applications ys (xs, res) in
        ("  " ^ s ^ x, y)
    | (Nil, Result printer) ->
        let x = M.Encode.from_string res in
        ("", printer x)
    | _ -> assert false

  let show spec snapshot =
    let name = M.Snapshot.name snapshot in
    let applications =
      M.Snapshot.applications snapshot
      |> List.map (fun (args, res) ->
             decode_applications spec (M.Encode.from_string args, res))
    in
    List.fold_left
      (fun acc (args, res) -> Printf.sprintf "%s%s  =>  %s\n%s" name args res acc)
      ""
      applications

  let make ?rand (Test { spec; f; count; name; rand = rand'; _ }) =
    let rand = match (rand, rand') with (None, x) -> x | (x, _) -> x in
    let spec_to_args =
      Option.fold
        ~none:Interpreter.spec_to_args
        ~some:(fun rand -> Interpreter.Internal_for_tests.spec_to_args rand)
        (* For testing only *)
        rand
    in
    let applications =
      List.init count (fun _ ->
          let args = spec_to_args spec in
          let res = Interpreter.(args_to_expr (Fun f) args |> interpret) in
          (M.Encode.to_string args [], M.Encode.to_string res []))
    in
    M.Snapshot.build name applications

  let next test snapshot =
    let Test.(Test { f; name; _ }) = test in
    match snapshot with
    | None -> make test
    | Some prev ->
        let decoded_applications =
          M.Snapshot.applications prev
          |> List.map (fun (x, y) ->
                 (M.Encode.from_string x, M.Encode.from_string y))
        in
        let new_applications =
          List.map
            (fun (args, _) ->
              let res = Interpreter.(args_to_expr (Fun f) args |> interpret) in
              (M.Encode.to_string args [], M.Encode.to_string res []))
            decoded_applications
        in
        M.Snapshot.build name new_applications
end

module Runner = struct
  type mode = Interactive | Promote | Error

  type res =
    [ `Passed of string
    | `Promoted of string
    | `Ignored of string
    | `Error of string * string ]
    list

  let get_passed xs = List.filter (function `Passed _ -> true | _ -> false) xs

  let get_promoted xs =
    List.filter (function `Promoted _ -> true | _ -> false) xs

  let get_ignored xs =
    List.filter (function `Ignored _ -> true | _ -> false) xs

  let get_errors xs = List.filter (function `Error _ -> true | _ -> false) xs

  let print_error = function
    | `Error (t, msg) -> Printf.printf "Error in test %s:\n%s\n" t msg
    | _ -> assert false

  let print_res xs =
    let passed = get_passed xs in
    let promoted = get_promoted xs in
    let ignored = get_ignored xs in
    let errors = get_errors xs in

    let () = match errors with x :: _ -> print_error x | _ -> () in

    Printf.printf
      {|Recap:
      %d passed
      %d promoted
      %d ignored
      %d fails
|}
      (List.length passed)
      (List.length promoted)
      (List.length ignored)
      (List.length errors)

  let input_msg () = Printf.printf "Do you want to promote these diff? [Y\\n]"

  let rec take_input () =
    match read_line () with
    | "Y" | "" -> true
    | "n" -> false
    | _ ->
        input_msg () ;
        take_input ()

  let interactive diff name path snapshot =
    match diff with
    | Diff.Same -> `Passed name
    | _ ->
        let msg =
          match diff with
          | Diff.(New s) -> s
          | Diff.(Diff s) -> s
          | _ -> assert false
        in
        let () = Printf.printf "%s" msg in
        let () = input_msg () in
        if take_input () then
          let () = Memory.Snapshot.write path snapshot in
          `Promoted name
        else `Ignored name

  let error diff name =
    match diff with
    | Diff.Same -> `Passed name
    | Diff.(New s) ->
        let msg = Printf.sprintf "Error: no previous snapshot, new:\n%s" s in
        `Error (name, msg)
    | Diff.(Diff s) ->
        let msg =
          Printf.sprintf "Error: difference between old and new snapshot:\n%s" s
        in
        `Error (name, msg)

  let promote diff name path snapshot =
    match diff with
    | Diff.Same -> `Passed name
    | Diff.(New _) | Diff.(Diff _) ->
        let () = Memory.Snapshot.write path snapshot in
        `Promoted name

  let run mode test =
    let Test.(Test { spec; path; name; _ }) = test in
    let prev = Memory.Snapshot.read path in
    let prev_str =
      Option.fold
        ~none:None
        ~some:(fun x -> Option.some @@ Snapshot.show spec x)
        prev
    in
    let next = Snapshot.next test prev in
    let next_str = Snapshot.show spec next in

    let diff = Diff.diff prev_str next_str in
    match mode with
    | Error -> error diff name
    | Promote -> promote diff name path next
    | Interactive -> interactive diff name path next

  let run_tests_with_res mode tests : res * int =
    let res = List.map (run mode) tests in
    let status =
      if List.exists (function `Error _ -> true | _ -> false) res then 1
      else 0
    in
    (res, status)

  let run_tests ?(mode = Error) tests =
    let (res, status) = run_tests_with_res mode tests in
    let () = print_res res in
    status
end
