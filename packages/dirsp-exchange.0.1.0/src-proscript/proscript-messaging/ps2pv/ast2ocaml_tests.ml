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

let emptyctx = Dirsp_ps2ocamlcore.Stack_context.create ()

let emptyloc = (Lexing.dummy_pos, Lexing.dummy_pos)

let number0 = (`Number 0., emptyloc)

let number1 = (`Number 1., emptyloc)

let variableA = (`Identifier "a", emptyloc)

let variableB = (`Identifier "b", emptyloc)

let add_expr a b = (`Add (a, b), emptyloc)

let sub_expr a b = (`Sub (a, b), emptyloc)

let join_with_prespace_tests =
  Alcotest.
    [ test_case
        "GIVEN empty list WHEN join_with_prespace THEN empty string"
        `Quick
        (fun _ ->
          (check string)
            "strings equal"
            ""
            (Dirsp_ps2ocamlcore.join_with_prespace []) )
    ; test_case
        "GIVEN a,b,c list WHEN join_with_prespace THEN a,b,c with space before \
         each"
        `Quick
        (fun _ ->
          (check string)
            "strings equal"
            " a b c"
            (Dirsp_ps2ocamlcore.join_with_prespace [ "a"; "b"; "c" ]) )
    ]


let overflow_underflow_tests =
  [ Alcotest.test_case
      "GIVEN (a:=maxint)+1 WHEN eval_statement THEN overflow"
      `Quick
      (fun _ ->
        Alcotest.check_raises
          "Invalid_argument raised"
          (Invalid_argument
             (Printf.sprintf "(a+1)=(%d+1) will overflow" Int.max_int) )
          (fun () ->
            let s =
              Dirsp_ps2ocamlcore.eval_statement
                0
                emptyctx
                (`Expression (add_expr variableA number1), emptyloc)
            in
            let s = String.trim s in
            (* WARNING: Make sure the [else CODE] block has an EXACT or formatted copy of the [s <> STRING] block *)
            if s
               <> "(*check s+t for overflow and underflow*) let s = a in let t \
                   = 1 in (if (s > 0) then (if (t <= Int.max_int - s) then () \
                   else raise (Invalid_argument (Format.sprintf \
                   \"(a+1)=(%d+%d) will overflow\" s t))) else if (s < 0) then \
                   (if (t >= Int.min_int - s) then () else raise \
                   (Invalid_argument (Format.sprintf \"(a+1)=(%d+%d) will \
                   underflow\" s t))));"
            then Alcotest.fail ("UNEXPECTED: " ^ s)
            else
              let a = Int.max_int in
              (*check s+t for overflow and underflow*)
              let s = a in
              let t = 1 in
              if s > 0
              then
                if t <= Int.max_int - s
                then ()
                else
                  raise
                    (Invalid_argument
                       (Format.sprintf "(a+1)=(%d+%d) will overflow" s t) )
              else if s < 0
              then
                if t >= Int.min_int - s
                then ()
                else
                  raise
                    (Invalid_argument
                       (Format.sprintf "(a+1)=(%d+%d) will underflow" s t) ) ) )
  ; Alcotest.test_case
      "GIVEN (a:=maxint-1)+1 WHEN eval_statement THEN ok"
      `Quick
      (fun _ ->
        let s =
          Dirsp_ps2ocamlcore.eval_statement
            0
            emptyctx
            (`Expression (add_expr variableA number1), emptyloc)
        in
        let s = String.trim s in
        (* WARNING: Make sure the [else CODE] block has an EXACT or formatted copy of the [s <> STRING] block *)
        if s
           <> "(*check s+t for overflow and underflow*) let s = a in let t = 1 \
               in (if (s > 0) then (if (t <= Int.max_int - s) then () else \
               raise (Invalid_argument (Format.sprintf \"(a+1)=(%d+%d) will \
               overflow\" s t))) else if (s < 0) then (if (t >= Int.min_int - \
               s) then () else raise (Invalid_argument (Format.sprintf \
               \"(a+1)=(%d+%d) will underflow\" s t))));"
        then Alcotest.fail ("UNEXPECTED: " ^ s)
        else
          let a = Int.max_int - 1 in
          (*check s+t for overflow and underflow*)
          let s = a in
          let t = 1 in
          if s > 0
          then
            if t <= Int.max_int - s
            then ()
            else
              raise
                (Invalid_argument
                   (Format.sprintf "(a+1)=(%d+%d) will overflow" s t) )
          else if s < 0
          then
            if t >= Int.min_int - s
            then ()
            else
              raise
                (Invalid_argument
                   (Format.sprintf "(a+1)=(%d+%d) will underflow" s t) ) )
  ; Alcotest.test_case
      "GIVEN (a:=minint)+(b:=maxint) WHEN eval_statement THEN ok"
      `Quick
      (fun _ ->
        let s =
          Dirsp_ps2ocamlcore.eval_statement
            0
            emptyctx
            (`Expression (add_expr variableA variableB), emptyloc)
        in
        let s = String.trim s in
        (* WARNING: Make sure the [else CODE] block has an EXACT or formatted copy of the [s <> STRING] block *)
        if s
           <> "(*check s+t for overflow and underflow*) let s = a in let t = b \
               in (if (s > 0) then (if (t <= Int.max_int - s) then () else \
               raise (Invalid_argument (Format.sprintf \"(a+b)=(%d+%d) will \
               overflow\" s t))) else if (s < 0) then (if (t >= Int.min_int - \
               s) then () else raise (Invalid_argument (Format.sprintf \
               \"(a+b)=(%d+%d) will underflow\" s t))));"
        then Alcotest.fail ("UNEXPECTED: " ^ s)
        else
          let a = Int.min_int in
          let b = Int.max_int in
          (*check s+t for overflow and underflow*)
          let s = a in
          let t = b in
          if s > 0
          then
            if t <= Int.max_int - s
            then ()
            else
              raise
                (Invalid_argument
                   (Format.sprintf "(a+b)=(%d+%d) will overflow" s t) )
          else if s < 0
          then
            if t >= Int.min_int - s
            then ()
            else
              raise
                (Invalid_argument
                   (Format.sprintf "(a+b)=(%d+%d) will underflow" s t) ) )
  ; Alcotest.test_case
      "GIVEN (a:=minint)+0 WHEN eval_statement THEN ok"
      `Quick
      (fun _ ->
        let s =
          Dirsp_ps2ocamlcore.eval_statement
            0
            emptyctx
            (`Expression (add_expr variableA number0), emptyloc)
        in
        let s = String.trim s in
        (* WARNING: Make sure the [else CODE] block has an EXACT or formatted copy of the [s <> STRING] block *)
        if s
           <> "(*check s+t for overflow and underflow*) let s = a in let t = 0 \
               in (if (s > 0) then (if (t <= Int.max_int - s) then () else \
               raise (Invalid_argument (Format.sprintf \"(a+0)=(%d+%d) will \
               overflow\" s t))) else if (s < 0) then (if (t >= Int.min_int - \
               s) then () else raise (Invalid_argument (Format.sprintf \
               \"(a+0)=(%d+%d) will underflow\" s t))));"
        then Alcotest.fail ("UNEXPECTED: " ^ s)
        else
          let a = Int.min_int in
          (*check s+t for overflow and underflow*)
          let s = a in
          let t = 0 in
          if s > 0
          then
            if t <= Int.max_int - s
            then ()
            else
              raise
                (Invalid_argument
                   (Format.sprintf "(a+0)=(%d+%d) will overflow" s t) )
          else if s < 0
          then
            if t >= Int.min_int - s
            then ()
            else
              raise
                (Invalid_argument
                   (Format.sprintf "(a+0)=(%d+%d) will underflow" s t) ) )
  ; Alcotest.test_case
      "GIVEN (a:=minint)-0 WHEN eval_statement THEN ok"
      `Quick
      (fun _ ->
        let s =
          Dirsp_ps2ocamlcore.eval_statement
            0
            emptyctx
            (`Expression (sub_expr variableA number0), emptyloc)
        in
        let s = String.trim s in
        (* WARNING: Make sure the [else CODE] block has an EXACT or formatted copy of the [s <> STRING] block *)
        if s
           <> "(*check s-t for overflow and underflow*) let s = a in let t = 0 \
               in (if (s > 0) then (if (t > 0 || -t <= Int.max_int - s) then \
               () else raise (Invalid_argument (Format.sprintf \"(a-0)=(%d - \
               %d) will overflow\" s t))) else if (s < 0) then (if (t < 0 || \
               (t <> Int.max_int && -t >= Int.min_int - s)) then () else raise \
               (Invalid_argument (Format.sprintf \"(a-0)=(%d - %d) will \
               underflow\" s t))));"
        then Alcotest.fail ("UNEXPECTED: " ^ s)
        else
          let a = Int.min_int in
          (*check s-t for overflow and underflow*)
          let s = a in
          let t = 0 in
          if s > 0
          then
            if t > 0 || -t <= Int.max_int - s
            then ()
            else
              raise
                (Invalid_argument
                   (Format.sprintf "(a-0)=(%d - %d) will overflow" s t) )
          else if s < 0
          then
            if t < 0 || (t <> Int.max_int && -t >= Int.min_int - s)
            then ()
            else
              raise
                (Invalid_argument
                   (Format.sprintf "(a-0)=(%d - %d) will underflow" s t) ) )
  ; Alcotest.test_case
      "GIVEN (a:=minint)-1 WHEN eval_statement THEN underflow"
      `Quick
      (fun _ ->
        Alcotest.check_raises
          "Invalid_argument raised"
          (Invalid_argument
             (Printf.sprintf "(a-1)=(%d - 1) will underflow" Int.min_int) )
          (fun () ->
            let s =
              Dirsp_ps2ocamlcore.eval_statement
                0
                emptyctx
                (`Expression (sub_expr variableA number1), emptyloc)
            in
            let s = String.trim s in
            (* WARNING: Make sure the [else CODE] block has an EXACT or formatted copy of the [s <> STRING] block *)
            if s
               <> "(*check s-t for overflow and underflow*) let s = a in let t \
                   = 1 in (if (s > 0) then (if (t > 0 || -t <= Int.max_int - \
                   s) then () else raise (Invalid_argument (Format.sprintf \
                   \"(a-1)=(%d - %d) will overflow\" s t))) else if (s < 0) \
                   then (if (t < 0 || (t <> Int.max_int && -t >= Int.min_int - \
                   s)) then () else raise (Invalid_argument (Format.sprintf \
                   \"(a-1)=(%d - %d) will underflow\" s t))));"
            then Alcotest.fail ("UNEXPECTED: " ^ s)
            else
              let a = Int.min_int in
              (*check s-t for overflow and underflow*)
              let s = a in
              let t = 1 in
              if s > 0
              then
                if t > 0 || -t <= Int.max_int - s
                then ()
                else
                  raise
                    (Invalid_argument
                       (Format.sprintf "(a-1)=(%d - %d) will overflow" s t) )
              else if s < 0
              then
                if t < 0 || (t <> Int.max_int && -t >= Int.min_int - s)
                then ()
                else
                  raise
                    (Invalid_argument
                       (Format.sprintf "(a-1)=(%d - %d) will underflow" s t) )
            ) )
  ]


let () =
  let open Alcotest in
  run
    "ast2ocaml"
    [ ("join_with_prespace", join_with_prespace_tests)
    ; ("overflow_underflow", overflow_underflow_tests)
    ]
