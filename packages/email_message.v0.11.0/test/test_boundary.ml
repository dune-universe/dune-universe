open! Core
open Async
open Email_message
open Email_message.Private
open Expect_test_helpers

open Boundary


let boundary = create "BOUNDARY"

let split str =
  let bs = Bigstring_shared.to_string in
  let prologue, parts, epilogue = split boundary (Bigstring_shared.of_string str) in
  let prologue = Option.map prologue ~f:bs in
  let parts = List.map parts ~f:bs in
  let epilogue = Option.map epilogue ~f:bs in
  print_s [%message ""
                      (prologue : string option)
                      (parts : string list)
                      (epilogue : string option)
  ]
;;

let join (prologue, parts, epilogue) =
  let prologue = Option.map prologue ~f:Bigstring_shared.of_string in
  let epilogue = Option.map epilogue ~f:Bigstring_shared.of_string in
  let parts    = List.map   parts    ~f:String_monoid.of_string in
  let joined = join boundary (prologue, parts, epilogue) in
  (* Expect tests ignore leading and trailing whitespace (and common indentation)
     on both the 'expected' and 'actual' outputs. Wrapping in '######'s ensures
     that no whitespace is stripped. *)
  printf "######\n%s\n######" (String_monoid.to_string joined)
;;

let%expect_test "split" =
  (* Simple tests with no prologue or epilogue *)
  split
    "--BOUNDARY\n\
     \n\
     --BOUNDARY--";
  let%bind () =
    [%expect {| ((prologue ()) (parts ("")) (epilogue ())) |}]
  in
  split
    "--BOUNDARY\n\
     P\n\
     --BOUNDARY--";
  let%bind () =
    [%expect {| ((prologue ()) (parts (P)) (epilogue ())) |}]
  in

  split
    "--BOUNDARY\r\n\
     P\r\n\
     --BOUNDARY--";
  let%bind () =
    [%expect {| ((prologue ()) (parts (P)) (epilogue ())) |}]
  in

  split
    "--BOUNDARY\n\
     P\n\
     --BOUNDARY\n\
     Q\n\
     --BOUNDARY--";
  let%bind () =
    [%expect {| ((prologue ()) (parts (P Q)) (epilogue ())) |}]
  in

  (* Prologue and epilogue *)
  split
    "A\n\
     --BOUNDARY\n\
     P\n\
     --BOUNDARY--";
  let%bind () =
    [%expect {|
      ((prologue (A))
       (parts    (P))
       (epilogue ())) |}]
  in

  split
    "--BOUNDARY\n\
     P\n\
     --BOUNDARY--\n\
     B";
  let%bind () =
    [%expect {|
      ((prologue ())
       (parts    (P))
       (epilogue ("\nB"))) |}]
  in

  split
    "A\n\
     --BOUNDARY\n\
     P\n\
     --BOUNDARY--\n\
     B";
  let%bind () =
    [%expect {|
      ((prologue (A))
       (parts    (P))
       (epilogue ("\nB"))) |}]
  in

  split
    "A\r\n\
     --BOUNDARY\r\n\
     P\r\n\
     --BOUNDARY--\r\n\
     B";
  let%bind () =
    [%expect {|
      ((prologue (A))
       (parts    (P))
       (epilogue ("\r\nB"))) |}]
  in

  (* Preserve extra whitespace *)
  split
    "\n\
     A\n\
     \n\
     --BOUNDARY\n\
     \n\
     P\n\
     \n\
     --BOUNDARY--\n\
     B\n\
    ";
  let%bind () =
    [%expect {|
      ((prologue ("\nA\n"))
       (parts    ("\nP\n"))
       (epilogue ("\nB\n"))) |}]
  in

  (* Whitespace padding on boundary line *)
  split
    "--BOUNDARY \n\
     B\n\
     --BOUNDARY--";
  let%bind () =
    [%expect {|
      ((prologue ("--BOUNDARY \nB"))
       (parts    ())
       (epilogue ())) |}]
  in

  (* Content with something that looks like a boundary *)
  split
    "--BOUNDARY\n\
     not a --BOUNDARY\n\
     not a =\n\
     --BOUNDARY either\n\
     --BOUNDARY--";
  let%bind () =
    [%expect {|
      ((prologue ())
       (parts ("not a --BOUNDARY\nnot a =\n--BOUNDARY either"))
       (epilogue ())) |}]
  in
  return ()
;;

let%expect_test "join" =

  (* Simple tests with no prologue or epilogue *)
  join (None, [""], None);
  let%bind () =
    [%expect {|
        ######
        --BOUNDARY

        --BOUNDARY--
        ######
        |}]
  in

  let%bind () =
    join (None, ["P"], None);
    [%expect {|
        ######
        --BOUNDARY
        P
        --BOUNDARY--
        ######
        |}]
  in

  let%bind () =
    join (None, ["P"; "Q"], None);
    [%expect {|
        ######
        --BOUNDARY
        P
        --BOUNDARY
        Q
        --BOUNDARY--
        ######
        |}]
  in

  (* Prologue and epilogue *)
  join (Some "A", ["P"], None);
  let%bind () =
    [%expect {|
        ######
        A
        --BOUNDARY
        P
        --BOUNDARY--
        ######
        |}];
  in

  join (None, ["P"], Some "\nB");
  let%bind () =
    [%expect {|
        ######
        --BOUNDARY
        P
        --BOUNDARY--
        B
        ######
        |}]
  in

  join (Some "A", ["P"], Some "\nB");
  let%bind () =
    [%expect {|
        ######
        A
        --BOUNDARY
        P
        --BOUNDARY--
        B
        ######
        |}]
  in

  (* Preserve extra whitespace *)
  join (Some "\nA\n", ["\nP\n"], Some "\nB\n");
  let%bind () =
    [%expect {|
        ######

        A

        --BOUNDARY

        P

        --BOUNDARY--
        B

        ######
        |}]
  in
  return ()
;;

module Non_compliant = struct
  (* The following tests document undefined behavior. *)

  let%expect_test "non-compliant [split]" =
    (* Parsing of weird and malformed data into a sensible form *)
    split
      "--BOUNDARY\n\
       --BOUNDARY--";
    let%bind () =
      [%expect {|
        ((prologue ()) (parts ("")) (epilogue ()))
        |}]
    in

    split
      "";
    let%bind () =
      [%expect {|
        ((prologue (""))
         (parts    ())
         (epilogue ()))
        |}]
    in

    split
      "\n";
    let%bind () =
      [%expect {|
        ((prologue ("\n"))
         (parts    ())
         (epilogue ()))
        |}]
    in

    (* Missing boundary markers *)
    split
      "A\n\
       --BOUNDARY--\n\
       B";
    let%bind () =
      [%expect {|
        ((prologue (A)) (parts ()) (epilogue ("\nB")))
        |}]
    in

    split
      "--BOUNDARY--\n\
       B";
    let%bind () =
      [%expect {|
        ((prologue ("")) (parts ()) (epilogue ("\nB")))
        |}]
    in

    split
      "--BOUNDARY--\n\
      ";
    let%bind () =
      [%expect {|
        ((prologue ("")) (parts ()) (epilogue ("\n")))
        |}]
    in

    split
      "--BOUNDARY--";
    let%bind () =
      [%expect {|
        ((prologue (""))
         (parts    ())
         (epilogue ()))
        |}]
    in

    split
      "A\n\
       --BOUNDARY--";
    let%bind () =
      [%expect {|
        ((prologue (A))
         (parts    ())
         (epilogue ()))
        |}]
    in
    return ()
  ;;

  let%expect_test "non-compliant [join]" =

    join (Some "A", [], Some "\nB");
    let%bind () =
      [%expect {|
        ######
        A
        B
        ######
      |}]
    in

    join (None, [], Some "\nB");
    let%bind () =
      [%expect {|
        ######

        B
        ######
      |}]
    in

    join (None, [], None);
    let%bind () =
      [%expect {|
        ######


        ######
      |}]
    in

    join (Some "A", [], None);
    let%bind () =
      [%expect {|
        ######
        A
        ######
      |}]
    in
    return ()
  ;;
end
