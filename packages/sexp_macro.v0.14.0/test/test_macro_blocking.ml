open Expect_test_helpers_core
open Sexp_macro
open Sexplib
open Sexplib.Conv
open Printf

module type Load = sig
  val load_sexp_conv_exn : ?allow_includes:bool -> string -> (Sexp.t -> 'a) -> 'a

  val load_sexps_conv
    :  ?allow_includes:bool
    -> string
    -> (Sexp.t -> 'a)
    -> 'a Sexp.Annotated.conv list

  val included_files : string -> string list
end

let () =
  Printexc.register_printer (fun exc ->
    match Sexplib.Conv.sexp_of_exn_opt exc with
    | None -> None
    | Some sexp -> Some (Sexp.to_string_hum ~indent:2 sexp))
;;

let command_exn str =
  match Sys.command str with
  | 0 -> ()
  | code -> failwith (sprintf "command %S exited with code %d" str code)
;;

let make ?(reference : (module Load) option) (module Load : Load) =
  (* shadowing Blocking to avoid mistakenly calling it instead of Load *)
  let module Blocking = struct end in
  let id x = x in
  let with_files files ~f =
    let dir = Core.Filename.temp_dir "macros-test" "" in
    List.iter
      (fun (file, contents) ->
         let file_dir = Filename.concat dir (Filename.dirname file) in
         command_exn ("mkdir -p " ^ file_dir);
         let out_channel = open_out (Filename.concat dir file) in
         output_string out_channel (contents ^ "\n");
         close_out out_channel)
      files;
    let tear_down () = command_exn ("rm -rf -- " ^ dir) in
    try
      let v = f dir in
      tear_down ();
      v
    with
    | e ->
      tear_down ();
      raise e
  in
  (* Not quite the same as [Str] functions because it reapplies itself, see the
     use below to eliminate "/./././...". *)
  let replace ~sub ~by str =
    let rec loop str i =
      if i + String.length sub < String.length str
      then
        if String.sub str i (String.length sub) = sub
        then (
          let str =
            String.sub str 0 i
            ^ by
            ^ String.sub
                str
                (i + String.length sub)
                (String.length str - i - String.length sub)
          in
          loop str i)
        else loop str (i + 1)
      else str
    in
    loop str 0
  in
  let replace dir sexp =
    sexp
    |> sexp_to_string
    |> replace ~sub:"/./" ~by:"/"
    |> replace ~sub:dir ~by:"DIR"
    |> Sexp.of_string
    (* reparse to normalize the layout (depends on the length of [dir]) *)
  in
  let replace_to_string dir sexp = sexp_to_string (replace dir sexp) in
  let print_header description files =
    let files =
      List.map
        (fun (file, contents) ->
           match Sexp.of_string (String.concat "" [ "("; contents; ")" ]) with
           | Atom _ -> assert false
           | List contents -> [%sexp (file : string), (contents : Sexp.t list)]
           | exception _ -> [%sexp (file : string), (contents : string)])
        files
    in
    print_s [%message "test" description (files : Sexp.t list)]
  in
  let check_equal ~description ~files ~actual ~reference =
    if actual = reference
    then (
      print_s [%message "test" description];
      print_endline "Actual output agrees with reference output.")
    else (
      print_header description files;
      print_string
        (Base.String.concat
           [ "Actual output does not agree with reference output.\nActual:\n"
           ; actual
           ; "Reference:\n"
           ; reference
           ]))
  in
  let check ?(f = id) ?allow_includes description files =
    with_files files ~f:(fun dir ->
      let filename = Filename.concat dir "input.sexp" in
      let output load =
        match Load.included_files filename, load ?allow_includes filename f with
        | included_files, output ->
          let included_files = replace dir ([%sexp_of: string list] included_files) in
          [%message (output : Sexp.t) (included_files : Sexp.t)] |> sexp_to_string
        | exception exn -> replace_to_string dir [%sexp "raised", (exn : exn)]
      in
      let actual = output Load.load_sexp_conv_exn in
      match reference with
      | None ->
        print_header description files;
        print_string actual
      | Some (module Reference) ->
        check_equal
          ~description
          ~files
          ~actual
          ~reference:(output Reference.load_sexp_conv_exn));
    print_newline ()
  in
  let check_error_count description ~f files =
    with_files files ~f:(fun dir ->
      let output load =
        let results = load (Filename.concat dir "input.sexp") f in
        replace_to_string dir [%sexp (results : _ Sexp.Annotated.conv list)]
      in
      let actual = output Load.load_sexps_conv in
      match reference with
      | None ->
        print_header description files;
        print_string actual
      | Some (module Reference) ->
        check_equal
          ~description
          ~files
          ~actual
          ~reference:(output Reference.load_sexps_conv));
    print_newline ()
  in
  check
    "simple"
    [ ( "input.sexp"
      , "(:include defs.sexp)\n\
        \         ((field1 value1)\n\
        \         (field2 ((:include include.sexp) 0004 0005))\n\
        \         (field3 (:concat a (:use f (x (:use x))))))" )
    ; "defs.sexp", "(:let x () y z)\n         (:let f (x) (:concat (:use x) (:use x)))"
    ; "include.sexp", "0001 0002 0003"
    ];
  check
    "include chain with subdirectories"
    [ "input.sexp", "(:include include/a.sexp)"
    ; "include/a.sexp", "(:include b.sexp)"
    ; "include/b.sexp", "(this is include/b)"
    ];
  check
    "not all files included"
    [ "input.sexp", "(:include include/a.sexp)"
    ; "include/a.sexp", "(:include b.sexp)"
    ; "include/b.sexp", "(this is include/b)"
    ; "include/c.sexp", "(this is include/c)"
    ];
  check
    "hello world"
    [ ( "input.sexp"
      , "(:include defs.sexp)\n\
        \         (:include template.sexp)\n\
        \         (:use f (a (:use a)) (b (:use b)))" )
    ; "defs.sexp", "(:let a () hello)\n         (:let b () \" world\")"
    ; "template.sexp", "(:let f (a b) (:concat (:use a) (:use b)))"
    ];
  check
    "chained let"
    [ ( "input.sexp"
      , "(:let f (x) (:use x))\n\
        \       (:let g (x) (:use f (x (:use x))))\n\
        \       (:use g (x bla))" )
    ];
  check
    "nested let1"
    [ ( "input.sexp"
      , "(:let f (x)\n\
        \           (:let g (y)\n\
        \              (:use y) (:use y))\n\
        \           (:use g (y (:use x))) (:use g (y (:use x))))\n\
        \         (:concat (:use f (x x)))" )
    ];
  check
    "nested let2"
    [ ( "input.sexp"
      , "(:let f (x)\n\
        \             (:let g (y) (:use x) (:use y))\n\
        \             (:use g (y (:use x))))\n\
        \         ((:use f (x bla)))" )
    ];
  check
    "argument list scoping"
    [ ( "input.sexp"
      , "(:let a () a)\n\
        \         (:let b () b)\n\
        \         (:let f (b a) (:concat (:use b) (:use a)))\n\
        \         (:use f (b (:use a)) (a (:use b)))" )
    ];
  check
    "empty argument"
    [ "input.sexp", "(:let f (x) (:use x) bla)\n                        (:use f (x))" ];
  check
    "scoping1"
    [ "input.sexp", "(:include include.sexp)\n       (:use f (x bla1))"
    ; ( "include.sexp"
      , "(:let y () bla2)\n       (:let f (x)\n           ((:use x) (:use y)))" )
    ];
  check
    "scoping2"
    [ "input.sexp", "(:include include.sexp)\n       (:use f (x bar))"
    ; ( "include.sexp"
      , "(:let f (x)\n\
        \           (:let g () (:use x))\n\
        \           (:let x () foo)\n\
        \           ((:use g) (:use x)))" )
    ];
  (* Function body is expanded at runtime, new definition of greet does nothing *)
  check
    "scoping3"
    [ ( "input.sexp"
      , "(:include include.sexp)\n\
        \       (:let greet () hide greet function)\n\
        \       (:use hi (name jane))" )
    ; ( "include.sexp"
      , "(:let greet (greeting name) (:concat (:use greeting) (:use name)))\n\
        \       (:let hi (name) (:use greet (greeting \"hi \") (name (:use name))))" )
    ];
  check
    "scoping4"
    [ "input.sexp", "(:include include.sexp)\n       ((:use f (x foo)))"
    ; ( "include.sexp"
      , "(:let f (x)\n\
        \           (:let g () (:use x))\n\
        \           (:let x () bar)\n\
        \           (:use g)\n\
        \           (:let f (y) (:use x) (:use y))\n\
        \           (:use f (y (:use g))))" )
    ];
  check
    "argument shadowing"
    [ "input.sexp", "(:include include.sexp)\n       (:use f (x foo))"
    ; ( "include.sexp"
      , "(:let f (x)\n\
        \           (:let g (x) (:use x))\n\
        \           ((:use x) (:use g (x bar))))" )
    ];
  check
    "high order function attempt"
    [ "input.sexp", "(:include include.sexp)\n       (:use g (f f))"
    ; ( "include.sexp"
      , "(:let f (x) (:concat (:use x) (:use x)))\n       (:let g (f) (:use f (x bla)))"
      )
    ];
  check
    "high order function attempt 2"
    [ "input.sexp", "(:include include.sexp)\n       (:use g (f (:use f)))"
    ; ( "include.sexp"
      , "(:let f (x) (:concat (:use x) (:use x)))\n       (:let g (f) (:use f (x bla)))"
      )
    ];
  (* Variable x is not used because g is never called *)
  check
    "unused variable (inside function macro)"
    [ "input.sexp", "(:include include.sexp)\n       (:use f (x x))"
    ; "include.sexp", "(:let f (x)\n           (:let g () (:use x))\n           ())"
    ];
  check
    "malformed concat"
    [ "input.sexp", "(:include include.sexp)\n       (:use f (x ()))"
    ; "include.sexp", "(:let f (x) (:concat (:use x) (:concat foo bar)))"
    ];
  check
    "error evaluating macros"
    [ "input.sexp", "(:include include.sexp)"; "include.sexp", "(:let f (()) foo)" ];
  check
    "error evaluating macros"
    [ "input.sexp", "(:include include.sexp)"; "include.sexp", "(:let f x foo)" ];
  check
    "unexpected :use"
    [ "input.sexp", "(:include include.sexp)"; "include.sexp", "(:concat :use x)" ];
  check
    "malformed argument"
    [ "input.sexp", "(:include include.sexp)"
    ; "include.sexp", "(:let f (x) (:use x))\n                          (:use f (()))"
    ];
  check
    "argument mismatch"
    [ "input.sexp", "(:include include.sexp)"
    ; "include.sexp", "(:let f (x) (:use x)) (:use f (y x))"
    ];
  check "unused variable" [ "input.sexp", "(:let f (a) body of f) (:use f (a a))" ];
  check
    "duplicated let argument"
    [ ( "input.sexp"
      , "(:let f (a a) (:concat (:use a) (:use a)))\n\
        \                       (:use f (a foo) (a foo))" )
    ];
  check
    "undefined variable in let"
    [ "input.sexp", "(:include include.sexp) ()"
    ; "include.sexp", "(:let f (x) ((:use x) (:use y)))"
    ];
  check
    "undefined variable"
    [ "input.sexp", "(:let x () x) (:include include.sexp)"; "include.sexp", "(:use x)" ];
  check
    ":include can cause variable capture"
    [ ( "input.sexp"
      , "(:let x () 2)\n         (:include include.sexp)\n         (:use x)" )
    ; "include.sexp", "(:let x () 1)"
    ];
  check "malformed concat" [ "input.sexp", "(:concat (a b))" ];
  check
    "malformed concat"
    [ "input.sexp", "(:include include.sexp)\n         (:use f (a ()))"
    ; "include.sexp", "(:let f (a)\n           (:concat (:use a)))"
    ];
  check
    "correct error location in a nested let"
    [ ( "input.sexp"
      , "(:let f ()\n\
        \           (:let g () (:let incorrect))\n\
        \           (:use g))\n\
        \         (:use f)" )
    ];
  check
    "correct location with chains of includes"
    [ "input.sexp", "(:include a)"
    ; "a", "(:include b)"
    ; "b", "something invalid like :concat"
    ];
  check "empty let body" [ "input.sexp", "\n(:let f ())" ];
  let rec conv_error = function
    | Sexp.List [ Sexp.Atom "trigger"; Sexp.Atom "error" ] as t ->
      raise (Pre_sexp.Of_sexp_error (Exit, t))
    | Sexp.Atom _ -> ()
    | Sexp.List ts -> List.iter conv_error ts
  in
  let conv_error sexp =
    conv_error sexp;
    sexp
  in
  check
    "error location for conversion errors"
    ~f:conv_error
    [ "input.sexp", "(:include include.sexp)"
    ; "include.sexp", "(:let err () error) (foo bar (trigger (:use err)))"
    ];
  check_error_count
    "multiple conversion errors"
    ~f:conv_error
    [ "input.sexp", "(:include include.sexp) (:include include.sexp)"
    ; "include.sexp", "(:let err () error) (foo bar (trigger (:use err)))"
    ];
  check
    "include loop"
    [ "input.sexp", "(:include include.sexp)"; "include.sexp", "(:include include.sexp)" ];
  (* what stops this loop is that the filenames become too long. We have to rewrite the
     error messages since the exact number of "./" in the path depends on the limit on
     path length. *)
  check
    "sneaky include loop"
    [ "input.sexp", "(:include include.sexp)"
    ; "include.sexp", "(:include ././include.sexp)"
    ];
  check
    "parsing error 1"
    [ "input.sexp", "(:include include.sexp) ()"; "include.sexp", ")" ];
  check
    "parsing error 2"
    [ "input.sexp", "(:include include.sexp) ()"; "include.sexp", "(" ];
  check
    "value is not interpreted as code"
    [ "input.sexp", "(:let id (x) (:use x)) (:use id (x ((:concat :us e) y)))" ];
  check
    ~allow_includes:false
    "can load without includes when they're forbidden"
    [ "input.sexp", "(:let id (x) (:use x)) (:use id (x y))" ];
  check
    ~allow_includes:false
    "forbidden includes raise"
    [ "input.sexp", "(:include a.sexp) (:let id (x) (:use x)) (:use id (x y))"
    ; "a.sexp", "(:let a_id (x) (:use x))"
    ]
;;

let%expect_test _ =
  make (module Blocking);
  [%expect
    {|
    (test simple (
      files (
        (input.sexp (
          (:include defs.sexp)
          ((field1 value1)
           (field2 ((:include include.sexp) 0004 0005))
           (field3 (:concat a (:use f (x (:use x))))))))
        (defs.sexp (
          (:let x () y z)
          (:let f
            (x)
            (:concat
              (:use x)
              (:use x)))))
        (include.sexp (0001 0002 0003)))))
    ((output ((field1 value1) (field2 (0001 0002 0003 0004 0005)) (field3 ayzyz)))
     (included_files (DIR/include.sexp DIR/defs.sexp DIR/input.sexp)))

    (test "include chain with subdirectories" (
      files (
        (input.sexp     ((:include include/a.sexp)))
        (include/a.sexp ((:include b.sexp)))
        (include/b.sexp ((this is include/b))))))
    ((output (this is include/b))
     (included_files (DIR/include/b.sexp DIR/include/a.sexp DIR/input.sexp)))

    (test "not all files included" (
      files (
        (input.sexp     ((:include include/a.sexp)))
        (include/a.sexp ((:include b.sexp)))
        (include/b.sexp ((this is include/b)))
        (include/c.sexp ((this is include/c))))))
    ((output (this is include/b))
     (included_files (DIR/include/b.sexp DIR/include/a.sexp DIR/input.sexp)))

    (test "hello world" (
      files (
        (input.sexp (
          (:include defs.sexp)
          (:include template.sexp)
          (:use f
            (a (:use a))
            (b (:use b)))))
        (defs.sexp (
          (:let a () hello)
          (:let b () " world")))
        (template.sexp ((
          :let f
          (a b)
          (:concat
            (:use a)
            (:use b))))))))
    ((output "hello world")
     (included_files (DIR/template.sexp DIR/defs.sexp DIR/input.sexp)))

    (test "chained let" (
      files ((
        input.sexp (
          (:let f (x) (:use x))
          (:let g (x) (:use f (x (:use x))))
          (:use g (x bla)))))))
    ((output bla) (included_files (DIR/input.sexp)))

    (test "nested let1" (
      files ((
        input.sexp (
          (:let f
            (x)
            (:let g
              (y)
              (:use y)
              (:use y))
            (:use g (y (:use x)))
            (:use g (y (:use x))))
          (:concat (:use f (x x))))))))
    ((output xxxx) (included_files (DIR/input.sexp)))

    (test "nested let2" (
      files ((
        input.sexp (
          (:let f
            (x)
            (:let g
              (y)
              (:use x)
              (:use y))
            (:use g (y (:use x))))
          ((:use f (x bla))))))))
    ((output (bla bla)) (included_files (DIR/input.sexp)))

    (test "argument list scoping" (
      files ((
        input.sexp (
          (:let a () a)
          (:let b () b)
          (:let f
            (b a)
            (:concat
              (:use b)
              (:use a)))
          (:use f
            (b (:use a))
            (a (:use b))))))))
    ((output ab) (included_files (DIR/input.sexp)))

    (test "empty argument" (
      files ((input.sexp ((:let f (x) (:use x) bla) (:use f (x)))))))
    ((output bla) (included_files (DIR/input.sexp)))

    (test scoping1 (
      files (
        (input.sexp ((:include include.sexp) (:use f (x bla1))))
        (include.sexp (
          (:let y () bla2)
          (:let f
            (x)
            ((:use x)
             (:use y))))))))
    ((output         (bla1             bla2))
     (included_files (DIR/include.sexp DIR/input.sexp)))

    (test scoping2 (
      files (
        (input.sexp ((:include include.sexp) (:use f (x bar))))
        (include.sexp ((
          :let f
          (x)
          (:let g () (:use x))
          (:let x () foo)
          ((:use g)
           (:use x))))))))
    ((output         (bar              foo))
     (included_files (DIR/include.sexp DIR/input.sexp)))

    (test scoping3 (
      files (
        (input.sexp (
          (:include include.sexp)
          (:let greet () hide greet function)
          (:use hi (name jane))))
        (include.sexp (
          (:let greet
            (greeting name)
            (:concat
              (:use greeting)
              (:use name)))
          (:let hi (name) (:use greet (greeting "hi ") (name (:use name)))))))))
    ((output "hi jane") (included_files (DIR/include.sexp DIR/input.sexp)))

    (test scoping4 (
      files (
        (input.sexp ((:include include.sexp) ((:use f (x foo)))))
        (include.sexp ((
          :let f
          (x)
          (:let g () (:use x))
          (:let x () bar)
          (:use g)
          (:let f
            (y)
            (:use x)
            (:use y))
          (:use f (y (:use g)))))))))
    ((output (foo bar foo)) (included_files (DIR/include.sexp DIR/input.sexp)))

    (test "argument shadowing" (
      files (
        (input.sexp ((:include include.sexp) (:use f (x foo))))
        (include.sexp ((
          :let f (x) (:let g (x) (:use x)) ((:use x) (:use g (x bar)))))))))
    ((output         (foo              bar))
     (included_files (DIR/include.sexp DIR/input.sexp)))

    (test "high order function attempt" (
      files (
        (input.sexp ((:include include.sexp) (:use g (f f))))
        (include.sexp (
          (:let f
            (x)
            (:concat
              (:use x)
              (:use x)))
          (:let g (f) (:use f (x bla))))))))
    (raised (
      Of_sexp_error
      DIR/include.sexp:2:19
      "Error evaluating macros: f is not a function, but it was applied to arguments"
      (invalid_sexp (:use f (x bla)))))

    (test "high order function attempt 2" (
      files (
        (input.sexp ((:include include.sexp) (:use g (f (:use f)))))
        (include.sexp (
          (:let f
            (x)
            (:concat
              (:use x)
              (:use x)))
          (:let g (f) (:use f (x bla))))))))
    (raised (
      Of_sexp_error
      DIR/input.sexp:2:18
      "Error evaluating macros: Formal args of f differ from supplied args, formal args are [x]"
      (invalid_sexp (:use f))))

    (test
     "unused variable (inside function macro)"
     (files (
       (input.sexp ((:include include.sexp) (:use f (x x))))
       (include.sexp ((:let f (x) (:let g () (:use x)) ()))))))
    ((output ()) (included_files (DIR/include.sexp DIR/input.sexp)))

    (test "malformed concat" (
      files (
        (input.sexp ((:include include.sexp) (:use f (x ()))))
        (include.sexp ((:let f (x) (:concat (:use x) (:concat foo bar))))))))
    (raised (
      Of_sexp_error
      DIR/include.sexp:1:12
      "Error evaluating macros: Malformed concat application: (:concat () foobar)"
      (invalid_sexp (:concat (:use x) (:concat foo bar)))))

    (test "error evaluating macros" (
      files (
        (input.sexp ((:include include.sexp))) (include.sexp ((:let f (()) foo))))))
    (raised (
      Of_sexp_error
      DIR/include.sexp:1:9
      "Error evaluating macros: Atom expected"
      (invalid_sexp ())))

    (test "error evaluating macros" (
      files (
        (input.sexp ((:include include.sexp))) (include.sexp ((:let f x foo))))))
    (raised (
      Of_sexp_error
      DIR/include.sexp:1:8
      "Error evaluating macros: Atom list expected"
      (invalid_sexp x)))

    (test "unexpected :use" (
      files (
        (input.sexp ((:include include.sexp))) (include.sexp ((:concat :use x))))))
    (raised (
      Of_sexp_error
      DIR/include.sexp:1:9
      "Error evaluating macros: Unexpected :use"
      (invalid_sexp :use)))

    (test "malformed argument" (
      files (
        (input.sexp ((:include include.sexp)))
        (include.sexp ((:let f (x) (:use x)) (:use f (())))))))
    (raised (
      Of_sexp_error
      DIR/include.sexp:2:34
      "Error evaluating macros: Malformed argument"
      (invalid_sexp (()))))

    (test "argument mismatch" (
      files (
        (input.sexp ((:include include.sexp)))
        (include.sexp ((:let f (x) (:use x)) (:use f (y x)))))))
    (raised (
      Of_sexp_error
      DIR/include.sexp:1:22
      "Error evaluating macros: Formal args of f differ from supplied args, formal args are [x]"
      (invalid_sexp (:use f (y x)))))

    (test "unused variable" (
      files ((input.sexp ((:let f (a) body of f) (:use f (a a)))))))
    (raised (
      Of_sexp_error
      DIR/input.sexp:1:0
      "Error evaluating macros: Unused variables: a"
      (invalid_sexp (:let f (a) body of f))))

    (test "duplicated let argument" (
      files ((
        input.sexp (
          (:let f
            (a a)
            (:concat
              (:use a)
              (:use a)))
          (:use f
            (a foo)
            (a foo)))))))
    (raised (
      Of_sexp_error
      DIR/input.sexp:1:0
      "Error evaluating macros: Duplicated let argument: a"
      (invalid_sexp (
        :let f
        (a a)
        (:concat
          (:use a)
          (:use a))))))

    (test "undefined variable in let" (
      files (
        (input.sexp ((:include include.sexp) ()))
        (include.sexp ((
          :let f
          (x)
          ((:use x)
           (:use y))))))))
    (raised (
      Of_sexp_error
      DIR/include.sexp:1:28
      "Error evaluating macros: Undefined variable (included files cannot reference variables from outside)"
      (invalid_sexp y)))

    (test "undefined variable" (
      files (
        (input.sexp ((:let x () x) (:include include.sexp)))
        (include.sexp ((:use x))))))
    (raised (
      Of_sexp_error
      DIR/include.sexp:1:6
      "Error evaluating macros: Undefined variable (included files cannot reference variables from outside)"
      (invalid_sexp x)))

    (test ":include can cause variable capture" (
      files (
        (input.sexp (
          (:let x () 2)
          (:include include.sexp)
          (:use     x)))
        (include.sexp ((:let x () 1))))))
    ((output 1) (included_files (DIR/include.sexp DIR/input.sexp)))

    (test "malformed concat" (files ((input.sexp ((:concat (a b)))))))
    (raised (
      Of_sexp_error
      DIR/input.sexp:1:0
      "Error evaluating macros: Malformed concat application: (:concat (a b))"
      (invalid_sexp (:concat (a b)))))

    (test "malformed concat" (
      files (
        (input.sexp ((:include include.sexp) (:use f (a ()))))
        (include.sexp ((:let f (a) (:concat (:use a))))))))
    (raised (
      Of_sexp_error
      DIR/include.sexp:2:11
      "Error evaluating macros: Malformed concat application: (:concat ())"
      (invalid_sexp (:concat (:use a)))))

    (test
     "correct error location in a nested let"
     (files ((
       input.sexp ((:let f () (:let g () (:let incorrect)) (:use g)) (:use f))))))
    (raised (
      Of_sexp_error
      DIR/input.sexp:2:23
      "Error evaluating macros: Unexpected :let"
      (invalid_sexp :let)))

    (test
     "correct location with chains of includes"
     (files (
       (input.sexp ((:include a)))
       (a          ((:include b)))
       (b (something invalid like :concat)))))
    (raised (
      Of_sexp_error
      DIR/b:1:23
      "Error evaluating macros: Unexpected :concat"
      (invalid_sexp :concat)))

    (test "empty let body" (files ((input.sexp ((:let f ()))))))
    (raised (
      Of_sexp_error
      DIR/input.sexp:2:0
      "Error evaluating macros: Empty let bodies not allowed"
      (invalid_sexp (:let f ()))))

    (test "error location for conversion errors" (
      files (
        (input.sexp ((:include include.sexp)))
        (include.sexp ((:let err () error) (foo bar (trigger (:use err))))))))
    (raised (
      Sexp_macro.Macro_conv_error (
        (Of_sexp_error DIR/include.sexp:1:29 Exit)
        (trigger  (:use    err))
        (expanded (trigger error)))))

    (test "multiple conversion errors" (
      files (
        (input.sexp (
          (:include include.sexp)
          (:include include.sexp)))
        (include.sexp ((:let err () error) (foo bar (trigger (:use err))))))))
    ((Error (
       (Sexp_macro.Macro_conv_error (
         (Of_sexp_error DIR/include.sexp:1:29 Exit)
         (trigger  (:use    err))
         (expanded (trigger error))))
       (trigger (:use err))))
     (Error (
       (Sexp_macro.Macro_conv_error (
         (Of_sexp_error DIR/include.sexp:1:29 Exit)
         (trigger  (:use    err))
         (expanded (trigger error))))
       (trigger (:use err)))))

    (test "include loop" (
      files (
        (input.sexp   ((:include include.sexp)))
        (include.sexp ((:include include.sexp))))))
    (raised ("Sexp_macro__Macro.Include_loop_detected(\"DIR/include.sexp\")"))

    (test "sneaky include loop" (
      files (
        (input.sexp   ((:include include.sexp)))
        (include.sexp ((:include ././include.sexp))))))
    (raised (
      "Error in file DIR/include.sexp" (
        Sys_error "DIR/include.sexp: File name too long")))

    (test "parsing error 1" (
      files ((input.sexp ((:include include.sexp) ())) (include.sexp ")"))))
    (raised (
      Sexplib.Sexp.Parse_error (
        (err_msg "DIR/include.sexp: unexpected character: ')'")
        (text_line     1)
        (text_char     0)
        (global_offset 0)
        (buf_pos       0))))

    (test "parsing error 2" (
      files ((input.sexp ((:include include.sexp) ())) (include.sexp "("))))
    (raised (
      Failure
      "DIR/include.sexp: Sexplib.Sexp.input_rev_sexps: reached EOF while in state Parsing_list"))

    (test "value is not interpreted as code" (
      files ((
        input.sexp ((:let id (x) (:use x)) (:use id (x ((:concat :us e) y))))))))
    ((output (:use y)) (included_files (DIR/input.sexp)))

    (test
     "can load without includes when they're forbidden"
     (files ((input.sexp ((:let id (x) (:use x)) (:use id (x y)))))))
    ((output y) (included_files (DIR/input.sexp)))

    (test "forbidden includes raise" (
      files (
        (input.sexp ((:include a.sexp) (:let id (x) (:use x)) (:use id (x y))))
        (a.sexp ((:let a_id (x) (:use x)))))))
    (raised (
      Of_sexp_error
      "Error evaluating macros: include macros are not allowed"
      (invalid_sexp (:include a.sexp)))) |}]
;;

let%expect_test _ =
  print_s
    [%sexp
      (Sexp_macro.expand_local_macros [ Sexp.of_string "(:use x)" ]
       : Sexp.t list Sexp_macro.conv)];
  [%expect {|
    (Error ((Failure "Error evaluating macros: Undefined variable") x)) |}]
;;
