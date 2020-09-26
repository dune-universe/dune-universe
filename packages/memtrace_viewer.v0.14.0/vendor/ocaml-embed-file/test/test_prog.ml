open! Core
open! Async
open! Import

let%expect_test _ =
  let prog = "embed_file.exe" in
  within_temp_dir
    ~links:[ "../bin/embed_file.exe", `In_path_as, prog ]
    (fun () ->
       let test_file = "test_file.css" in
       let%bind () =
         Writer.save
           test_file
           ~contents:
             {|
body {
  margin: 0;
  /* "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua." */
}
|}
       in
       let%bind () =
         Process.run_expect_no_output_exn
           ~prog
           ~args:([ test_file ] @ [ "-output"; "the_output" ])
           ()
       in
       let%bind () = Reader.file_contents "the_output.ml" >>| print_endline in
       [%expect
         {|
         let test_file_dot_css =
           "\nbody {\n  margin: 0;\n  /* \"Lorem ipsum dolor sit amet, consectetur adipiscing el\
            it, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\" */\n}\n"
         ;; |}];
       let%bind () = Reader.file_contents "the_output.mli" >>| print_endline in
       [%expect {| val test_file_dot_css : string |}];
       return ())
;;
