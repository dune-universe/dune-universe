
open EzFile.OP

let create_files dir =
  let dir_c = dir // "c" in
  let dir_f = dir // "f" in
  let dir_f_d = dir_f // "d" in
  let dir_f_m = dir_f // "m" in
  let dir_f_r = dir_f // "r" in
  let dir_u = dir // "u" in
  List.iter (fun dir ->
      EzFile.make_dir ~p:true dir)
    [ dir;
      dir_c;
      dir_f_d;
      dir_f_m;
      dir_f_r;
      dir_u;
    ];
  List.iter (fun file ->
      EzFile.write_file file file)
    [
      dir // "a.ml";
      dir // "d.mli";
      dir // "g.txt";
      dir // "z.obj";


      dir_c // "x.ml";
      dir_f // "b.ml";
      dir_f // "e.mli";
      dir_f // "o.mli";
      dir_f // "y.ml";
      dir_u // "x.ml";
    ];
  ()

let test () =

  let _ret = Sys.command "rm -rf tmpx" in
  let dir = "tmpx" in
  create_files dir;

  let selectors =
    [
      EzFile.select ();
      EzFile.select ~deep:true ();
      EzFile.select ~deep:true ~dft:`After ();
      EzFile.select ~deep:true ~dft:`Before ();

      EzFile.select ~kinds:[Unix.S_REG] ();
      EzFile.select ~kinds:[Unix.S_REG] ~deep:true ();
      EzFile.select ~kinds:[Unix.S_REG] ~deep:true ~dft:`After ();
      EzFile.select ~kinds:[Unix.S_REG] ~deep:true ~dft:`Before ();
    ]
  in

  List.iter (fun select ->
      Printf.printf "==================================\n";
      EzFile.iter_dir ~select dir ~f:(fun file ->
          Printf.printf "file %s\n%!" file
        )
    ) selectors;

  List.iter (fun select ->
      Printf.printf "----------------------------------\n";
      let iterator = EzFile.iterator ~select dir in
      let rec iter () =
        match iterator () with
        | Some file ->
            Printf.printf "file %s\n%!" file;
            iter ()
        | None -> ()
      in
      iter ()
    ) selectors;
  ()


let%expect_test "getcwd" =
  test ();
  [%expect {|
    ==================================
    file a.ml
    file c
    file d.mli
    file f
    file g.txt
    file u
    file z.obj
    ==================================
    file a.ml
    file c
    file d.mli
    file f
    file g.txt
    file u
    file z.obj
    file c/x.ml
    file f/b.ml
    file f/d
    file f/e.mli
    file f/m
    file f/o.mli
    file f/r
    file f/y.ml
    file u/x.ml
    ==================================
    file a.ml
    file c/x.ml
    file c
    file d.mli
    file f/b.ml
    file f/d
    file f/e.mli
    file f/m
    file f/o.mli
    file f/r
    file f/y.ml
    file f
    file g.txt
    file u/x.ml
    file u
    file z.obj
    ==================================
    file a.ml
    file c
    file c/x.ml
    file d.mli
    file f
    file f/b.ml
    file f/d
    file f/e.mli
    file f/m
    file f/o.mli
    file f/r
    file f/y.ml
    file g.txt
    file u
    file u/x.ml
    file z.obj
    ==================================
    file a.ml
    file d.mli
    file g.txt
    file z.obj
    ==================================
    file a.ml
    file d.mli
    file g.txt
    file z.obj
    file c/x.ml
    file f/b.ml
    file f/e.mli
    file f/o.mli
    file f/y.ml
    file u/x.ml
    ==================================
    file a.ml
    file c/x.ml
    file d.mli
    file f/b.ml
    file f/e.mli
    file f/o.mli
    file f/y.ml
    file g.txt
    file u/x.ml
    file z.obj
    ==================================
    file a.ml
    file c/x.ml
    file d.mli
    file f/b.ml
    file f/e.mli
    file f/o.mli
    file f/y.ml
    file g.txt
    file u/x.ml
    file z.obj
    ----------------------------------
    file a.ml
    file c
    file d.mli
    file f
    file g.txt
    file u
    file z.obj
    ----------------------------------
    file a.ml
    file c
    file d.mli
    file f
    file g.txt
    file u
    file z.obj
    file c/x.ml
    file f/b.ml
    file f/d
    file f/e.mli
    file f/m
    file f/o.mli
    file f/r
    file f/y.ml
    file u/x.ml
    ----------------------------------
    file a.ml
    file c/x.ml
    file c
    file d.mli
    file f/b.ml
    file f/d
    file f/e.mli
    file f/m
    file f/o.mli
    file f/r
    file f/y.ml
    file f
    file g.txt
    file u/x.ml
    file u
    file z.obj
    ----------------------------------
    file a.ml
    file c
    file c/x.ml
    file d.mli
    file f
    file f/b.ml
    file f/d
    file f/e.mli
    file f/m
    file f/o.mli
    file f/r
    file f/y.ml
    file g.txt
    file u
    file u/x.ml
    file z.obj
    ----------------------------------
    file a.ml
    file d.mli
    file g.txt
    file z.obj
    ----------------------------------
    file a.ml
    file d.mli
    file g.txt
    file z.obj
    file c/x.ml
    file f/b.ml
    file f/e.mli
    file f/o.mli
    file f/y.ml
    file u/x.ml
    ----------------------------------
    file a.ml
    file c/x.ml
    file d.mli
    file f/b.ml
    file f/e.mli
    file f/o.mli
    file f/y.ml
    file g.txt
    file u/x.ml
    file z.obj
    ----------------------------------
    file a.ml
    file c/x.ml
    file d.mli
    file f/b.ml
    file f/e.mli
    file f/o.mli
    file f/y.ml
    file g.txt
    file u/x.ml
    file z.obj |}]

let _unused filename =
  let () =
    EzFile.make_select EzFile.iter_dir ~deep:true filename
      ~f:(fun path ->
          print_string path;
        )
  in
  ()
