(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open StdLabels

module Tests = DrawingTests.Make(Cairo)

let () =
  Tests.tests
  |> List.iter ~f:(fun {Tests.name; width; height; draw; known_failure=_} ->
    let img = Cairo.Image.create Cairo.Image.ARGB32 ~width ~height in
    let ctx = Cairo.create img in
    draw ctx;
    Cairo.PNG.write img (Printf.sprintf "drawing_tests/in_command_line/%s.png" name);
  )
