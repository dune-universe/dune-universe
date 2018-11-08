(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr
open Tst

module T = Tests.Make(struct
  let title = "Tests in native"

  module C = Cairo

  module N = struct
    let name = "Cairo"

    let create () =
      let img = Cairo.Image.create Cairo.Image.ARGB32 ~w:10 ~h:10 in
      Cairo.create img

    let backend = `Cairo
  end

  module DrawingTest(T: sig
    type t = {name: string; width: int; height: int; draw: C.context -> unit}
  end) = struct
    let run {T.name; width; height; draw} =
      let img = Cairo.Image.create Cairo.Image.ARGB32 ~w:width ~h:height in
      let ctx = Cairo.create img in
      draw ctx;
      Cairo.PNG.write img (Frmt.apply "Tests/Drawing/Cairo/%s.png" name);
  end

  module Limitation(L: sig
    type t = {name: string; width: int; height: int; draws: (C.context -> string list) list}
  end) = struct
    let run {L.name; width; height; draws} =
      let img = Cairo.Image.create Cairo.Image.ARGB32 ~w:width ~h:height in
      draws
      |> Li.iter_i ~f:(fun ~i draw ->
        let ctx = Cairo.create img in
        let script = draw ctx in
        OutFile.with_channel (Frmt.apply "Tests/Limitations/%s.%i.txt" name i) ~f:(fun chan ->
          OutCh.print chan "<pre>\n";
          Li.iter ~f:(OutCh.print chan "%s\n") script;
          OutCh.print chan "</pre>"
        )
      );
      Cairo.PNG.write img (Frmt.apply "Tests/Limitations/%s.png" name);
  end
end)

let () =
  let argv = Li.of_array OCamlStandard.Sys.argv in
  Exit.exit (command_line_main ~argv T.test)
