(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr
open Tst

let () = Js.Unsafe.eval_string {|
  global["Canvas"] = require("canvas");
  global["Image"] = Canvas.Image;
  var fs = require("fs");
  global["pixelmatch"] = require("pixelmatch");

  global["writeTo"] = function(canvas, fileName) {
    var out = fs.createWriteStream(fileName);
    var stream = canvas.pngStream();
    stream.on("data", function(chunk) {
      out.write(chunk);
    });
  }
|}

let (canvas: (int -> int -> Dom_html.canvasElement Js.t) Js.constr) =
  Js.Unsafe.global##._Canvas

let (image: (Dom_html.imageElement Js.t) Js.constr) =
  Js.Unsafe.global##._Image

let (pixelmatch:
  Dom_html.canvasPixelArray Js.t
  -> Dom_html.canvasPixelArray Js.t
  -> Dom_html.canvasPixelArray Js.t
  -> int
  -> int
  -> <includeAA: bool Js_of_ocaml.Js.readonly_prop; threshold: float Js_of_ocaml.Js.readonly_prop> Js.t
  -> int
) =
  Js.Unsafe.global##.pixelmatch

let (writeTo: Dom_html.canvasElement Js.t -> Js.js_string Js.t -> unit) =
  Js.Unsafe.global##.writeTo

module T = Tests.Make(struct
  let title = "Tests in node.js"

  module C = JsOfOCairo

  module N = struct
    let name = "JsOfOCairo"

    let create () =
      JsOfOCairo.create (new%js canvas 10 10)

    let backend = `Node
  end

  module DrawingTest(T: sig
    type t = {name: string; width: int; height: int; draw: C.context -> unit}
  end) = struct
    let known_failures = [
      "set_dash";
    ]

    let run {T.name; width; height; draw} =
      let known_failure = Li.Poly.contains known_failures name in
      let cairo_image = new%js image in
      cairo_image##.src := (Js.string (Frmt.apply "Tests/Drawing/Cairo/%s.png" name));
      let cairo_canvas = new%js canvas width height in
      let cairo_context = cairo_canvas##getContext Dom_html._2d_ in
      cairo_context##drawImage cairo_image 0. 0.;
      let cairo_data = cairo_context##getImageData 0. 0. (Fl.of_int width) (Fl.of_int height) in

      let jsooc_canvas = new%js canvas width height in
      draw (JsOfOCairo.create jsooc_canvas);
      let jsooc_data = (jsooc_canvas##getContext Dom_html._2d_)##getImageData 0. 0. (Fl.of_int width) (Fl.of_int height) in

      let diff_canvas = new%js canvas width height in
      let diff_context = diff_canvas##getContext Dom_html._2d_ in
      let diff_data = diff_context##createImageData width height in

      let differences =
        pixelmatch
          cairo_data##.data
          jsooc_data##.data
          diff_data##.data
          width
          height
          (object%js (_) val threshold=0.09 val includeAA=false end)
      in
      diff_context##putImageData diff_data 0. 0.;

      if differences <> 0 then begin
        writeTo cairo_canvas (Js.string (Frmt.apply "Tests/Drawing/JsOfOCairo/%s.Cairo.png" name));
        writeTo jsooc_canvas (Js.string (Frmt.apply "Tests/Drawing/JsOfOCairo/%s.JsOfOCairo.png" name));
        writeTo diff_canvas (Js.string (Frmt.apply "Tests/Drawing/JsOfOCairo/%s.diff.png" name));
      end;

      if known_failure && differences = 0 then
        fail "Expected failure but drawings are identical"
      else if not known_failure && differences <> 0 then
        fail "Drawings are different"
      end

  module Limitation(L: sig end) = struct
    let run _ = ()
  end
end)

let () =
  let argv = Li.of_array OCamlStandard.Sys.argv in
  Js.Unsafe.global##.process##.exitCode :=
    match command_line_main ~argv T.test with
      | Exit.Success -> 0
      | Exit.Failure n -> n
