(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr
open Tst

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

module DrawingTests: sig
  val onload: (Dom_html.imageElement Js.t StrSoMap.t -> unit) -> unit
end = struct
  let tests_count = Li.size Tests.drawing_tests

  let onload callback =
    let cairo_images = ref StrSoMap.empty in
    Tests.drawing_tests
    |> Li.iter ~f:(fun name ->
      let cairo_image = Dom_html.createImg Dom_html.document in
      cairo_image##.src := Js.string (Frmt.apply "Tests/Drawing/Cairo/%s.png" name);
      cairo_image##.onload := (Dom.handler (fun _ ->
        cairo_images := StrSoMap.replace !cairo_images ~k:name ~v:cairo_image;
        if StrSoMap.size !cairo_images = tests_count then callback !cairo_images;
        Js._true
      ));
    )
end

let drawing_test_ps = ref StrSoMap.empty

let result_to_element result =
  let rec aux is_in_drawing_tests = function
    | Result.Single {label; status} ->
      let li = Dom_html.createLi Dom_html.document in
      let p = Dom_html.createP Dom_html.document in
      li##.className := Js.string (if status = Result.Status.Success then "success" else "failure");
      p##.textContent := Js.some (Js.string (Frmt.apply "%s: %s" label (Result.Status.to_string status)));
      Dom.appendChild li p;
      if is_in_drawing_tests then Dom.appendChild li (StrSoMap.get !drawing_test_ps ~k:label);
      li
    | Result.Group {name; children; counts={Result.Counts.successes; failures; errors}} ->
      let li = Dom_html.createLi Dom_html.document in
      let p = Dom_html.createP Dom_html.document in
      li##.className := Js.string (if failures + errors = 0 then "success" else "failure");
      p##.textContent := Js.some (Js.string (Frmt.apply "%s: (successes: %i, failures: %i, errors: %i)" name successes failures errors));
      Dom.appendChild li p;
      let ul = Dom_html.createUl Dom_html.document in
      Dom.appendChild li ul;
      children
      |> Li.iter ~f:(fun child ->
        Dom.appendChild ul (aux (name = "Drawing tests on JsOfOCairo") child)
      );
      li
  in
  let ul = Dom_html.createUl Dom_html.document in
  Dom.appendChild ul (aux false result);
  ul

let () = DrawingTests.onload (fun cairo_images ->
  let module T = Tests.Make(struct
    let title = "Tests in browser"

    module C = JsOfOCairo

    module N = struct
      let name = "JsOfOCairo"

      let create () =
        let canvas = Dom_html.createCanvas Dom_html.document in
        canvas##.width := 10;
        canvas##.height := 10;
        JsOfOCairo.create canvas

      let backend = `Browser
    end

    module DrawingTest(T: sig
      type t = {name: string; width: int; height: int; draw: C.context -> unit}
    end) = struct
      let run {T.name; width; height; draw} =
        let cairo_image = StrSoMap.get cairo_images ~k:name in
        let cairo_canvas = Dom_html.createCanvas Dom_html.document in
        cairo_canvas##.width := width;
        cairo_canvas##.height := height;
        let cairo_context = cairo_canvas##getContext Dom_html._2d_ in
        cairo_context##drawImage cairo_image 0. 0.;
        let cairo_data = cairo_context##getImageData 0. 0. (Fl.of_int width) (Fl.of_int height) in

        let jsooc_canvas = Dom_html.createCanvas Dom_html.document in
        jsooc_canvas##.width := width;
        jsooc_canvas##.height := height;
        draw (JsOfOCairo.create jsooc_canvas);
        let jsooc_data = (jsooc_canvas##getContext Dom_html._2d_)##getImageData 0. 0. (Fl.of_int width) (Fl.of_int height) in

        let diff_canvas = Dom_html.createCanvas Dom_html.document in
        diff_canvas##.width := width;
        diff_canvas##.height := height;
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

        let p = Dom_html.createP Dom_html.document in
        (* p##.textContent := Js.some (Js.string (Frmt.apply "Some info for %s" name)); *)
        Dom.appendChild p cairo_canvas;
        Dom.appendChild p jsooc_canvas;
        Dom.appendChild p diff_canvas;

        drawing_test_ps := StrSoMap.replace !drawing_test_ps ~k:name ~v:p;

        check_int ~expected:0 differences
    end

    module Limitation(L: sig
      type t = {name: string; width: int; height: int; draws: (C.context -> string list) list}
    end) = struct
      let make_col innerHTML =
        let div = Dom_html.(createDiv document) in
        div##setAttribute (Js.string "class") (Js.string "col");
        div##.innerHTML := Js.string innerHTML;
        div

      let run {L.name; width; height; draws} =
        let canvas = Dom_html.(createCanvas document) in
        canvas##.width := width;
        canvas##.height := height;
        let scripts ~i draw =
          let cairo = Frmt.apply "<div class=\"cairo_pre\" data-src=\"Tests/Limitations/%s.%i.txt\"></div>" name i
          and jsooc =
            draw (JsOfOCairo.create canvas)
            |> Li.map ~f:(Frmt.apply "%s\n")
            |> StrLi.join
            |> Frmt.apply "<pre>%s</pre>"
          in
          (cairo, jsooc)
        in
        let scripts =
          match draws with
            | [draw] ->
              [scripts ~i:0 draw]
            | _ ->
              draws
              |> Li.map_i ~f:(fun ~i draw ->
                let (cairo, jsooc) = scripts ~i draw
                and header = Frmt.apply "<p>Context n°%i:</p>" i in
                (header ^ cairo, header ^ jsooc)
              )
        in
        let cairo_col =
          let script =
            scripts
            |> Li.map ~f:Tu2.get_0
            |> StrLi.join
          in
          make_col (Frmt.apply "<h3>Cairo:</h3>%s<img src=\"Tests/Limitations/%s.png\"></img>" script name)
        and jsooc_col =
          let script =
            scripts
            |> Li.map ~f:Tu2.get_1
            |> StrLi.join
          in
          let col = make_col (Frmt.apply "<h3>JsOfOCairo:</h3>%s" script) in
          Dom.appendChild col canvas;
          col
        in
        let container = Dom_html.getElementById (Frmt.apply "limitations_%s" name) in
        Dom.appendChild container cairo_col;
        Dom.appendChild container jsooc_col
    end
  end) in
  T.test
  |> Test.run
  |> result_to_element
  |> Dom.appendChild (Dom_html.getElementById "tests_in_browser");
  ignore (Js.Unsafe.eval_string {|
    jQuery("#tests_in_browser ul ul ul").hide();
    jQuery("#tests_in_browser p").click(function() {
      jQuery(this).parent().children("ul").slideToggle();
    });
    jQuery("div.cairo_pre").each(function() {
      var div = jQuery(this);
      div.load(div.data("src"));
    })
  |});
)
