(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr

module Make(C: CairoMock.S) = struct
  type t = {name: string; width: int; height: int; draws: (C.context -> string list) list}

  module DecoratedC = CairoMock.Decorate(C)

  let make name width height draws =
    let draws =
      draws
      |> Li.map ~f:(fun draw ->
        fun ctx ->
          let ctx = DecoratedC.create ctx in
          draw ctx;
          DecoratedC.calls ctx
      )
    in
    {name; width; height; draws}

  let limitations = DecoratedC.([
    make "arc_more_than_2pi" 100 100 [fun ctx ->
      arc ctx 50. 50. ~r:40. ~a1:0. ~a2:7.;
      ignore (Path.get_current_point ctx);
      line_to ctx 50. 50.;
      stroke ctx
    ];
    make "font_extents" 100 10 [fun ctx ->
      ignore (font_extents ctx)
    ];
    make "text_extents" 100 10 [fun ctx ->
      ignore (text_extents ctx "Hello")
    ];
    make "show_text" 100 240 [fun ctx ->
      set_font_size ctx 20.;
      [
        (Upright, Normal, "sans-serif");
        (Italic, Normal, "sans-serif");
        (Oblique, Normal, "sans-serif");
        (Upright, Bold, "sans-serif");
        (Italic, Bold, "sans-serif");
        (Oblique, Bold, "sans-serif");
        (Upright, Normal, "serif");
        (Upright, Normal, "monospace");
      ]
      |> Li.iter ~f:(fun (slant, weight, family) ->
        move_to ctx 10. 20.;
        select_font_face ctx ~slant ~weight family;
        show_text ctx "He";
        show_text ctx "llo";
        translate ctx 0. 30.;
      )
    ];
    make "reuse_canvas_transformation" 100 60 [
      (fun ctx -> scale ctx 2. 3.);
      (fun ctx ->
        move_to ctx 10. 10.;
        line_to ctx 40. 10.;
        stroke ctx);
    ];
    make "reuse_canvas_line_cap" 100 60 [
      (fun ctx -> set_line_cap ctx ROUND);
      (fun ctx ->
        set_line_width ctx 20.;
        move_to ctx 30. 30.;
        line_to ctx 70. 30.;
        stroke ctx);
    ];
  ])
end
