(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr
open Tst

module Make(C: CairoMock.S)(N: sig
  val name: string
  val create: unit -> C.context
  val backend: [`Cairo | `Node | `Browser | `CairoMock ]
end) = struct
  module DecoratedC = CairoMock.Decorate(C)
  open DecoratedC

  let backend_name = match N.backend with
    | `Cairo -> "Cairo"
    | `Node | `Browser -> "JsOfOCairo" (*BISECT-IGNORE*) (* Test code *)
    | `CairoMock -> "CairoMock"

  let make_n' name fs checks =
    name >: (lazy (
      let c = create (N.create ()) in
      Li.iter ~f:(fun f -> ignore (f c)) fs;
      Li.Two.to_pair_list checks (calls c)
      |> Li.iter ~f:(fun (check, actual) ->
        check actual
      )
    ))

  let make' name f check =
    make_n' name [f] [check]

  let make_n name fs expected =
    make_n' name fs (Li.map ~f:(fun expected -> check_string ~expected) expected)

  let make name f expected =
    Frmt.with_result ~f:(fun expected ->
      make_n name [f] [expected]
    ) expected

  let catch error f ctx =
    expect_exception ~expected:(Error error) (lazy (f ctx))

  let test = ~:: "Decoration tests on CairoMock.Decorate(%s)" N.name ([
    make_n "save, restore" [save; restore] ["save"; "restore"];
    make "invalid restore" (catch INVALID_RESTORE restore) "restore -> raise (%s.Error(INVALID_RESTORE))" backend_name;

    make "scale" (fun c -> scale c 3. 2.) "scale 3.00 2.00";
    make "translate" (fun c -> translate c 3. 2.) "translate 3.00 2.00";
    make "rotate" (fun c -> rotate c 3.) "rotate 3.00";
    make "transform" (fun c -> transform c {xx=1.; xy=2.; yx=3.; yy=4.; x0=5.; y0=6.}) "transform {xx=1.00; xy=2.00; yx=3.00; yy=4.00; x0=5.00; y0=6.00}";
    make "set_matrix" (fun c -> set_matrix c {xx=1.; xy=2.; yx=3.; yy=4.; x0=5.; y0=6.}) "set_matrix {xx=1.00; xy=2.00; yx=3.00; yy=4.00; x0=5.00; y0=6.00}";
    make "identity_matrix" identity_matrix "identity_matrix";
    make "get_matrix" get_matrix "get_matrix -> {xx=1.00; xy=0.00; yx=0.00; yy=1.00; x0=0.00; y0=0.00}";
    make "user_to_device" (fun c -> user_to_device c 2. 3.) "user_to_device 2.00 3.00 -> (2.00, 3.00)";
    make "user_to_device_distance" (fun c -> user_to_device_distance c 2. 3.) "user_to_device_distance 2.00 3.00 -> (2.00, 3.00)";
    make "device_to_user" (fun c -> device_to_user c 2. 3.) "device_to_user 2.00 3.00 -> (2.00, 3.00)";
    make "device_to_user_distance" (fun c -> device_to_user_distance c 2. 3.) "device_to_user_distance 2.00 3.00 -> (2.00, 3.00)";

    make "move_to" (fun c -> move_to c 4.05 2.957) "move_to 4.05 2.96";
    make "invalid rel_move_to" (catch NO_CURRENT_POINT (fun c -> rel_move_to c 4.05 2.957)) "rel_move_to 4.05 2.96 -> raise (%s.Error(NO_CURRENT_POINT))" backend_name;
    make_n "rel_move_to" [(fun c -> move_to c 1. 2.); fun c -> rel_move_to c 3. 4.] ["move_to 1.00 2.00"; "rel_move_to 3.00 4.00"];
    make "line_to" (fun c -> line_to c 4.05 2.957) "line_to 4.05 2.96";
    make "invalid rel_line_to" (catch NO_CURRENT_POINT (fun c -> rel_line_to c 4.05 2.957)) "rel_line_to 4.05 2.96 -> raise (%s.Error(NO_CURRENT_POINT))" backend_name;
    make_n "rel_line_to" [(fun c -> move_to c 1. 2.); fun c -> rel_line_to c 3. 4.] ["move_to 1.00 2.00"; "rel_line_to 3.00 4.00"];
    make "curve_to" (fun c -> curve_to c 1. 2. 3. 4. 5. 6.) "curve_to 1.00 2.00 3.00 4.00 5.00 6.00";
    make "invalid rel_curve_to" (catch NO_CURRENT_POINT (fun c -> rel_curve_to c 1. 2. 3. 4. 5. 6.)) "rel_curve_to 1.00 2.00 3.00 4.00 5.00 6.00 -> raise (%s.Error(NO_CURRENT_POINT))" backend_name;
    make_n "rel_curve_to" [(fun c -> move_to c 1. 2.); fun c -> rel_curve_to c 1. 2. 3. 4. 5. 6.] ["move_to 1.00 2.00"; "rel_curve_to 1.00 2.00 3.00 4.00 5.00 6.00"];
    make "rectangle" (fun c -> rectangle c 2. 3. ~w:4. ~h:5.) "rectangle 2.00 3.00 ~w:4.00 ~h:5.00";
    make "arc" (fun c -> arc c 1. 2. ~r:3. ~a1:4. ~a2:5.) "arc 1.00 2.00 ~r:3.00 ~a1:4.00 ~a2:5.00";
    make "arc_negative" (fun c -> arc_negative c 1. 2. ~r:3. ~a1:4. ~a2:5.) "arc_negative 1.00 2.00 ~r:3.00 ~a1:4.00 ~a2:5.00";
    make "Path.close" Path.close "Path.close";
    make "Path.clear" Path.clear "Path.clear";
    make "Path.get_current_point" Path.get_current_point "Path.get_current_point -> (0.00, 0.00)";
    make_n "move_to, Path.get_current_point" [(fun c -> move_to c 1. 2.); ignore % Path.get_current_point] ["move_to 1.00 2.00"; "Path.get_current_point -> (1.00, 2.00)"];

    make "stroke" stroke "stroke";
    make "stroke_preserve" stroke_preserve "stroke_preserve";
    make "fill" fill "fill";
    make "fill_preserve" fill_preserve "fill_preserve";
    make "clip" clip "clip";
    make "clip_preserve" clip_preserve "clip_preserve";
    make "paint" paint "paint";
    make "paint with alpha" (paint ~alpha:0.5) "paint ~alpha:0.50";

    make "set_line_width" (fun c -> set_line_width c 3.) "set_line_width 3.00";
    make "get_line_width" get_line_width "get_line_width -> 2.00";
    make "set_dash" (fun c -> set_dash c [|2.; 3.|]) "set_dash [|2.00; 3.00|]";
    make "set_dash with ofs" (fun c -> set_dash c ~ofs:2. [|3.; 4.|]) "set_dash ~ofs:2.00 [|3.00; 4.00|]";
    make "get_dash" get_dash "get_dash -> ([||], 0.00)";
    make "set_fill_rule" (fun c -> set_fill_rule c EVEN_ODD) "set_fill_rule EVEN_ODD";
    make "get_fill_rule" get_fill_rule "get_fill_rule -> WINDING";
    make "set_line_cap ROUND" (fun c -> set_line_cap c ROUND) "set_line_cap ROUND";
    make "set_line_cap SQUARE" (fun c -> set_line_cap c SQUARE) "set_line_cap SQUARE";
    make "get_line_cap" get_line_cap "get_line_cap -> BUTT";
    make "set_line_join JOIN_ROUND" (fun c -> set_line_join c JOIN_ROUND) "set_line_join JOIN_ROUND";
    make "set_line_join JOIN_BEVEL" (fun c -> set_line_join c JOIN_BEVEL) "set_line_join JOIN_BEVEL";
    make "get_line_join" get_line_join "get_line_join -> JOIN_MITER";
    make "set_miter_limit" (fun c -> set_miter_limit c 3.) "set_miter_limit 3.00";
    make "get_miter_limit" get_miter_limit "get_miter_limit -> 10.00";
    make "set_operator IN" (fun c -> set_operator c IN) "set_operator IN";
    make "set_operator OUT" (fun c -> set_operator c OUT) "set_operator OUT";
    make "set_operator ATOP" (fun c -> set_operator c ATOP) "set_operator ATOP";
    make "set_operator DEST_OVER" (fun c -> set_operator c DEST_OVER) "set_operator DEST_OVER";
    make "set_operator DEST_IN" (fun c -> set_operator c DEST_IN) "set_operator DEST_IN";
    make "set_operator DEST_OUT" (fun c -> set_operator c DEST_OUT) "set_operator DEST_OUT";
    make "set_operator DEST_ATOP" (fun c -> set_operator c DEST_ATOP) "set_operator DEST_ATOP";
    make "set_operator XOR" (fun c -> set_operator c XOR) "set_operator XOR";
    make "set_operator ADD" (fun c -> set_operator c ADD) "set_operator ADD";
  ] @ (if N.backend = `Node || N.backend = `Browser then [] else [ (*BISECT-IGNORE*) (* Test code *)
    make "set_operator DEST" (fun c -> set_operator c DEST) "set_operator DEST";
    make "set_operator CLEAR" (fun c -> set_operator c CLEAR) "set_operator CLEAR";
    make "set_operator SOURCE" (fun c -> set_operator c SOURCE) "set_operator SOURCE";
    make "set_operator SATURATE" (fun c -> set_operator c SATURATE) "set_operator SATURATE";
  ]) @ [
    make "get_operator" get_operator "get_operator -> OVER";

    make "set_source_rgb" (fun c -> set_source_rgb c 0.5 0.6 0.7) "set_source_rgb 0.50 0.60 0.70";
    make "set_source_rgba" (fun c -> set_source_rgba c 0.5 0.6 0.7 0.8) "set_source_rgba 0.50 0.60 0.70 0.80";
    make "set_source Rgba" (fun c -> set_source c (Pattern.create_rgb 0.5 0.6 0.7)) "set_source (Rgba {r=0.50; g=0.60; b=0.70; a=1.00})";
    make
      "set_source LinearGradient"
      (fun c ->
        let p = Pattern.create_linear ~x0:1. ~y0:2. ~x1:3. ~y1:4. in
        Pattern.add_color_stop_rgb p 0.1 0.2 0.3;
        set_source c p
      )
      "set_source (LinearGradient {x0=1.00; y0=2.00; x1=3.00; y1=4.00; stop_points=[{position=0.00; r=0.10; g=0.20; b=0.30; a=1.00}]})"
    ;
    make "set_source RadialGradient" (fun c -> set_source c (Pattern.create_radial ~x0:1. ~y0:2. ~r0:5. ~x1:3. ~y1:4. ~r1:6.)) "set_source (RadialGradient {x0=1.00; y0=2.00; r0=5.00; x1=3.00; y1=4.00; r16.00; stop_points=[]})";
    make "get_source" get_source "get_source -> (Rgba {r=0.00; g=0.00; b=0.00; a=1.00})";

    make "set_font_size" (fun c -> set_font_size c 3.) "set_font_size 3.00";
    make "select_font_face" (fun c -> select_font_face c "foo-bar") "select_font_face \"foo-bar\"";
    make "select_font_face Upright" (fun c -> select_font_face c ~slant:Upright "foo-bar") "select_font_face ~slant:Upright \"foo-bar\"";
    make "select_font_face Oblique" (fun c -> select_font_face c ~slant:Oblique "foo-bar") "select_font_face ~slant:Oblique \"foo-bar\"";
    make "select_font_face Normal" (fun c -> select_font_face c ~weight:Normal "foo-bar") "select_font_face ~weight:Normal \"foo-bar\"";
    make "select_font_face Italic Bold" (fun c -> select_font_face c ~slant:Italic ~weight:Bold "foo-bar") "select_font_face ~slant:Italic ~weight:Bold \"foo-bar\"";
    make "show_text" (fun c -> show_text c "flibidiboo") "show_text \"flibidiboo\"";
    make' "text_extents"
      (fun c -> text_extents c "abcd")
      (Frmt.with_scan_result
        "text_extents \"abcd\" -> {x_bearing=%f; y_bearing=%f; width=%f; height=%f; x_advance=%f; y_advance=%f}"
        ~f:(fun _ _ width _ _ _ -> check_float_in ~low:10. ~high:50. width)
      )
    ;
    make' "font_extents"
      font_extents
      (Frmt.with_scan_result
        "font_extents -> {ascent=%f; descent=%f; baseline=%f; max_x_advance=%f; max_y_advance=%f}"
        ~f:(fun ascent descent _ _ _ ->
          check_float_in ~low:5. ~high:15. ascent;
          check_float_in ~low:1. ~high:7. descent
        )
      )
    ;
  ])
end
