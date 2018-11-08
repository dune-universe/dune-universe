(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr
open Tst

module Make(C: CairoMock.S)(N: sig
  val name: string
  val create: unit -> C.context
  val backend: [`Cairo | `Node | `Browser | `CairoMock ]
end) = struct
  open C

  let backend_name = match N.backend with
    | `Cairo -> "Cairo"
    | `Node | `Browser -> "JsOfOCairo" (*BISECT-IGNORE*) (* Test code *)
    | `CairoMock -> "CairoMock"

  let check_matrix =
    let equal {xx; xy; yx; yy; x0; y0} m =
      (
        Fl.approx_equal m.xx xx
        && Fl.approx_equal m.xy xy
        && Fl.approx_equal m.yx yx
        && Fl.approx_equal m.yy yy
        && Fl.approx_equal m.x0 x0
        && Fl.approx_equal m.y0 y0
      )
    and repr {xx; xy; yx; yy; x0; y0} =
      Frmt.apply "{xx=%f; xy=%f; yx=%f; yy=%f; x0=%f; y0=%f}" xx xy yx yy x0 y0 (*BISECT-IGNORE*) (* Test code *)
    in
    check ~equal ~repr

  let check_coords ?precision =
    let equal (x0, y0) (x1, y1) =
      (Fl.approx_equal ?precision x0 x1 && Fl.approx_equal ?precision y0 y1)
    and repr (x, y) =
      Frmt.apply "(%f, %f)" x y (*BISECT-IGNORE*) (* Test code *)
    in
    check ~equal ~repr

  let check_float_tuple_4 =
    let repr (a, b, c, d) =
      Frmt.apply "(%f, %f, %f, %f)" a b c d (*BISECT-IGNORE*) (* Test code *)
    in
    check_poly ~repr

  let check_float_tuple_5 =
    let repr (a, b, c, d, e) =
      Frmt.apply "(%f, %f, %f, %f, %f)" a b c d e (*BISECT-IGNORE*) (* Test code *)
    in
    check_poly ~repr

  let check_float_tuple_6 =
    let repr (a, b, c, d, e, f) =
      Frmt.apply "(%f, %f, %f, %f, %f, %f)" a b c d e f (*BISECT-IGNORE*) (* Test code *)
    in
    check_poly ~repr

  let test = ~:: "Universal tests on %s" N.name [
    "saved-and-restored settings" >:: (
      let make name setter getter check initial_value other_value other_values =
        name >: (lazy (
          let ctx = N.create () in
          check ~expected:initial_value (getter ctx);
          (initial_value::other_value::other_values)
          |> Li.iter ~f:(fun value ->
            setter ctx value;
            check ~expected:value (getter ctx);
            save ctx;
            check ~expected:value (getter ctx);
            setter ctx other_value;
            check ~expected:other_value (getter ctx);
            restore ctx;
            check ~expected:value (getter ctx);
          )
        ))
      in
      [
        make "line_width" set_line_width get_line_width check_float_exact 2. 1. [4.];
        make "miter_limit" set_miter_limit get_miter_limit check_float_exact 10. 5. [20.];
        (let repr = function
          (*BISECT-IGNORE-BEGIN*)
          | WINDING -> "WINDING"
          | EVEN_ODD -> "EVEN_ODD"
          (*BISECT-IGNORE-END*)
        in
        make "fill_rule" set_fill_rule get_fill_rule (check_poly ~repr) WINDING EVEN_ODD []);
        (let repr = function
          (*BISECT-IGNORE-BEGIN*)
          | BUTT -> "BUTT"
          | ROUND -> "ROUND"
          | SQUARE -> "SQUARE"
          (*BISECT-IGNORE-END*)
        in
        make "line_cap" set_line_cap get_line_cap (check_poly ~repr) BUTT ROUND [SQUARE]);
        (let repr = function
          (*BISECT-IGNORE-BEGIN*)
          | JOIN_MITER -> "JOIN_MITER"
          | JOIN_ROUND -> "JOIN_ROUND"
          | JOIN_BEVEL -> "JOIN_BEVEL"
          (*BISECT-IGNORE-END*)
        in
        make "line_join" set_line_join get_line_join (check_poly ~repr) JOIN_MITER JOIN_ROUND [JOIN_BEVEL]);
        (let repr = function
          (*BISECT-IGNORE-BEGIN*)
          | CLEAR -> "CLEAR"
          | SOURCE -> "SOURCE"
          | OVER -> "OVER"
          | IN -> "IN"
          | OUT -> "OUT"
          | ATOP -> "ATOP"
          | DEST -> "DEST"
          | DEST_OVER -> "DEST_OVER"
          | DEST_IN -> "DEST_IN"
          | DEST_OUT -> "DEST_OUT"
          | DEST_ATOP -> "DEST_ATOP"
          | XOR -> "XOR"
          | ADD -> "ADD"
          | SATURATE -> "SATURATE"
          (*BISECT-IGNORE-END*)
        in
        make "operator" set_operator get_operator (check_poly ~repr) OVER IN ([OUT; ATOP; DEST_OVER; DEST_IN; DEST_OUT; DEST_ATOP; XOR; ADD] @ (if N.backend = `Node || N.backend = `Browser then [] else [CLEAR; SOURCE; DEST; SATURATE]))); (*BISECT-IGNORE*) (* Test code *)
        "dash" >:: [
          (let repr dashes =
            (*BISECT-IGNORE-BEGIN*)
            dashes
            |> Li.of_array
            |> Li.map ~f:Fl.repr
            |> StrLi.join ~sep:"; "
            |> Frmt.apply "[|%s|]"
            (*BISECT-IGNORE-END*)
          in
          make "dashes" (fun c dashes -> set_dash c dashes) (fun c -> get_dash c |> Tu2.get_0) (check_poly ~repr) [||] [|1.; 2.|] ([[|3.; 4.; 5.; 6.|]; [|7.; 8.; 9.; 10.; 11.; 12.|]] @ (if N.backend = `Node || N.backend = `Browser then [] else [[|3.|]; [|4.; 5.; 6.|]]))); (*BISECT-IGNORE*) (* Test code *)
          make "offset" (fun c ofs -> set_dash c ~ofs [|10.; 10.|]) (fun c -> get_dash c |> Tu2.get_1) check_float_exact 0. 2. [3.];
        ];
        make
          "source"
          (fun c (r, g, b, a) -> set_source_rgba c r g b a)
          (fun c -> get_source c |> Pattern.get_rgba)
          (check_float_tuple_4)
          (0., 0., 0., 1.)
          (1., 0., 0., 0.5)
          [(0., 0., 1., 0.7)]
        ;
      ]
    );
    "status_to_string" >:: (
      let make name status expected =
        name >: (lazy (
          check_string ~expected (status_to_string status)
        ))
      in
      [
        make "INVALID_RESTORE" INVALID_RESTORE "cairo_restore() without matching cairo_save()";
        make "INVALID_POP_GROUP" INVALID_POP_GROUP "no saved group to pop, i.e. cairo_pop_group() without matching cairo_push_group()";
        make "NO_CURRENT_POINT" NO_CURRENT_POINT "no current point defined";
        make "INVALID_MATRIX" INVALID_MATRIX "invalid matrix (not invertible)";
        make "INVALID_STATUS" INVALID_STATUS "invalid value for an input cairo_status_t";
        make "NULL_POINTER" NULL_POINTER "NULL pointer";
        make "INVALID_STRING" INVALID_STRING "input string not valid UTF-8";
        make "INVALID_PATH_DATA" INVALID_PATH_DATA "input path data not valid";
        make "READ_ERROR" READ_ERROR "error while reading from input stream";
        make "WRITE_ERROR" WRITE_ERROR "error while writing to output stream";
        make "SURFACE_FINISHED" SURFACE_FINISHED "the target surface has been finished";
        make "SURFACE_TYPE_MISMATCH" SURFACE_TYPE_MISMATCH "the surface type is not appropriate for the operation";
        make "PATTERN_TYPE_MISMATCH" PATTERN_TYPE_MISMATCH "the pattern type is not appropriate for the operation";
        make "INVALID_CONTENT" INVALID_CONTENT "invalid value for an input cairo_content_t";
        make "INVALID_FORMAT" INVALID_FORMAT "invalid value for an input cairo_format_t";
        make "INVALID_VISUAL" INVALID_VISUAL "invalid value for an input Visual*";
        make "FILE_NOT_FOUND" FILE_NOT_FOUND "file not found";
        make "INVALID_DASH" INVALID_DASH "invalid value for a dash setting";
        make "INVALID_DSC_COMMENT" INVALID_DSC_COMMENT "invalid value for a DSC comment";
        make "INVALID_INDEX" INVALID_INDEX "invalid index passed to getter";
        make "CLIP_NOT_REPRESENTABLE" CLIP_NOT_REPRESENTABLE "clip region not representable in desired format";
        make "TEMP_FILE_ERROR" TEMP_FILE_ERROR "error creating or writing to a temporary file";
        make "INVALID_STRIDE" INVALID_STRIDE "invalid value for stride";
        make "FONT_TYPE_MISMATCH" FONT_TYPE_MISMATCH "the font type is not appropriate for the operation";
        make "USER_FONT_IMMUTABLE" USER_FONT_IMMUTABLE "the user-font is immutable";
        make "USER_FONT_ERROR" USER_FONT_ERROR "error occurred in a user-font callback function";
        make "NEGATIVE_COUNT" NEGATIVE_COUNT "negative number used where it is not allowed";
        make "INVALID_CLUSTERS" INVALID_CLUSTERS "input clusters do not represent the accompanying text and glyph arrays";
        make "INVALID_SLANT" INVALID_SLANT "invalid value for an input cairo_font_slant_t";
        make "INVALID_WEIGHT" INVALID_WEIGHT "invalid value for an input cairo_font_weight_t";
        make "INVALID_SIZE" INVALID_SIZE "invalid value (typically too big) for the size of the input (surface, pattern, etc.)";
        make "USER_FONT_NOT_IMPLEMENTED" USER_FONT_NOT_IMPLEMENTED "user-font method not implemented";
        make "DEVICE_TYPE_MISMATCH" DEVICE_TYPE_MISMATCH "the device type is not appropriate for the operation";
        make "DEVICE_ERROR" DEVICE_ERROR "an operation to the device caused an unspecified error";
        make "INVALID_MESH_CONSTRUCTION" INVALID_MESH_CONSTRUCTION "invalid operation during mesh pattern construction";
        make "DEVICE_FINISHED" DEVICE_FINISHED "the target device has been finished";
        make "JBIG2_GLOBAL_MISSING" JBIG2_GLOBAL_MISSING "CAIRO_MIME_TYPE_JBIG2_GLOBAL_ID used but no CAIRO_MIME_TYPE_JBIG2_GLOBAL data provided";
      ]
    );
    "exceptions" >:: [
      "Cairo Error" >: (lazy (
        check_string ~expected:(Frmt.apply "%s.Error(INVALID_RESTORE)" backend_name) (Exn.to_string (Error INVALID_RESTORE));
      ));
      "Not Cairo" >: (lazy (
        (* This test covers the None case in the registered printer *)
        check_string ~expected:"Not_found" (Exn.to_string Not_found);
      ))
    ];
    "transformations" >:: (
      let identity = {xx=1.; xy=0.; yx=0.; yy=1.; x0=0.; y0=0.} in
      let make name f expected =
        name >: (lazy (
          let ctx = N.create () in
          check_matrix ~expected:identity (get_matrix ctx);
          f ctx;
          check_matrix ~expected (get_matrix ctx);
          save ctx;
          check_matrix ~expected (get_matrix ctx);
          identity_matrix ctx;
          check_matrix ~expected:identity (get_matrix ctx);
          restore ctx;
          check_matrix ~expected (get_matrix ctx);
        ))
      in
      [
        make "translate" (fun c -> translate c 2. 3.) {xx=1.; xy=0.; yx=0.; yy=1.; x0=2.; y0=3.};
        make "scale" (fun c -> scale c 2. 3.) {xx=2.; xy=0.; yx=0.; yy=3.; x0=0.; y0=0.};
        make "rotate" (fun c -> rotate c (Fl.pi /. 4.)) (let s = Fl.sqrt(2.) /. 2. in {xx=s; xy=(-.s); yx=s; yy=s; x0=0.; y0=0.});
        make "set_matrix" (fun c -> set_matrix c {xx=1.; xy=2.; yx=3.; yy=4.; x0=5.; y0=6.}) {xx=1.; xy=2.; yx=3.; yy=4.; x0=5.; y0=6.};
        make "transform" (fun c -> scale c 2. 3.; transform c {xx=1.; xy=2.; yx=3.; yy=4.; x0=5.; y0=6.}) {xx=2.; xy=4.; yx=9.; yy=12.; x0=10.; y0=18.};
      ]
    );
    "matrix" >:: [
      "init_identity" >: (lazy (
        check_matrix ~expected:{xx=1.; xy=0.; yx=0.; yy=1.; x0=0.; y0=0.} (Matrix.init_identity ())
      ));
      "init_translate" >: (lazy (
        check_matrix ~expected:{xx=1.; xy=0.; yx=0.; yy=1.; x0=2.; y0=3.} (Matrix.init_translate 2. 3.)
      ));
      "init_scale" >: (lazy (
        check_matrix ~expected:{xx=2.; xy=0.; yx=0.; yy=3.; x0=0.; y0=0.} (Matrix.init_scale 2. 3.)
      ));
      "init_rotate" >: (lazy (
        check_matrix ~expected:{xx=(Fl.sqrt 3. /. 2.); xy=(-0.5); yx=0.5; yy=(Fl.sqrt 3. /. 2.); x0=0.; y0=0.} (Matrix.init_rotate (Fl.pi /. 6.))
      ));
      "invert" >:: [
        "non invertible" >: (lazy (
          expect_exception ~expected:(Error INVALID_MATRIX) (lazy (
            let m = {xx=1.; xy=2.; yx=1.; yy=2.; x0=3.; y0=4.} in
            Matrix.invert m
          ))
        ));
        "invertible" >: (lazy (
          let m = {xx=1.; xy=2.; yx=3.; yy=4.; x0=5.; y0=6.} in
          Matrix.invert m;
          check_matrix ~expected:{xx=(-2.); xy=1.; yx=1.5; yy=(-0.5); x0=4.; y0=(-4.5)} m
        ));
      ];
      "scale" >: (lazy (
        let m = {xx=1.; xy=2.; yx=3.; yy=4.; x0=5.; y0=6.} in
        Matrix.scale m 7. 8.;
        check_matrix ~expected:{xx=7.; xy=16.; yx=21.; yy=32.; x0=5.; y0=6.} m
      ));
      "translate" >: (lazy (
        let m = {xx=1.; xy=2.; yx=3.; yy=4.; x0=5.; y0=6.} in
        Matrix.translate m 7. 8.;
        check_matrix ~expected:{xx=1.; xy=2.; yx=3.; yy=4.; x0=28.; y0=59.} m
      ));
      "rotate" >: (lazy (
        let m = {xx=1.; xy=2.; yx=3.; yy=4.; x0=5.; y0=6.} in
        Matrix.rotate m (Fl.pi /. 6.);
        let s = Fl.sqrt 3. /. 2. in
        check_matrix ~expected:{xx=(s +. 1.); xy=(2. *. s -. 0.5); yx=(3. *. s +. 2.); yy=(4. *. s -. 1.5); x0=5.; y0=6.} m
      ));
    ];
    "coordinates transformation" >: (lazy (
      let ctx = N.create ()
      and m = {xx=1.; xy=2.; yx=3.; yy=4.; x0=5.; y0=6.} in
      set_matrix ctx m;
      check_coords ~expected:(-2., 2.) (device_to_user ctx 7. 8.);
      check_coords ~expected:(-6., 6.5) (device_to_user_distance ctx 7. 8.);
      check_coords ~expected:(28., 59.) (user_to_device ctx 7. 8.);
      check_coords ~expected:(23., 53.) (user_to_device_distance ctx 7. 8.);
      check_coords ~expected:(28., 59.) (Matrix.transform_point m 7. 8.);
      check_coords ~expected:(23., 53.) (Matrix.transform_distance m ~dx:7. ~dy:8.);
    ));
    "invalid restore" >: (lazy (
      let ctx = N.create () in
      expect_exception ~expected:(Error INVALID_RESTORE) (lazy (restore ctx))
    ));
    "no current point" >:: (
      let make name f =
        name >: (lazy (
          let ctx = N.create () in
          expect_exception ~expected:(Error NO_CURRENT_POINT) (lazy (f ctx))
        ))
      in
      [
        make "rel_move_to" (fun c -> rel_move_to c 1. 2.);
        make "rel_line_to" (fun c -> rel_line_to c 1. 2.);
        make "rel_curve_to" (fun c -> rel_curve_to c 1. 2. 3. 4. 5. 6.);
        make "Path.clear" (fun c -> move_to c 1. 2.; Path.clear c; rel_move_to c 3. 4.);
        make "stroke" (fun c -> move_to c 1. 2.; line_to c 3. 4.; stroke c; rel_move_to c 3. 4.);
        make "fill" (fun c -> move_to c 1. 2.; line_to c 3. 4.; fill c; rel_move_to c 3. 4.);
        make "clip" (fun c -> move_to c 1. 2.; line_to c 3. 4.; clip c; rel_move_to c 3. 4.);
      ]
    );
    "current point" >:: (
      let make' name f check =
        name >: (lazy (
          let ctx = N.create () in
          f ctx;
          check (Path.get_current_point ctx)
        ))
      in
      let make name f expected =
        (* Low precision because Cairo makes approximations on arcs *)
        make' name f (check_coords ~precision:1e-3 ~expected)
      in
      [
        make "no-op" (fun _ -> ()) (0., 0.);
        make "move_to" (fun c -> move_to c 1. 2.) (1., 2.);
        make "paint" paint (0., 0.);
        make "move_to, paint" (fun c -> move_to c 1. 2.; paint c) (1., 2.);
        make "save, move_to, restore"
          (fun c ->
            save c;
            move_to c 1. 2.;
            restore c;
          )
          (1., 2.);
        make "save, scale, move_to, restore"
          (fun c ->
            save c;
            scale c 3. 4.;
            move_to c 1. 2.;
            restore c;
          )
          (3., 8.);
        make "rel_move_to" (fun c -> move_to c 1. 2.; rel_move_to c 3. 4.) (4., 6.);
        make "line_to" (fun c -> line_to c 1. 2.) (1., 2.);
        make "rel_line_to" (fun c -> move_to c 1. 2.; rel_line_to c 3. 4.) (4., 6.);
        make "rectangle" (fun c -> rectangle c 1. 2. ~w:3. ~h:4.) (1., 2.);
        "arc" >:: [
          make "0" (fun c -> arc c 1. 2. ~r:3. ~a1:(-1.) ~a2:0.) (4., 2.);
          make "pi / 6" (fun c -> arc c 1. 2. ~r:3. ~a1:(-1.) ~a2:(Fl.pi /. 6.)) (1. +. 3. *. Fl.sqrt(3.) /. 2., 2. +. 3. *. 0.5);
          make "pi / 4" (fun c -> arc c 1. 2. ~r:3. ~a1:0. ~a2:(Fl.pi /. 4.)) (1. +. 3. *. Fl.sqrt(2.) /. 2., 2. +. 3. *. Fl.sqrt(2.) /. 2.);
          make "pi / 2" (fun c -> arc c 1. 2. ~r:3. ~a1:(-1.) ~a2:(Fl.pi /. 2.)) (1., 5.);
          make "3 pi" (fun c -> arc c 1. 2. ~r:3. ~a1:(Fl.pi /. 2.) ~a2:(3. *. Fl.pi)) (-2., 2.);
        ];
        "arc_negative" >:: [
          make "0" (fun c -> arc_negative c 1. 2. ~r:3. ~a1:(-1.) ~a2:0.) (4., 2.);
          make "pi / 6" (fun c -> arc_negative c 1. 2. ~r:3. ~a1:(-1.) ~a2:(Fl.pi /. 6.)) (1. +. 3. *. Fl.sqrt(3.) /. 2., 2. +. 3. *. 0.5);
          make "pi / 4" (fun c -> arc_negative c 1. 2. ~r:3. ~a1:0. ~a2:(Fl.pi /. 4.)) (1. +. 3. *. Fl.sqrt(2.) /. 2., 2. +. 3. *. Fl.sqrt(2.) /. 2.);
          make "pi / 2" (fun c -> arc_negative c 1. 2. ~r:3. ~a1:(-1.) ~a2:(Fl.pi /. 2.)) (1., 5.);
        ];
        make "curve_to" (fun c -> curve_to c 1. 2. 3. 4. 5. 6.) (5., 6.);
        make "rel_line_to" (fun c -> move_to c 1. 2.; rel_curve_to c 1. 2. 3. 4. 5. 6.) (6., 8.);
        make "Path.close" (fun c -> move_to c 1. 2.; line_to c 3. 4.; line_to c 5. 6.; Path.close c) (1., 2.);
        make "stroke_preserve" (fun c -> move_to c 1. 2.; line_to c 3. 4.; stroke_preserve c) (3., 4.);
        make "fill_preserve" (fun c -> move_to c 1. 2.; line_to c 3. 4.; fill_preserve c) (3., 4.);
        make "clip_preserve" (fun c -> move_to c 1. 2.; line_to c 3. 4.; clip_preserve c) (3., 4.);
        make' "show_text" (fun c -> move_to c 1. 2.; show_text c "Hello") (fun (x, y) ->
          check_float_in ~low:10. ~high:50. x;
          check_float ~expected:2. y
        );
      ]
    );
    "patterns" >:: Pattern.[
      "create_rgb, get_rgba" >: (lazy (
        let p = create_rgb 0.1 0.2 0.3 in
        check_float_tuple_4 ~expected:(0.1, 0.2, 0.3, 1.) (get_rgba p)
      ));
      "set_source_rgb, get_rgba" >: (lazy (
        let ctx = N.create () in
        set_source_rgb ctx 0.1 0.2 0.3;
        let p = get_source ctx in
        check_float_tuple_4 ~expected:(0.1, 0.2, 0.3, 1.) (get_rgba p)
      ));
      "create_rgba, get_rgba" >: (lazy (
        let p = create_rgba 0.1 0.2 0.3 0.4 in
        check_float_tuple_4 ~expected:(0.1, 0.2, 0.3, 0.4) (get_rgba p)
      ));
      "set_source_rgba, get_rgba" >: (lazy (
        let ctx = N.create () in
        set_source_rgba ctx 0.1 0.2 0.3 0.4;
        let p = get_source ctx in
        check_float_tuple_4 ~expected:(0.1, 0.2, 0.3, 0.4) (get_rgba p)
      ));
      "create_linear, get_linear_points" >: (lazy (
        let p = create_linear ~x0:1. ~y0:2. ~x1:3. ~y1:4. in
        check_float_tuple_4 ~expected:(1., 2., 3., 4.) (get_linear_points p)
      ));
      "create_radial, get_radial_circles" >: (lazy (
        let p = create_radial ~x0:1. ~y0:2. ~r0:3. ~x1:4. ~y1:5. ~r1:6. in
        check_float_tuple_6 ~expected:(1., 2., 3., 4., 5., 6.) (get_radial_circles p)
      ));
      "create_linear, add_color_stop_rgba, get_color_stop_count, get_color_stop_rgba" >: (lazy (
        let p = create_linear ~x0:1. ~y0:2. ~x1:3. ~y1:4. in
        add_color_stop_rgba p ~ofs:0.1 0.2 0.3 0.4 0.5;
        check_int ~expected:1 (get_color_stop_count p);
        check_float_tuple_5 ~expected:(0.1, 0.2, 0.3, 0.4, 0.5) (get_color_stop_rgba p ~idx:0)
      ));
      "create_radial, add_color_stop_rgb, get_color_stop_count, get_color_stop_rgba" >: (lazy (
        let p = create_radial ~x0:1. ~y0:2. ~r0:3. ~x1:4. ~y1:5. ~r1:6. in
        add_color_stop_rgb p 0.2 0.3 0.4;
        check_int ~expected:1 (get_color_stop_count p);
        check_float_tuple_5 ~expected:(0., 0.2, 0.3, 0.4, 1.) (get_color_stop_rgba p ~idx:0)
      ));
      "multiple color stops" >: (lazy (
        let p = create_linear ~x0:1. ~y0:2. ~x1:3. ~y1:4. in
        add_color_stop_rgb p ~ofs:0.2 0.2 0.2 0.2;
        add_color_stop_rgb p ~ofs:0.1 0.1 0.1 0.1;
        add_color_stop_rgb p ~ofs:0.2 0.21 0.21 0.21;
        add_color_stop_rgb p ~ofs:0.3 0.3 0.3 0.3;
        check_int ~expected:4 (get_color_stop_count p);
        check_float_tuple_5 ~expected:(0.1, 0.1, 0.1, 0.1, 1.) (get_color_stop_rgba p ~idx:0);
        check_float_tuple_5 ~expected:(0.2, 0.2, 0.2, 0.2, 1.) (get_color_stop_rgba p ~idx:1);
        check_float_tuple_5 ~expected:(0.2, 0.21, 0.21, 0.21, 1.) (get_color_stop_rgba p ~idx:2);
        check_float_tuple_5 ~expected:(0.3, 0.3, 0.3, 0.3, 1.) (get_color_stop_rgba p ~idx:3)
      ));
      "create_rgb, add_color_stop_rgb" >: (lazy (
        let ctx = N.create () in
        set_source ctx (create_rgb 0.1 0.2 0.3);
        let p = get_source ctx in
        (* This is a bit weird: add_color_stop_rgb returns silently, but puts the pattern in a state where all getters fail. *)
        add_color_stop_rgb p ~ofs:0.1 0.1 0.1 0.1;
        add_color_stop_rgb p ~ofs:0.2 0.2 0.2 0.2;
        expect_exception ~expected:(Error PATTERN_TYPE_MISMATCH) (lazy (get_rgba p));
        expect_exception ~expected:(Error PATTERN_TYPE_MISMATCH) (lazy (get_color_stop_count p));
        expect_exception ~expected:(Error PATTERN_TYPE_MISMATCH) (lazy (get_linear_points p));
        expect_exception ~expected:(Error PATTERN_TYPE_MISMATCH) (lazy (get_radial_circles p));
        expect_exception ~expected:(Error PATTERN_TYPE_MISMATCH) (lazy (get_color_stop_rgba ~idx:0 p));
      ));
      "mismatch" >:: (
        let make name p f =
          name >: (lazy (
            let ctx = N.create () in
            (* Forget type *)
            set_source ctx p;
            let p = get_source ctx in
            expect_exception ~expected:(Error PATTERN_TYPE_MISMATCH) (lazy (f p))
          ))
        in
        [
          make "create_rgb, get_linear_points" (create_rgb 0.1 0.2 0.3) get_linear_points;
          make "create_rgb, get_radial_circles" (create_rgb 0.1 0.2 0.3) get_radial_circles;
          make "create_rgb, get_color_stop_count" (create_rgb 0.1 0.2 0.3) get_color_stop_count;
          make "create_rgb, get_color_stop_rgba" (create_rgb 0.1 0.2 0.3) (get_color_stop_rgba ~idx:0);
          make "create_linear, get_rgba" (create_linear ~x0:1. ~y0:2. ~x1:3. ~y1:4.) get_rgba;
          make "create_linear, get_radial_circles" (create_linear ~x0:1. ~y0:2. ~x1:3. ~y1:4.) get_radial_circles;
          make "create_radial, get_rgba" (create_radial ~x0:1. ~y0:2. ~r0:3. ~x1:4. ~y1:5. ~r1:6.) get_rgba;
          make "create_radial, get_linear_points" (create_radial ~x0:1. ~y0:2. ~r0:3. ~x1:4. ~y1:5. ~r1:6.) get_linear_points;
        ]
      );
    ];
    (* Functions about text and fonts are not covered by these tests because they don't behave the same accross all implementations. *)
  ]
end
