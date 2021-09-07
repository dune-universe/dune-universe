(* Small interface to gnuplot *)

let unit_interval x = x >= 0.0 && x <= 1.

type color = { r : float; g : float; b : float }

module Color = struct
  type t = color

  let rgb r g b =
    if not (unit_interval r && unit_interval g && unit_interval b) then
      invalid_arg "rgb" ;
    { r; g; b }

  let red = { r = 1.0; g = 0.0; b = 0.0 }

  let green = { r = 0.0; g = 1.0; b = 0.0 }

  let blue = { r = 0.0; g = 0.0; b = 1.0 }

  let black = { r = 0.0; g = 0.0; b = 0.0 }
end

type pointtype =
  | Dot
  | Plus
  | Cross
  | Star
  | Square
  | Box
  | Circle
  | Disk
  | Delta
  | Delta_solid

module Pointtype = struct
  type t = pointtype

  let square = Square

  let dot = Dot

  let circle = Circle

  let delta = Delta

  let delta_solid = Delta_solid

  let plus = Plus

  let cross = Cross

  let star = Star

  let box = Box

  let disk = Disk
end

type fill_style =
  | Empty
  | Solid of { density : float; transparent : bool }
  | Pattern of { code : int; transparent : bool }

type border_style = Noborder (* TODO *)

type fill =
  { fill_style : fill_style;
    border : border_style;
    fill_color : Color.t option
  }

module Fill = struct
  type t = fill

  let default = { fill_style = Empty; border = Noborder; fill_color = None }

  let set_empty fill = { fill with fill_style = Empty }

  let set_solid ~density ~transparent fill =
    if not (unit_interval density) then invalid_arg "set_solid" ;
    { fill with fill_style = Solid { density; transparent } }

  let set_pattern ~code ~transparent fill =
    { fill with fill_style = Pattern { code; transparent } }

  let set_color color fill = { fill with fill_color = Some color }
end

type style = { shape_type : shape_type; color : Color.t option }

and shape_type =
  | Point of { ptyp : Pointtype.t option; psize : float option }
  | Circle of { fill : Fill.t option; radius : float }

module Style = struct
  type t = style

  let default =
    { shape_type = Point { ptyp = None; psize = None }; color = None }

  let set_point ?ptyp ?psize style =
    { style with shape_type = Point { ptyp; psize } }

  let set_circle ?fill ~radius style =
    { style with shape_type = Circle { fill; radius } }

  let set_color color style = { style with color = Some color }
end

type r1 = { x : float } [@@unboxed]

type r2 = { x : float; y : float }

type r3 = { x : float; y : float; z : float }

type r4 = { w : float; x : float; y : float; z : float }

let r1 x = { x }

let r2 x y = { x; y }

let r3 x y z = { x; y; z }

let r4 w x y z = { w; x; y; z }

module Data = struct
  (* This could be refined later into something smarter (allowing to replot the same
     data without having to duplicate it in the script for instance) *)
  type 'a t = 'a Seq.t

  let of_seq s = s

  let of_array = Array.to_seq

  let of_list = List.to_seq
end

type histogram_options =
  { color : color option; bins : int option; binwidth : float option }

type 'data with_metadata =
  { data : 'data; style : style; legend : string option }

type _ axes =
  | Dim2_axes : { xaxis : string; yaxis : string } -> r2 axes
  | Dim3_axes : { xaxis : string; yaxis : string; zaxis : string } -> r3 axes

type _ spec =
  | Scatter :
      { data : 'dim Data.t with_metadata;
        error_bars : r2 Seq.t option
            (** Error bars for 2d point plots, always none for 3d plots *)
      }
      -> 'dim spec
  | Histogram :
      { data : r1 Seq.t; options : histogram_options; legend : string option }
      -> r2 spec
  | Line :
      { data : 'dim Data.t with_metadata;
        with_points : bool;
        error_bars : r2 Seq.t option
            (** Error bars for 2d line plots, always none for 3d plots *)
      }
      -> 'dim spec

(*
  - error bars: with scatter and with lines
  - box: only with lines
*)
type plot =
  | Plot :
      { axes : 'dim axes; plots : 'dim spec list; title : string option }
      -> plot

type plot_matrix = { plots : plot option array array; title : string }

module Scatter = struct
  let points_2d ~points ?(style = Style.default) ?legend ?error_bars () =
    Scatter { data = { data = points; style; legend }; error_bars }

  let points_3d ~points ?(style = Style.default) ?legend () =
    Scatter { data = { data = points; style; legend }; error_bars = None }
end

module Line = struct
  let compute_with_points style legend with_points =
    match (with_points, style.shape_type) with
    | (None, Point { ptyp = Some _; psize = _ }) -> true
    | (None, _) -> false
    | (Some false, Point { ptyp = Some _; psize = _ }) ->
        let legend = Option.value ~default:"none" legend in
        let msg =
          Printf.sprintf
            "line_2d: style inconsistent with with_points = false (legend = \
             \"%s\")"
            legend
        in
        invalid_arg msg
    | (Some false, _) -> false
    | (Some true, _) ->
        (* TODO: what does Gnuplot do with [with_points] and circle style? *)
        true

  let line_2d ~points ?(style = Style.default) ?legend ?with_points ?error_bars
      () =
    let with_points = compute_with_points style legend with_points in
    Line { data = { data = points; style; legend }; with_points; error_bars }

  let line_3d ~points ?(style = Style.default) ?legend ?with_points () =
    let with_points = compute_with_points style legend with_points in
    Line
      { data = { data = points; style; legend };
        with_points;
        error_bars = None
      }

  let to_2d (ys : r1 Seq.t) : r2 Seq.t =
    let c = ref 0.0 in
    Seq.map
      (fun ({ x } : r1) ->
        let y = !c in
        c := y +. 1. ;
        { x; y })
      ys

  let line ~(points : r1 Seq.t) ?style ?legend ?with_points ?error_bars () =
    let points = to_2d points in
    line_2d ~points ?style ?legend ?with_points ?error_bars ()
end

module Histogram = struct
  type options = histogram_options =
    { color : color option; bins : int option; binwidth : float option }

  let hist ~points ?color ?bins ?binwidth ?legend () =
    match (bins, binwidth) with
    | (None, None) ->
        let bins = Some 20 in
        Histogram { data = points; options = { color; bins; binwidth }; legend }
    | _ ->
        Histogram { data = points; options = { color; bins; binwidth }; legend }
end

module Axes = struct
  let _2 ~xaxis ~yaxis = Dim2_axes { xaxis; yaxis }

  let _3 ~xaxis ~yaxis ~zaxis = Dim3_axes { xaxis; yaxis; zaxis }
end

let plot2 ~xaxis ~yaxis ?title plots =
  Plot { axes = Axes._2 ~xaxis ~yaxis; plots; title }

let plot3 ~xaxis ~yaxis ~zaxis ?title plots =
  Plot { axes = Axes._3 ~xaxis ~yaxis ~zaxis; plots; title }

(* ------------------------- *)
(* Producing gnuplot scripts *)

let sf = Printf.sprintf

let concat l = String.concat "\n" l

module GP_data : sig
  type t = private string

  val _1d : data_name:string -> r1 Seq.t -> t

  val _2d : data_name:string -> r2 Seq.t -> t

  val _3d : data_name:string -> r3 Seq.t -> t

  val _4d : data_name:string -> r4 Seq.t -> t
end = struct
  type t = string

  (* TODO: optimize final concat *)

  let _1d ~data_name points =
    let acc = ref [] in
    Seq.iter (fun ({ x } : r1) -> acc := string_of_float x :: !acc) points ;
    concat ([sf "$%s << EOD" data_name] @ List.rev !acc @ [sf "EOD"])

  let _2d ~data_name points =
    let acc = ref [] in
    Seq.iter (fun ({ x; y } : r2) -> acc := sf "%f %f" x y :: !acc) points ;
    concat ([sf "$%s << EOD" data_name] @ List.rev !acc @ [sf "EOD"])

  let _3d ~data_name points =
    let acc = ref [] in
    Seq.iter
      (fun ({ x; y; z } : r3) -> acc := sf "%f %f %f" x y z :: !acc)
      points ;
    concat ([sf "$%s << EOD" data_name] @ List.rev !acc @ [sf "EOD"])

  let _4d ~data_name points =
    let acc = ref [] in
    Seq.iter
      (fun ({ w; x; y; z } : r4) -> acc := sf "%f %f %f %f" w x y z :: !acc)
      points ;
    concat ([sf "$%s << EOD" data_name] @ List.rev !acc @ [sf "EOD"])
end

module GP_style : sig
  type t = private string

  val linecolor : color -> t

  val fillcolor : color -> t [@@ocaml.warning "-32"]

  val render : style -> t
end = struct
  type t = string

  let ptcode s =
    match s with
    | Dot -> 0
    | Plus -> 1
    | Cross -> 2
    | Star -> 3
    | Square -> 4
    | Box -> 5
    | Disk -> 6
    | Circle -> 7
    | Delta -> 8
    | Delta_solid -> 9

  (* UI from the 70's *)
  (* Note: this mapping should depend on the terminal type ... *)
  let pointtype (s : pointtype option) (sz : float option) =
    match (s, sz) with
    | (None, None) -> ""
    | (None, Some sz) -> sf "pointsize %f" sz
    | (Some pt, None) -> sf "pointtype %d" (ptcode pt)
    | (Some pt, Some sz) -> sf "pointtype %d pointsize %f" (ptcode pt) sz

  let linecolor { r; g; b } =
    let r = int_of_float (255. *. r) in
    let g = int_of_float (255. *. g) in
    let b = int_of_float (255. *. b) in
    sf "lc rgb '0x%02X%02X%02X'" r g b

  let fillcolor { r; g; b } =
    let r = int_of_float (255. *. r) in
    let g = int_of_float (255. *. g) in
    let b = int_of_float (255. *. b) in
    sf "fillcolor rgb '0x%02X%02X%02X'" r g b

  let fill (f : fill) =
    let fill_style_str =
      match f.fill_style with
      | Empty -> sf "fs empty"
      | Solid { density; transparent = true } ->
          sf "fs transparent solid %f" density
      | Solid { density; transparent = false } -> sf "fs solid %f" density
      | Pattern { code; transparent = true } ->
          sf "fs transparent pattern %d" code
      | Pattern { code; transparent = false } -> sf "fs pattern %d" code
    in
    let fill_color_str = Option.fold ~none:"" ~some:fillcolor f.fill_color in
    String.concat " " [fill_style_str; fill_color_str]

  let render (style : style) =
    let clr_str = Option.fold ~none:"" ~some:linecolor style.color in
    match style.shape_type with
    | Point { ptyp; psize } ->
        let pointtype_str = pointtype ptyp psize in
        String.concat " " [pointtype_str; clr_str]
    | Circle { fill = None; _ } -> clr_str
    | Circle { fill = Some fillspec; _ } ->
        let fill_str = fill fillspec in
        let clr_str = Option.fold ~none:"" ~some:linecolor style.color in
        String.concat " " [clr_str; fill_str]
end

module GP_subcommand : sig
  type t = private string

  val scatter_2d : data_name:string -> style -> legend_opt:string option -> t

  val y_error_bars : data_name:string -> style -> legend_opt:string option -> t

  val y_error_lines : data_name:string -> style -> legend_opt:string option -> t

  val scatter_3d : data_name:string -> style -> legend_opt:string option -> t

  val lines_2d : data_name:string -> style -> legend_opt:string option -> t

  val lines_3d : data_name:string -> style -> legend_opt:string option -> t

  val linespoints_2d :
    data_name:string -> style -> legend_opt:string option -> t

  val linespoints_3d :
    data_name:string -> style -> legend_opt:string option -> t

  val histogram :
    data_name:string -> histogram_options -> legend_opt:string option -> t
end = struct
  type t = string

  let print_legend legend_opt =
    Option.fold ~none:"notitle" ~some:(fun x -> sf "title '%s'" x) legend_opt

  let scatter_2d ~data_name s ~legend_opt =
    let sty = (GP_style.render s :> string) in
    match s.shape_type with
    | Point _ ->
        sf
          "$%s using 1:2 %s with points %s"
          data_name
          (print_legend legend_opt)
          sty
    | Circle { radius; _ } ->
        sf
          "$%s using 1:2:(%f) %s with circles %s"
          data_name
          radius
          (print_legend legend_opt)
          sty

  let y_error_bars ~data_name s ~legend_opt =
    let sty = (GP_style.render s :> string) in
    sf
      "$%s using 1:2:3:4 %s with yerrorbars %s"
      data_name
      (print_legend legend_opt)
      sty

  let y_error_lines ~data_name s ~legend_opt =
    let sty = (GP_style.render s :> string) in
    sf
      "$%s using 1:2:3:4 %s with yerrorlines %s"
      data_name
      (print_legend legend_opt)
      sty

  let scatter_3d ~data_name s ~legend_opt =
    let sty = (GP_style.render s :> string) in
    match s.shape_type with
    | Point _ ->
        sf
          "$%s using 1:2:3 %s with points %s"
          data_name
          (print_legend legend_opt)
          sty
    | Circle { radius; _ } ->
        sf
          "$%s using 1:2:3:(%f) %s with circles %s"
          data_name
          radius
          (print_legend legend_opt)
          sty

  let lines_2d ~data_name s ~legend_opt =
    let sty = (GP_style.render s :> string) in
    sf "$%s using 1:2 %s with lines %s" data_name (print_legend legend_opt) sty

  let lines_3d ~data_name s ~legend_opt =
    let sty = (GP_style.render s :> string) in
    sf
      "$%s using 1:2:3 %s with lines %s"
      data_name
      (print_legend legend_opt)
      sty

  let linespoints_2d ~data_name s ~legend_opt =
    let sty = (GP_style.render s :> string) in
    sf
      "$%s using 1:2 %s with linespoints %s"
      data_name
      (print_legend legend_opt)
      sty

  let linespoints_3d ~data_name s ~legend_opt =
    let sty = (GP_style.render s :> string) in
    sf
      "$%s using 1:2:3 %s with linespoints %s"
      data_name
      (print_legend legend_opt)
      sty

  let hist_preprocessing_text { bins; binwidth; _ } =
    match (bins, binwidth) with
    | (None, None) -> ""
    | (Some nbins, None) -> sf "bins=%d" nbins
    | (None, Some width) -> sf "bins binwidth=%f" width
    | (Some nbins, Some width) -> sf "bins=%d binwidth=%f" nbins width

  let histogram ~data_name preprocessing_opts ~legend_opt =
    sf
      "$%s using 1 %s %s with boxes %s"
      data_name
      (print_legend legend_opt)
      (hist_preprocessing_text preprocessing_opts)
      (Option.fold
         ~none:""
         ~some:(fun c -> (GP_style.linecolor c :> t))
         preprocessing_opts.color)
end

module GP_command : sig
  type t = private string

  val plot : GP_subcommand.t list -> t

  val splot : GP_subcommand.t list -> t
end = struct
  type t = string

  let concat_subcommands plots =
    List.map (fun (x : GP_subcommand.t) -> (x :> string)) plots
    |> String.concat ", "

  let splot plots = sf "splot %s" (concat_subcommands plots)

  let plot plots = sf "plot %s" (concat_subcommands plots)
end

module GP_script : sig
  type t = private string

  val render : plot -> t

  val render_matrix : title:string -> plot option array array -> t
end = struct
  type t = string

  let set_xlabel l = sf "set xlabel \"%s\"" l

  let set_ylabel l = sf "set ylabel \"%s\"" l

  let set_zlabel l = sf "set zlabel \"%s\"" l

  let set_title t = sf "set title \"%s\"" t

  let namegen =
    let x = ref (-1) in
    fun () ->
      incr x ;
      sf "name_%d" !x

  let rec seq_map2 (seq1 : 'a Seq.t) (seq2 : 'b Seq.t) f : _ Seq.t =
   fun () ->
    match (seq1 (), seq2 ()) with
    | (Nil, Nil) -> Nil
    | (Cons (x, tl1), Cons (y, tl2)) -> Cons (f x y, seq_map2 tl1 tl2 f)
    | _ -> invalid_arg "seq_map2: uneven length"

  let make_error_bars (data : r2 Seq.t) (error_bars : r2 Seq.t) =
    seq_map2 data error_bars (fun ({ x; y } : r2) ({ x = y1; y = y2 } : r2) ->
        r4 x y (y +. y1) (y -. y2))

  let scatter (type dim) (axes : dim axes)
      ({ data; style; legend } : dim Data.t with_metadata)
      (error_bars : r2 Seq.t option) =
    match axes with
    | Dim2_axes _ -> (
        let data_name = namegen () in
        match error_bars with
        | None ->
            let data_block = GP_data._2d ~data_name data in
            let command_chunk =
              GP_subcommand.scatter_2d ~data_name style ~legend_opt:legend
            in
            (`Data data_block, `Command command_chunk)
        | Some error_bars ->
            let data_with_errors =
              GP_data._4d ~data_name (make_error_bars data error_bars)
            in
            let command_chunk =
              GP_subcommand.y_error_bars ~data_name style ~legend_opt:legend
            in
            (`Data data_with_errors, `Command command_chunk))
    | Dim3_axes _ ->
        let data_name = namegen () in
        let data_block = GP_data._3d ~data_name data in
        let command_chunk =
          GP_subcommand.scatter_3d ~data_name style ~legend_opt:legend
        in
        (`Data data_block, `Command command_chunk)

  let line (type dim) (axes : dim axes)
      ({ data; style; legend } : dim Data.t with_metadata) with_points
      error_bars =
    match axes with
    | Dim2_axes _ -> (
        let data_name = namegen () in
        match error_bars with
        | None ->
            let data_block = GP_data._2d ~data_name data in
            let command_chunk =
              if with_points then
                GP_subcommand.linespoints_2d ~data_name style ~legend_opt:legend
              else GP_subcommand.lines_2d ~data_name style ~legend_opt:legend
            in
            (`Data data_block, `Command command_chunk)
        | Some error_bars ->
            (* assert (not with_points) ; *)
            let data_with_errors =
              GP_data._4d ~data_name (make_error_bars data error_bars)
            in
            let command_chunk =
              GP_subcommand.y_error_lines ~data_name style ~legend_opt:legend
            in
            (`Data data_with_errors, `Command command_chunk))
    | Dim3_axes _ ->
        let data_name = namegen () in
        let data_block = GP_data._3d ~data_name data in
        let command_chunk =
          if with_points then
            GP_subcommand.linespoints_3d ~data_name style ~legend_opt:legend
          else GP_subcommand.lines_3d ~data_name style ~legend_opt:legend
        in
        (`Data data_block, `Command command_chunk)

  let histogram (data : r1 Seq.t) (options : histogram_options) legend_opt =
    let data_name = namegen () in
    let data_block = GP_data._1d ~data_name data in
    let command_chunk =
      GP_subcommand.histogram ~data_name options ~legend_opt
    in
    (`Data data_block, `Command command_chunk)

  let rec spec_list :
      type dim.
      dim axes ->
      dim spec list ->
      GP_data.t list ->
      GP_subcommand.t list ->
      GP_data.t list * GP_subcommand.t list =
    fun (type dim) (axes : dim axes) (specs : dim spec list) data_acc cmd_acc ->
     match specs with
     | [] -> (List.rev data_acc, List.rev cmd_acc)
     | Scatter { data; error_bars } :: tl ->
         let (`Data data_block, `Command command_chunk) =
           scatter axes data error_bars
         in
         spec_list axes tl (data_block :: data_acc) (command_chunk :: cmd_acc)
     | Histogram { data; options; legend } :: tl ->
         let (`Data data_block, `Command command_chunk) =
           histogram data options legend
         in
         spec_list axes tl (data_block :: data_acc) (command_chunk :: cmd_acc)
     | Line { data; with_points; error_bars } :: tl ->
         let (`Data data_block, `Command command_chunk) =
           line axes data with_points error_bars
         in
         spec_list axes tl (data_block :: data_acc) (command_chunk :: cmd_acc)

  let render (Plot { axes; plots; title }) =
    let title_cmd =
      match title with None -> set_title "" | Some title -> set_title title
    in
    match axes with
    | Dim2_axes { xaxis; yaxis } ->
        let (all_data, cmds) = spec_list axes plots [] [] in
        let all_data = (all_data :> string list) in
        let cmd = (GP_command.plot cmds :> string) in
        concat
          [set_xlabel xaxis; set_ylabel yaxis; concat all_data; title_cmd; cmd]
    | Dim3_axes { xaxis; yaxis; zaxis } ->
        let (all_data, cmds) = spec_list axes plots [] [] in
        let all_data = (all_data :> string list) in
        let cmd = (GP_command.splot cmds :> string) in
        concat
          [ set_xlabel xaxis;
            set_ylabel yaxis;
            set_zlabel zaxis;
            concat all_data;
            title_cmd;
            cmd ]

  let multiplot ~title ~matrix =
    let rows = Array.length matrix in
    let cols = Array.length matrix.(0) in
    let plots =
      List.flatten (Array.to_list ((Array.map Array.to_list) matrix))
    in
    concat
      ([ sf
           "set multiplot layout %d, %d rowsfirst downwards title '%s'"
           rows
           cols
           title ]
      @ plots @ [sf "unset multiplot"])

  let render_matrix ~title plots =
    let plot_matrix =
      Array.map
        (Array.map (function
            | None -> "set multiplot next"
            | Some p -> render p))
        plots
    in
    multiplot ~title ~matrix:plot_matrix
end

(* Plot targets *)
type target =
  | Pdf_target of { cm_size : (float * float) option; pdf_file : string }
  | Png_target of { pixel_size : (int * int) option; png_file : string }
  | X11_target
  | Qt_target of { pixel_size : (int * int) option }

let pdf ?cm_size ~pdf_file () = Pdf_target { cm_size; pdf_file }

let png ?pixel_size ~png_file () = Png_target { pixel_size; png_file }

let x11 = X11_target

let qt ?pixel_size () = Qt_target { pixel_size }

module GP_run = struct
  let set_target (t : target) =
    let print_pixel_size pixel_size =
      Option.fold ~none:"" ~some:(fun (x, y) -> sf "size %d, %d" x y) pixel_size
    in
    match t with
    | Png_target { pixel_size; png_file } ->
        concat
          [ sf "set terminal pngcairo %s" (print_pixel_size pixel_size);
            sf "set output '%s'" png_file ]
    | Pdf_target { cm_size; pdf_file } ->
        concat
          [ sf
              "set terminal pdf %s"
              (Option.fold
                 ~none:""
                 ~some:(fun (x, y) -> sf "size %fcm, %fcm" x y)
                 cm_size);
            sf "set output '%s'" pdf_file ]
    | X11_target -> "set terminal x11 noreplotonresize"
    | Qt_target { pixel_size } ->
        sf "set terminal qt %s" (print_pixel_size pixel_size)

  let is_target_interactive (t : target) =
    match t with
    | Png_target _ | Pdf_target _ -> false
    | X11_target | Qt_target _ -> true

  let make_script ~target ~(plot : GP_script.t) =
    concat
      ([set_target target; (plot :> string)]
      @ if is_target_interactive target then ["pause -1"] else [])

  let write_script ~filename ~target ~(plot : GP_script.t) =
    match open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o666 filename with
    | exception _ ->
        Format.eprintf "write_script: could not open file %s, exiting" filename ;
        exit 1
    | oc ->
        let script = make_script ~target ~plot in
        output_string oc script ;
        close_out oc

  let run_script ?(path = "gnuplot") ~target ~(plot : GP_script.t) () =
    let (name, oc) = Filename.open_temp_file ~perms:0o666 "gnuplot" ".gp" in
    let full_command =
      concat
        ([set_target target; (plot :> string)]
        @ if is_target_interactive target then ["pause -1"] else [])
    in
    output_string oc full_command ;
    close_out oc ;
    match Unix.system (sf "%s %s" path name) with
    | WEXITED 0 -> Unix.unlink name
    | _ ->
        Unix.unlink name ;
        Format.eprintf "run_script: call to gnuplot failed, exiting" ;
        exit 1

  let write_and_run_script ?(path = "gnuplot") ~filename ~target
      ~(plot : GP_script.t) () =
    match open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o666 filename with
    | exception _ ->
        Format.eprintf "write_script: could not open file %s, exiting" filename ;
        exit 1
    | oc -> (
        let script = make_script ~target ~plot in
        output_string oc script ;
        close_out oc ;
        match Unix.system (sf "%s %s" path filename) with
        | WEXITED 0 -> ()
        | _ ->
            Format.eprintf
              "run_script: call to gnuplot failed, exiting (wrote script to %s)"
              filename ;
            exit 1)
end

type action = Exec | Save_to of string | Exec_and_save_to of string

let exec = Exec

let save_to filename = Save_to filename

let exec_and_save_to filename = Exec_and_save_to filename

let write_plot ~filename ~target ~plot =
  let plot = GP_script.render plot in
  GP_run.write_script ~filename ~target ~plot

let run_plot ?path ~target ~plot () =
  let plot = GP_script.render plot in
  GP_run.run_script ?path ~target ~plot ()

let write_and_run_plot ?path ~filename ~target ~plot () =
  let plot = GP_script.render plot in
  GP_run.write_and_run_script ?path ~filename ~target ~plot ()

let write_matrix ~filename ~title ~target ~plots =
  let plot = GP_script.render_matrix ~title plots in
  GP_run.write_script ~filename ~target ~plot

let run_matrix ?path ~title ~target ~plots () =
  let plot = GP_script.render_matrix ~title plots in
  GP_run.run_script ?path ~target ~plot ()

let write_and_run_matrix ?path ~title ~filename ~target ~plots () =
  let plot = GP_script.render_matrix ~title plots in
  GP_run.write_and_run_script ?path ~filename ~target ~plot ()

let run ?path ~plot ~target = function
  | Exec -> run_plot ?path ~plot ~target ()
  | Save_to filename -> write_plot ~filename ~target ~plot
  | Exec_and_save_to filename ->
      write_and_run_plot ?path ~filename ~target ~plot ()

let run_matrix ?path ~plots ~target ?(title = "") = function
  | Exec -> run_matrix ?path ~title ~plots ~target ()
  | Save_to filename -> write_matrix ~filename ~title ~target ~plots
  | Exec_and_save_to filename ->
      write_and_run_matrix ?path ~title ~filename ~target ~plots ()
