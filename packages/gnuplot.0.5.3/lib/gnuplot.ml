(*
   Gnuplot-OCaml - Simple interface to Gnuplot

   Copyright (C) 2014-  Oliver Gu
   email: gu.oliver@yahoo.com

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Core
open Printf

module Color = struct
  type t =
    [ `Black
    | `Red
    | `Green
    | `Yellow
    | `Blue
    | `Magenta
    | `Cyan
    | `White
    | `Rgb of int * int * int
    ]

  let to_rgb = function
    | `Black -> 0, 0, 0
    | `Red -> 255, 0, 0
    | `Green -> 0, 128, 0
    | `Yellow -> 255, 255, 0
    | `Blue -> 0, 0, 255
    | `Magenta -> 255, 0, 255
    | `Cyan -> 0, 255, 255
    | `White -> 255, 255, 255
    | `Rgb (r, g, b) -> r, g, b
end

let format_style = function
  | `Solid -> " fs solid"
  | `Pattern n -> sprintf " fs pattern %d" n

let datefmt = "%Y-%m-%d"
let timefmt = "%Y-%m-%d-%H:%M:%S"

let format_date d = Date.format d datefmt
let format_time t ~zone = Time.format t timefmt ~zone
let format_num = Float.to_string

module Internal_format = struct

  let format_arg ?(default = "") f a_opt =
    Option.value_map a_opt ~default ~f:(fun a -> f a)

  let format_plot_title = format_arg (sprintf "set title \"%s\"")

  let format_title = format_arg (sprintf " t \"%s\"") ~default:" notitle"

  let format_color s = format_arg (fun (color : Color.t) ->
    let r, g, b = Color.to_rgb color in
    sprintf " %s rgb '#%02x%02x%02x'" s r g b)

  let format_num_arg s = format_arg (sprintf " %s %d" s)

  let format_fill = format_arg format_style

  let format_label label = format_arg (fun s -> sprintf "set %s \"%s\"" label s)
end
open Internal_format

module Command = struct
  type t = {
    command : string;
    cleanup : string;
  }
end

module Range = struct
  type t =
    | X  of float * float
    | Y  of float * float
    | XY of float * float * float * float
    | Date of Date.t * Date.t
    | Time of Time.t * Time.t * Time.Zone.t
    | Local_time of Time.t * Time.t

  let range ?xspec ?yspec () =
    let sep = match xspec, yspec with
      | Some _, Some _ -> "\n"
      | _, _ -> ""
    in
    let xspec = format_arg (sprintf "set xrange %s") xspec in
    let yspec = format_arg (sprintf "set yrange %s") yspec in
    { Command.
      command = String.concat [xspec; yspec] ~sep;
      cleanup = "set autoscale xy";
    }

  let to_cmd t =
    match t with
    | X (x1, x2) ->
      range
        ~xspec:(sprintf "[%s:%s]" (format_num x1) (format_num x2))
        ()
    | Y (y1, y2) ->
      range
        ~yspec:(sprintf "[%s:%s]" (format_num y1) (format_num y2))
        ()
    | XY (x1, x2, y1, y2) ->
      range
        ~xspec:(sprintf "[%s:%s]" (format_num x1) (format_num x2))
        ~yspec:(sprintf "[%s:%s]" (format_num y1) (format_num y2))
        ()
    | Date (d1, d2) ->
      range
        ~xspec:(sprintf "[\"%s\":\"%s\"]" (format_date d1) (format_date d2))
        ()
    | Time (t1, t2, zone) ->
      range
        ~xspec:(sprintf "[\"%s\":\"%s\"]" (format_time t1 ~zone) (format_time t2 ~zone))
        ()
    | Local_time (t1, t2) ->
      let zone = Lazy.force Time.Zone.local in
      range
        ~xspec:(sprintf "[\"%s\":\"%s\"]" (format_time t1 ~zone) (format_time t2~zone))
        ()
end

module Filling = struct
  type t =
    [ `Solid
    | `Pattern of int
    ]

  let to_cmd t =
    { Command.
      command = format_style t |> sprintf "set style %s\n";
      cleanup = "";
    }
end

module Output_type = struct
  type t =
    [ `Wxt
    | `X11
    | `Qt
    | `Png of string | `Png_cairo of string
    | `Eps of string
    ]
end

module Output = struct
  type t = {
    font : string option;
    output : Output_type.t;
  }

  let create ?font output = { font; output; }

  let to_cmd t =
    let font = t.font |> format_arg (sprintf " font '%s'") in
    let output =
      match t.output with
      | `Wxt ->
        "set term wxt persist"^font
      | `X11 ->
        "set term x11 persist"^font
      | `Qt ->
        "set term qt persist"^font
      | `Png s ->
        sprintf "set term png%s\nset output '%s'" font s
      | `Png_cairo s ->
        sprintf "set term pngcairo%s\nset output '%s'" font s
      | `Eps s ->
        sprintf "set term postscript eps enhanced%s\nset output '%s'" font s
    in
    { Command.
      command = output;
      cleanup = "set term x11";
    }


end

module Grid = struct
  let to_cmd =
    { Command.
      command = "set grid";
      cleanup = "unset grid";
    }
end

module Title = struct
  type t = {
    title : string option;
  }

  let create ?title () = { title }

  let to_cmd t =
    { Command.
      command = format_plot_title t.title;
      cleanup = "unset title";
    }
end

module Labels = struct
  type t = {
    x : string option;
    y : string option;
  }

  let create ?x ?y () = { x; y; }

  let to_cmd t =
    let cmd =
      [ format_label "xlabel" t.x, "set xlabel"
      ; format_label "ylabel" t.y, "set ylabel"
      ] |> List.filter ~f:(fun (s, _) -> s <> "")
    in
    { Command.
      command = cmd |> List.map ~f:fst |> String.concat ~sep:"\n";
      cleanup = cmd |> List.map ~f:snd |> String.concat ~sep:"\n";
    }
end

module Timefmtx = struct
  type t = {
    timefmt : string;
    format  : string option;
  }

  let create ?format timefmt = { timefmt; format; }

  let to_cmd t =
    { Command.
      command =
        [ Some ("set timefmt \""^t.timefmt^"\"\nset xdata time")
        ; Option.map t.format ~f:(fun fmt -> "\nset format x \""^fmt^"\"")
        ] |> List.filter_opt |> String.concat;
      cleanup = "set xdata";
    }
end

type kind =
  | Lines
  | Points
  | Linespoints
  | Steps
  | Histogram
  | Candlesticks

type data =
  | Data_Y of float list
  | Data_XY of (float * float) list
  | Data_TimeY of (Time.t * float) list * Time.Zone.t
  | Data_DateY of (Date.t * float) list
  | Data_TimeOHLC of (Time.t * (float * float * float * float)) list * Time.Zone.t
  | Data_DateOHLC of (Date.t * (float * float * float * float)) list
  | Func of string

module Series = struct
  type t = {
    cmd : string;
    data : data;
  }

  let create ?title ?color ?weight ?fill kind data =
    let kind_text =
      match kind with
      | Lines -> "lines"
      | Points -> "points"
      | Linespoints -> "linespoints"
      | Steps -> "steps"
      | Histogram -> "histogram"
      | Candlesticks -> "candlesticks"
    in
    let cmd =
      String.concat [
        (match data with
         | Data_Y _ -> " '-' using 1 with " ^ kind_text
         | Data_XY _ | Data_TimeY _ | Data_DateY _ ->
           " '-' using 1:2 with " ^ kind_text
         | Data_TimeOHLC _ | Data_DateOHLC _ ->
           " '-' using 1:2:3:4:5 with " ^ kind_text
         | Func f -> f ^ " with " ^ kind_text)
      ; format_title title
      ; format_num_arg "lw" weight
      ; format_color "lc" color
      ; format_fill fill ]
    in
    { cmd; data; }

  let lines ?title ?color ?weight data =
    create ?title ?color ?weight Lines (Data_Y data)

  let lines_xy ?title ?color ?weight data =
    create ?title ?color ?weight Lines (Data_XY data)

  let lines_timey ?title ?color ?weight ~zone data =
    create ?title ?color ?weight Lines (Data_TimeY (data, zone))

  let lines_datey ?title ?color ?weight data =
    create ?title ?color ?weight Lines (Data_DateY data)

  let lines_func ?title ?color ?weight f =
    create ?title ?color ?weight Lines (Func f)

  let points ?title ?color ?weight data =
    create ?title ?color ?weight Points (Data_Y data)

  let points_xy ?title ?color ?weight data =
    create ?title ?color ?weight Points (Data_XY data)

  let points_timey ?title ?color ?weight ~zone data =
    create ?title ?color ?weight Points (Data_TimeY (data, zone))

  let points_datey ?title ?color ?weight data =
    create ?title ?color ?weight Points (Data_DateY data)

  let points_func ?title ?color ?weight f =
    create ?title ?color ?weight Points (Func f)

  let linespoints ?title ?color ?weight data =
    create ?title ?color ?weight Linespoints (Data_Y data)

  let linespoints_xy ?title ?color ?weight data =
    create ?title ?color ?weight Linespoints (Data_XY data)

  let linespoints_timey ?title ?color ?weight ~zone data =
    create ?title ?color ?weight Linespoints (Data_TimeY (data, zone))

  let linespoints_datey ?title ?color ?weight data =
    create ?title ?color ?weight Linespoints (Data_DateY data)

  let linespoints_func ?title ?color ?weight f =
    create ?title ?color ?weight Linespoints (Func f)

  let steps ?title ?color ?weight data =
    create ?title ?color ?weight Steps (Data_Y data)

  let steps_xy ?title ?color ?weight data =
    create ?title ?color ?weight Steps (Data_XY data)

  let steps_timey ?title ?color ?weight ~zone data =
    create ?title ?color ?weight Steps (Data_TimeY (data, zone))

  let steps_datey ?title ?color ?weight data =
    create ?title ?color ?weight Steps (Data_DateY data)

  let histogram ?title ?color ?weight ?fill data =
    create ?title ?color ?weight ?fill Histogram (Data_Y data)

  let candles_time_ohlc ?title ?color ?weight ?fill ~zone data =
    create ?title ?color ?weight ?fill Candlesticks (Data_TimeOHLC (data, zone))

  let candles_date_ohlc ?title ?color ?weight ?fill data =
    create ?title ?color ?weight ?fill Candlesticks (Data_DateOHLC data)
end

module Gp = struct
  type t = {
    channel : Out_channel.t;
    verbose : bool;
  }

  let create ?(verbose = false) ?path () =
    let path = Option.value path ~default:"gnuplot" in
    { channel = Unix.open_process_out path; verbose }

  let send_cmd t cmd = Out_channel.output_string t.channel (cmd^"\n")

  let close t = ignore (Unix.close_process_out t.channel)

  let send_data t data =
    match data with
    | Data_Y data ->
      List.iter data ~f:(fun y -> send_cmd t (format_num y));
      send_cmd t "e"
    | Data_XY data ->
      List.iter data ~f:(fun (x, y) ->
        send_cmd t (format_num x ^" "^ format_num y));
      send_cmd t "e"
    | Data_TimeY (data, zone) ->
      List.iter data ~f:(fun (tm, y) ->
        send_cmd t (format_time tm ~zone ^" "^ format_num y));
      send_cmd t "e"
    | Data_DateY data ->
      List.iter data ~f:(fun (d, y) ->
        send_cmd t (format_date d ^" "^ format_num y));
      send_cmd t "e"
    | Data_TimeOHLC (data, zone) ->
      List.iter data ~f:(fun (tm, (o, h, l, c)) ->
        send_cmd t (format_time tm ~zone ^" "^
                    format_num         o ^" "^
                    format_num         h ^" "^
                    format_num         l ^" "^
                    format_num         c));
      send_cmd t "e"
    | Data_DateOHLC data ->
      List.iter data ~f:(fun (d, (o, h, l, c)) ->
        send_cmd t (format_date d ^" "^
                    format_num  o ^" "^
                    format_num  h ^" "^
                    format_num  l ^" "^
                    format_num  c));
      send_cmd t "e"
    | Func _ -> ()

  let internal_set ?output ?title ?(use_grid=false) ?fill ?range ?labels ?timefmtx t =
    let commands =
      [ Option.map output ~f:Output.to_cmd
      ; Option.map title ~f:(fun title -> Title.(create ~title () |> to_cmd))
      ; Option.some_if use_grid Grid.to_cmd
      ; Option.map fill ~f:Filling.to_cmd
      ; Option.map timefmtx ~f:Timefmtx.to_cmd
      ; Option.map range ~f:Range.to_cmd
      ; Option.map labels ~f:Labels.to_cmd
      ] |> List.filter_opt
    in
    List.iter commands ~f:(fun cmd ->
      if t.verbose then printf "Setting:\n%s\n%!" cmd.Command.command;
      send_cmd t cmd.Command.command)

  let set ?output ?title ?use_grid ?fill ?labels t =
    internal_set ?output ?title ?use_grid ?fill ?labels t

  let unset ?fill ?labels t =
    let commands =
      [ Option.map fill ~f:Filling.to_cmd
      ; Option.map labels ~f:Labels.to_cmd
      ] |> List.filter_opt
    in
    List.iter commands ~f:(fun cmd ->
      if cmd.Command.cleanup <> "" then begin
        if t.verbose then printf "Setting:\n%s\n%!" cmd.Command.cleanup;
        send_cmd t cmd.Command.cleanup
      end)

  let plot_many ?output ?title ?use_grid ?fill ?range ?labels ?format t data =
    begin match (List.hd_exn data).Series.data with
      | Data_TimeY _ | Data_TimeOHLC _ ->
        let timefmtx = Timefmtx.create ?format timefmt in
        internal_set ?output ?title ?use_grid ?fill ?range ?labels ~timefmtx t
      | Data_DateY _ | Data_DateOHLC _ ->
        let timefmtx = Timefmtx.create ?format datefmt in
        internal_set ?output ?title ?use_grid ?fill ?range ?labels ~timefmtx t
      | _ ->
        internal_set ?output ?title ?use_grid ?fill ?range ?labels t
    end;
    let cmd =
      "plot \\\n" ^
      (List.map data ~f:(fun s -> s.Series.cmd) |> String.concat ~sep:", \\\n")
    in
    if t.verbose then printf "Command: %s\n%!" cmd;
    send_cmd t cmd;
    List.iter data ~f:(fun s -> send_data t s.Series.data);
    unset ?fill ?labels t;
    Out_channel.flush t.channel

  let plot ?output ?title ?use_grid ?fill ?range ?labels ?format t data =
    plot_many ?output ?title ?use_grid ?fill ?range ?labels  ?format t [data]

  let plot_func ?output ?title ?use_grid ?fill ?range ?labels t func =
    plot_many ?output ?title ?use_grid ?fill ?range ?labels t [Series.lines_func func]
end
