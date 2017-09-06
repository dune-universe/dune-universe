(*
 * Copyright (c) 2016-2017 Richard Mortier <mort@cantab.net>
 *
 * Licensed under the ISC Licence; see LICENSE.md in the root of this
 * distribution or the full text at https://opensource.org/licenses/isc-license
 *
 *)

open CalendarLib
open Astring
open Notty
open Notty.Infix

module F = struct
  type t = {
    s: string -> image;
    b: string -> image;
    iu: string -> image;
    i: string -> image;
    u: string -> image;
    r: string -> image;
  }

  let s = I.string A.empty
  let b = I.string A.(st bold)
  let i = I.string A.(st italic)
  let iu = I.string A.(st italic ++ st underline)
  let u = I.string A.(st underline)
  let r = I.string A.(st reverse)

  let plain = {
    s;
    b=s;
    iu=s;
    i=s;
    u=s;
    r=s
  }

  let pretty = {
    s;
    b;
    iu;
    i;
    u;
    r
  }

  let lpad ?(f=s) ~w x = x |> f |> I.hsnap ~align:`Right w
  let rpad ?(f=s) ~w x = x |> f |> I.hsnap ~align:`Left w
  let centre ?(f=s) ~w x = x |> f |> I.hsnap ~align:`Middle w
  let endl = Notty_unix.output_image_endline
end

let chunk n list =
  list
  |> List.fold_left (fun (i,acc) e ->
      match (i, n) with
      | (i, n) when i = 0 || n = 1 -> 1, [e] :: acc
      | (i, n) when i < n ->
        let n' = (i+1) mod n in
        let acc' = ((e :: List.hd acc) :: (List.tl acc)) in
        n', acc'
      | (_i, _n) (* i >= n *) -> failwith "never reached"
    ) (0, [])
  |> snd
  |> List.(rev_map rev)

let render_header ~f ~weeks_of_year ~first_dow ~y ~m =
  let title =
    Printf.sprintf "%s %d" (Printer.name_of_month m) y
  in
  let colheads =
    let wk = if not weeks_of_year then I.void 0 0 else f.F.iu "wk" in
    wk
    <|> (
      I.hcat (Days.of_week first_dow
              |> List.map (fun d -> f.F.u (" " ^ Day.to_string d))
             )
      |> I.hcrop (if weeks_of_year then 0 else 1) 0

    )
  in
  F.centre ~f:f.F.b ~w:(I.width colheads) title
  <->
  colheads

let render_day ~f ~today ~y ~m d =
  let f = if (Date.year today = y
              && Date.month today = m
              && Date.day_of_month today = d
             )
    then f.F.r
    else f.F.s
  in
  I.hsnap ~align:`Right 3 (f (Printf.sprintf "%2d" d))

let render_week ~f ~weeks_of_year ~width ~today ~y ~m align weekdays =
  let work_week ~y ~m days =
    let _, wk, _ =
      Date.(to_business (make y (int_of_month m) (List.hd days)))
    in
    string_of_int wk
  in
  let ww =
    if weeks_of_year then
      I.hsnap ~align:`Left 2 (weekdays |> work_week ~y ~m |> f.F.i)
    else
      I.void 0 0
  in
  let ds = (weekdays
            |> List.map (render_day ~f ~today ~y ~m)
            |> I.hcat
            |> I.hsnap ~align (3*7)
            |> I.hcrop (if weeks_of_year then 0 else 1) 0
           )
  in
  I.hsnap ~align:`Left width (ww <|> ds)

let render_month ~f ~weeks_of_year ~width ~first_dow ~today ~y ~m days =
  let first_day_of_full_week =
    Date.(day_of_month (nth_weekday_of_month y m first_dow 1))
  in
  let split n list =
    let rec aux i acc = function
      | [] -> List.rev acc, []
      | h :: t as l ->
        if i = 0 then List.rev acc, l else aux (i-1) (h :: acc) t
    in
    aux n [] list
  in
  let firstweek, otherweeks = split (first_day_of_full_week - 1) days in
  (
    (match firstweek with
     | [] -> I.void 0 0
     | firstweek ->
       render_week ~f ~weeks_of_year ~width ~today ~y ~m `Right firstweek
    )
    <->
    I.vcat (chunk 7 otherweeks |> List.map (fun weekdays ->
        render_week ~f ~weeks_of_year ~width ~today ~y ~m `Left weekdays
      ))
  )

(** generate a list of month-year pairs from the CLI specified range *)
let months range =
  let parse ?(rh=false) input =
    let input = String.Ascii.capitalize input in
    Printer.Date.(
      (* monthyear *)
      try from_fstring "%d%b%Y" ("01"^input)
      with Invalid_argument _ ->
        ( (* year *)
          try from_fstring "%d%b%Y" ("01"^(if rh then "Dec" else "Jan")^input)
          with Invalid_argument _ ->
            ( (* month *)
              let thisyear = string_of_int Date.(year (today ())) in
              from_fstring "%d%b%Y" ("01"^input^thisyear)
            )
        )
    )
  in
  let expand st nd =
    let st = parse st in
    let nd = parse ~rh:true nd in
    let rec aux d nd acc =
      match Date.compare d nd with
      | n  when n > 0 (* >  *) -> List.rev acc
      | _n            (* <= *) ->
        let d' = (Date.next d `Month) in
        aux d' nd (d::acc)
    in
    aux st nd []
  in
  match String.cuts ~sep:"-" range with
  | [st; nd] -> expand st nd
  | [st]     -> expand st st
  | _        -> invalid_arg ("invalid date range: " ^ range)

let cal plain weeks_of_year today ncols sep first_dow range =
  let f = if plain then F.plain else F.pretty in
  let sep = F.s sep in

  range
  |> months
  |> List.map (fun monthyear ->
      let m, y = Date.(month monthyear, year monthyear) in
      let header = render_header ~f ~weeks_of_year ~first_dow ~y ~m in
      let days =
        let width = I.width header in
        let days = Days.of_month monthyear in
        render_month ~f ~weeks_of_year ~width ~first_dow ~today ~y ~m days
      in
      let pad = I.void (if not weeks_of_year then 4 else 1) 0 in
      (
        header
        <->
        days
      )
      <|>
      (pad </> sep)
    )

  |> chunk ncols
  |> List.map I.hcat
  |> I.vcat
  |> F.endl
