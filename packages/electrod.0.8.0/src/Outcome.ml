(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2020 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

open Containers

type t =
  { trace : states option
  ; nbvars : int
  ; conversion_time : Mtime.span
  ; analysis_time : Mtime.span
  }

(** nonempty *)
and states = state list

and state_type =
  | Plain
  | Loop

(** A state is either a plain state, or the target of a lasso from the last
    state of the trace. *)
and state = state_type * valuation

(** A valuation maps set/relation names to the tuples they
    contain. Notice: the valuation is {b sorted} over names. *)
and valuation = (Name.t, Tuple_set.t) List.Assoc.t

let valuation valu = valu

let plain_state v = (Plain, v)

let loop_state v = (Loop, v)

let to_loop = function _, v -> loop_state v

let loop_is_present trace =
  match trace with
  | [] ->
      invalid_arg "Outcome.loop_is_present: empty states"
  | _ :: _ ->
      List.exists (function Loop, _ -> true | Plain, _ -> false) trace


let no_trace nbvars conversion_time analysis_time =
  { trace = None; analysis_time; nbvars; conversion_time }


let sort_states (atom_renaming, name_renaming) states =
  let sort = List.sort (fun (n1, _) (n2, _) -> Name.compare n1 n2) in
  let rename (name, ts) =
    ( List.assoc ~eq:Name.equal name name_renaming
    , Tuple_set.rename atom_renaming ts )
  in
  List.map (fun (typ, v) -> (typ, sort (List.map rename v))) states


let trace back_renamings nbvars conversion_time analysis_time states =
  assert (
    (not @@ List.is_empty states)
    && List.exists (function Loop, _ -> true | Plain, _ -> false) states );
  { trace = Some (sort_states back_renamings states)
  ; analysis_time
  ; nbvars
  ; conversion_time
  }


let some_trace { trace; _ } = Option.is_some trace

open Fmtc

module PPPlain = struct
  let pp_valuation out valu =
    pf out "%a" (hvbox @@ list ~sep:sp @@ pair ~sep:equal Name.pp Tuple_set.pp)
    @@ List.sort (fun (n1, _) (n2, _) -> Name.compare n1 n2) valu


  let pp_state out = function
    | Plain, v ->
        (const string "  " **< brackets_ pp_valuation) out v
    | Loop, v ->
        (const string "->" **< brackets_ pp_valuation) out v


  let pp out t =
    match t.trace with
    | None ->
        pf out "--no trace--"
    | Some trace ->
        (vbox @@ list ~sep:sp pp_state) out trace
end

module PPChrono = struct
  module PB = PrintBox

  let to_string_width width fmt t =
    let module F = Format in
    let old_margin = F.get_margin () in
    F.pp_set_margin F.str_formatter width;
    F.fprintf F.str_formatter "%a" fmt t;
    let s = F.flush_str_formatter () in
    F.pp_set_margin F.str_formatter old_margin;
    s


  let state_as_array ((typ, v) : state) =
    let ts_strings =
      List.map (fun (_, ts) -> to_string_width 40 Tuple_set.pp ts) v
    in
    ( match typ with
    | Loop ->
        ts_strings @ [ "LOOP" ]
    | _ ->
        ts_strings @ [ " " ] )
    |> Array.of_list


  let pp out t =
    match t.trace with
    | None ->
        pf out "--no trace--"
    | Some [] ->
        assert false
    | Some ((_, hd) :: _ as trace) ->
        let trace_strings = List.map state_as_array trace in
        (* prepend names: *)
        let preprended =
          ( Array.of_list
          @@ List.map (fun (name, _) -> to_string_width 40 Name.pp name) hd
          @ [ " " ] )
          :: trace_strings
          |> Array.of_list
        in
        let table = PB.transpose preprended in
        (* mark differences *)
        for line = 0 to Array.length table - 2 do
          (* last is for LOOPs *)
          for col = Array.length table.(line) - 1 downto 2 do
            (* first is for name and second has no predecessor *)
            if String.equal table.(line).(col) table.(line).(col - 1)
            then table.(line).(col) <- "-==-"
          done
        done;
        PrintBox_text.output Stdlib.stdout
        @@ PB.grid_text ~pad:(PB.hpad 1) table
end

module PPXML = struct
  let kwd = string

  let attr = styled `Green string

  let pp_atom out at =
    let tag = "a" in
    pf out "<%a>%a</%a>" kwd tag (styled `Cyan Atom.pp) at kwd tag


  let pp_tuple out tuple =
    let tag = "t" in
    pf
      out
      "@[<h><%a>%a</%a>@]"
      kwd
      tag
      (list ~sep:cut pp_atom)
      (Tuple.to_list tuple)
      kwd
      tag


  let pp_one_valuation out (name, ts) =
    let tag = "rel" in
    let attribute = "name" in
    if Tuple_set.is_empty ts
    then pf out "@[<h><%a %a=\"%a\"/>@]" kwd tag attr attribute Name.pp name
    else
      pf
        out
        "@[<v><%a %a=\"%a\">@,  @[<v>%a@]@,</%a>@]"
        kwd
        tag
        attr
        attribute
        Name.pp
        name
        (Tuple.Set.pp ~pp_sep:(const string "") pp_tuple)
        (Tuple_set.tuples ts)
        kwd
        tag


  let pp_valuation out valu =
    list ~sep:cut pp_one_valuation out
    @@ List.sort (fun (n1, _) (n2, _) -> Name.compare n1 n2) valu


  let pp_state out st =
    let tag = "st" in
    let attribute = "loop-target" in
    let valu, loop =
      match st with Loop, v -> (v, true) | Plain, v -> (v, false)
    in
    pf
      out
      "@[<v><%a %a=\"%a\">@,  @[<v>%a@]@,</%a>@]"
      kwd
      tag
      attr
      attribute
      (styled `Cyan bool)
      loop
      pp_valuation
      valu
      kwd
      tag


  let pp out { trace; nbvars; conversion_time; analysis_time } =
    let ct = Mtime.Span.to_ms conversion_time in
    let at = Mtime.Span.to_ms analysis_time in
    pf
      out
      "<?%a %a=\"1.0\" %a=\"UTF-8\"?>@\n"
      kwd
      "xml"
      attr
      "version"
      attr
      "encoding";
    ( match trace with
    | None ->
        pf
          out
          "@[<h><%a nbvars='%d' conversion-time='%.0f' analysis-time='%.0f'/>@]@\n"
          kwd
          "notrace"
          nbvars
          ct
          at
    | Some trace ->
        let tag = "trace" in
        pf
          out
          "@[<v><%a nbvars='%d' conversion-time='%.0f' analysis-time='%.0f'>@,\
          \ @[<v>%a@]@,\
           </%a>@]"
          kwd
          tag
          nbvars
          ct
          at
          (list ~sep:sp pp_state)
          trace
          kwd
          tag );
    Format.pp_print_flush out ()
end

let pp ~(format : [ `XML | `Plain | `Chrono ]) out trace =
  match format with
  | `Plain ->
      PPPlain.pp out trace
  | `Chrono ->
      PPChrono.pp out trace
  | `XML ->
      PPXML.pp out trace
