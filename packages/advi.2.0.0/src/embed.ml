(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Jun Furuse, Didier Rémy and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* $Id$ *)

(* Embedding applications (in particular tcl/tk) applications. *)

(* In hash table t, returns all elements (vals) that verify predicate p. *)
let hashtbl_find_all t p =
  let res = ref [] in
  Hashtbl.iter (fun k x -> if p x then res := x :: !res) t;
  !res;;

type app_mode = | Fake | Raw | Sticky | Persistent | Ephemeral;;

type signal = int;;

type app = {
  app_name : Launch.app_name;
  app_mode : app_mode;
  app_pid : int;
  app_wid : GraphicsY11.window_id;
};;

let app_table = Hashtbl.create 17;;

let add_app app_name app_mode pid wid =
 Hashtbl.add app_table app_name {
   app_name = app_name;
   app_mode = app_mode;
   app_pid = pid;
   app_wid = wid;
  };;

let pid_in_app_table pid =
 hashtbl_find_all app_table (fun app -> app.app_pid == pid) <> [];;

(* Register an application with its mode, name, (sub)window id,
   and a fake process id (actually max_int).
   This function does not actually launch the application, it just
   allocates the ressources to launch it afterwards. *)
let fake_embed_app command app_mode app_name width height x gry =
 let wid = GraphicsY11.open_subwindow ~x ~y:gry ~width ~height in
 add_app app_name app_mode max_int wid;;

(* The function that launches all embedded applications.

   When encountering an embedded application, a call to raw_embed_app
   is stored in the list of applications to be launched at the next
   pause (in the [embeds] list reference).

   This function allocates a (sub)window for the application and tries
   to launch the application into this window. *)
let raw_embed_app command app_mode app_name width height x gry =
 (*Misc.debug (Printf.sprintf "Launching command %s" command);*)
 if Launch.can_execute_command command then begin

  let wid = GraphicsY11.open_subwindow ~x ~y:gry ~width ~height in

  (***
    The following ``@'' macros are recognized in embedded commands:

    @p : designates the embedding target window id (an X window identifier)
         in this case the geometry x and y specification should be 0.

      If @p is not specified, the applications will be treated by the WM.
      (If they are X apps, of course...)

    @g : designates the geometry like 100x100+20+30
    @w : designates the width of the target window in pixel
    @h : designates the height of the target window in pixel
    @x : designates the abscissa of the application against the root
    @y : designates the ordiante of the application against the root

    Why using "@" ?
    Just because '\' is for TeX. "%" is for TeX. "$" is for TeX...

    For some time, we also accept ``!'' as a synonymous for ``@''.
  ***)

  (* If there is no @p, the application geometry will be treated
     by the WM. In such cases, we try to fix the geometry
     so that it is against the root window. *)

  let against_wid =
     Misc.contains_string command "@p" ||
     Misc.contains_string command "!p" in

  let geom_x, geom_y =
    if against_wid then "0", "0" else
    (* fix the geometry *)
    let (ww, wh, wx, wy) = GraphicsY11.get_geometry () in
    string_of_int (wx + x),
    string_of_int (wy + (wh - gry) - height) in

  let geom_w = string_of_int width
  and geom_h = string_of_int height in

  let geom = Printf.sprintf "%sx%s+%s+%s" geom_w geom_h geom_x geom_y in

  let env = function
    | 'p' -> wid
    | 'g' -> geom
    | 'w' -> geom_w
    | 'h' -> geom_h
    | 'x' -> geom_x
    | 'y' -> geom_y
    | _ -> raise Not_found in

  let command = Misc.string_substitute_var env command in
  let pid = Launch.fork_process command in
  if pid_in_app_table pid
  then failwith (Printf.sprintf "pid %d is already in the app_table!" pid)
  else add_app app_name app_mode pid wid
 end;;

let find_embedded_app app_name = Hashtbl.find app_table app_name;;

let find_all_embedded_app app_name =
  hashtbl_find_all app_table (fun app -> app.app_name = app_name);;

let map_embed app =
  GraphicsY11.map_subwindow app.app_wid;;

let map_embedded_app app_name =
  try
    map_embed (find_embedded_app app_name)
  with Not_found -> ();;

let map_all_embedded_app app_name =
  List.iter map_embed (find_all_embedded_app app_name);;

let unmap_embed app = GraphicsY11.unmap_subwindow app.app_wid;;

let unmap_embedded_app app_name =
  try
    unmap_embed (find_embedded_app app_name)
  with Not_found -> ();;

let unmap_all_embedded_app app_name =
  List.iter unmap_embed (find_all_embedded_app app_name);;

let move_or_resize_persistent_app
    command app_mode app_name width height x gry =
  try
    let app = find_embedded_app app_name in
    let wid = app.app_wid in
    GraphicsY11.resize_subwindow wid width height;
    let gry = gry + height - width in
    GraphicsY11.move_subwindow wid x gry
  with Not_found -> ();;

(* In hash table t, verifies that at least one element verifies p. *)
let hashtbl_exists t f =
  try Hashtbl.iter (fun _ x -> if f x then raise Exit) t; false
  with Exit -> true;;

(* embedded apps must be displayed when synced. *)
let embed_app command app_mode app_name width height x gry =
  let already_launched app_name = Hashtbl.mem app_table app_name in
  match app_mode with
  | Fake ->
     Launch.add_embed
      (fun () ->
        (* prerr_endline ("Launching fake app " ^ app_name); *)
        fake_embed_app command app_mode app_name width height x gry)
  | Raw ->
     Launch.add_embed
      (fun () ->
        (* prerr_endline ("Launching raw app " ^ app_name); *)
        raw_embed_app command app_mode app_name width height x gry)
  | Sticky ->
     if not (already_launched app_name) then
     Launch.add_embed
      (fun () ->
        (* prerr_endline ("Launching sticky app " ^ app_name); *)
        raw_embed_app command app_mode app_name width height x gry) else
     Launch.add_persist
      (fun () ->
        (* prerr_endline ("Moving " ^ app_name); *)
        move_or_resize_persistent_app command app_mode app_name
          width height x gry)
  | Persistent ->
     if not (already_launched app_name) then
     Launch.add_embed
      (fun () ->
        (* prerr_endline ("Launching persistent app " ^ app_name); *)
        raw_embed_app command app_mode app_name width height x gry);
     Launch.add_persist
      (fun () ->
        (* prerr_endline ("Mapping " ^ app_name); *)
        map_embedded_app app_name);
     Launch.add_unmap_embed
      (fun () ->
        (* prerr_endline ("Unmapping " ^ app_name); *)
        unmap_embedded_app app_name)
  | Ephemeral ->
     Launch.add_embed
      (fun () ->
        (* prerr_endline ("Launching ephemeral app " ^ app_name); *)
        raw_embed_app command app_mode app_name width height x gry);;

(* Kill the process and close the associated window. *)
let unembed_app app =
  (* prerr_endline (Printf.sprintf "kill_app (pid=%d, window=%s)" pid wid); *)
  begin
    try Hashtbl.remove app_table app.app_name with
    | _ ->
       Misc.warning
         (Printf.sprintf "kill_app failed to remove application %s..."
            app.app_name)
  end;
  (* Fake apps cannot be killed! *)
  if app.app_mode <> Fake then begin
    begin try Unix.kill app.app_pid Sys.sigquit with _ -> 
      (* prerr_endline
         (Printf.sprintf
            "kill_app (pid=%d,window=%s): process already dead" pid wid); *)
      ()
    end;
    while
      try
        let pid', _ = Unix.waitpid [Unix.WNOHANG] 0 in
        pid' <> 0
      with
      | Unix.Unix_error(Unix.ECHILD, _, _) -> false
    do () done;
    (* prerr_endline (Printf.sprintf "kill_app (pid=%d, window=%s)" pid wid); *)
  end;
  (* if this is the forked process, do not close the window!!! *)
  if Unix.getpid () = Launch.advi_process
  then GraphicsY11.close_subwindow app.app_wid;;

let unembed_apps_with_mode mode =
  (* begin match mode with
  | Fake -> prerr_endline "Killing fake apps"
  | Raw -> prerr_endline "Killing raw apps"
  | Persistent -> prerr_endline "Killing persistent apps"
  | Sticky -> prerr_endline "Killing sticky apps"
  | Ephemeral -> prerr_endline "Killing ephemeral apps"
  end; *)
  let to_be_removed =
    hashtbl_find_all app_table (fun app -> app.app_mode = mode) in
  List.iter unembed_app to_be_removed;;

let signal_app signal app =
  (* prerr_endline
    (Printf.sprintf
      "signal_app (pid=%d, window=%s) signal=%i killing=%b kill is %i"
      app.app_pid app.app_wid sig_val (sig_val = Sys.sigquit) Sys.sigquit); *)
  if signal = Sys.sigquit then unembed_app app else
  try Unix.kill app.app_pid signal
  with _ ->
    (* prerr_endline
        (Printf.sprintf
          "signal_app (pid=%d, window=%s) signal=%i: cannot signal process"
          pid wid signal); *)
    ();;

let kill_embedded_app signal app_name =
  (* prerr_endline
   (Printf.sprintf
     "kill_embedded_app (signal=%i app_name=%s)"
     signal app_name); *)
  try
    let app = find_embedded_app app_name in
    signal_app signal app with
  | Not_found ->
      Misc.warning (Printf.sprintf "application %s is not running" app_name);;

let kill_all_embedded_app signal app_name =
  (* prerr_endline
   (Printf.sprintf
     "kill_all_embedded_app (signal=%i app_name=%s)"
     signal app_name); *)
  let apps = find_all_embedded_app app_name in
  List.iter (signal_app signal) apps;;

let kill_ephemeral_apps () =
  unembed_apps_with_mode Ephemeral;;

let kill_persistent_apps () =
  Launch.unmap_persistent_apps ();
  unembed_apps_with_mode Sticky;
  unembed_apps_with_mode Persistent;;
  unembed_apps_with_mode Raw;;

let kill_all_embedded_apps () =
  kill_ephemeral_apps ();
  kill_persistent_apps ();;
