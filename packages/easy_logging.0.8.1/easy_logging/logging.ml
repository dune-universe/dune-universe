(*
    This file is part of easy_logging.

    easy_logging is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    easy_logging is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with easy_logging.  If not, see <https://www.gnu.org/licenses/>.
*)


include Logging_types
open CalendarLib


let debug = ref false

class logger
    ?parent:(parent=None)
    (name: string)
  =
  object(self)

    val name = name

    val mutable level : level = NoLevel

    val mutable handlers : Handlers.t list = []

    val parent : logger option = parent

    val mutable propagate = true

    val mutable tag_generators : (unit -> string) list = []

    method name = name
    method set_level new_level = level <- new_level
    method add_handler h = handlers <- h::handlers

    method get_handlers = handlers
    method set_handlers hs = handlers <- hs

    method set_propagate p = propagate <- p

    method effective_level : level =
      match level, parent  with
      | NoLevel, None  -> NoLevel
      | NoLevel, Some p -> p#effective_level
      | l,_ -> l

    method internal_level = level

    method get_handlers_propagate =
      if !debug
      then
        print_endline (Printf.sprintf "[%s] returning (%i) handlers" name
                         (List.length handlers));
      match propagate, parent with
      | true, Some p -> handlers @ p#get_handlers_propagate
      | _ -> handlers

    method add_tag_generator t  =
      tag_generators <- t :: tag_generators

    method private treat_msg : 'a. ('a -> string) -> string list -> level -> 'a -> unit
      = fun unwrap_fun tags msg_level msg ->

        if !debug
        then
          print_endline ( Printf.sprintf "[%s]/%s -- Treating msg \"%s\" at level %s"
                            name (show_level level)
                            (unwrap_fun msg) (show_level msg_level));

        let generated_tags = List.map (fun x -> x ()) tag_generators in
        let item : log_item= {
          level = msg_level;
          logger_name = name;
          msg = unwrap_fun msg;
          tags=generated_tags @ tags;
          timestamp = Fcalendar.to_unixfloat @@ Fcalendar.now ()
        } in
        List.iter (fun handler ->
            Handlers.apply handler item)
          self#get_handlers_propagate

    method private _log_msg : 'a. ('a -> string) -> string list -> level -> 'a -> unit
      = fun unwrap_fun tags msg_level msg ->
        if msg_level >= self#effective_level
        then
          self#treat_msg unwrap_fun tags msg_level msg
        else
          ()

    method private _flog_msg : 'a. string list -> level -> ('a, unit, string, unit) format4 -> 'a
      =  fun tags msg_level ->
        if msg_level >= self#effective_level
        then
          Printf.ksprintf (
            self#treat_msg (fun x -> x) tags msg_level)
        else Printf.ifprintf ()


    method flash : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
      = fun ?tags:(tags=[]) -> self#_flog_msg tags Flash
    method error : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
      = fun ?tags:(tags=[]) -> self#_flog_msg tags Error
    method warning : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
      = fun ?tags:(tags=[]) -> self#_flog_msg tags Warning
    method info : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
      = fun ?tags:(tags=[]) -> self#_flog_msg tags Info
    method trace : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
      = fun ?tags:(tags=[]) -> self#_flog_msg tags Trace
    method debug : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
      = fun ?tags:(tags=[]) -> self#_flog_msg tags Debug


    method lflash ?tags:(tags=[]) = self#_log_msg Lazy.force tags Flash
    method lerror ?tags:(tags=[]) = self#_log_msg Lazy.force tags Error
    method lwarning ?tags:(tags=[]) = self#_log_msg Lazy.force tags Warning
    method linfo ?tags:(tags=[]) =  self#_log_msg Lazy.force tags Info
    method ltrace ?tags:(tags=[]) =  self#_log_msg Lazy.force tags Trace
    method ldebug ?tags:(tags=[]) = self#_log_msg Lazy.force tags Debug


    method sflash ?tags:(tags=[]) = self#_log_msg (fun x -> x) tags Flash
    method serror ?tags:(tags=[]) = self#_log_msg (fun x -> x) tags Error
    method swarning ?tags:(tags=[]) = self#_log_msg (fun x -> x) tags Warning
    method sinfo ?tags:(tags=[]) =  self#_log_msg (fun x -> x) tags Info
    method strace ?tags:(tags=[]) =  self#_log_msg (fun x -> x) tags Trace
    method sdebug ?tags:(tags=[]) = self#_log_msg (fun x -> x) tags Debug
  end

let root_logger = new logger "root"

module Infra =
  Logging_infra.MakeTree(
  struct
    type t = logger
    let make (n:string) parent = new logger ~parent n
    let root = root_logger
  end)


let handlers_config = ref Handlers.default_config
let set_handlers_config c = handlers_config := c

let get_logger name =
  Infra.get name

let make_logger ?propagate:(propagate=true) name lvl hdescs  =
  let l = Infra.get name in
  l#set_level lvl;
  l#set_propagate propagate;
  List.iter (fun hdesc -> l#add_handler (Handlers.make ~config:(!handlers_config) hdesc)) hdescs;
  l


let rec _tree_to_yojson tree =
  match tree with
  | Infra.Node (logger, children_map) ->
    let children_json = Infra.SMap.to_seq children_map
                        |> Seq.map (fun (a,b) -> _tree_to_yojson b)
                        |> List.of_seq in

    `Assoc ["name", `String logger#name;
            "level", `String (show_level logger#internal_level);
            "children", `List children_json]
let tree_to_yojson () =
  _tree_to_yojson Infra.internal.data

