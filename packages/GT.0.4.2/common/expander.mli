(*
 * Generic Transformers PPX syntax extension.
 * Copyright (C) 2016-2019
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

(** Expander: base module for Generic Tranformers that utilize plugins. *)

open Ppxlib

(** Arguments of plugin are empty *)
type config_plugin = Skip | Use of (longident * expression) list

module Make : functor (Helpers: GTHELPERS_sig.S) -> sig

open Helpers

val str_type_decl_many_plugins: loc:loc ->
  Str.t list ->
  (Base.string * config_plugin) list ->
  Ppxlib.rec_flag * Ppxlib.type_declaration list ->
  Str.t HelpersBase.List.t

val sig_type_decl_many_plugins: loc:loc ->
  Sig.t list ->
  (Base.string * config_plugin) list ->
  Ppxlib.rec_flag * Ppxlib.type_declaration list ->
  Sig.t HelpersBase.List.t
(*
val str_type_ext_many_plugins: loc:loc ->
  Str.t list ->
  (Base.string * config_plugin) list ->
  Ppxlib.type_extension ->
  Str.t HelpersBase.List.t
*)

end

(** Registers a plugin. See {! Plugin_intf.PluginRes } for plugin interface. *)
val register_plugin: string -> (module Plugin_intf.MAKE) -> unit

val get_registered_plugins: unit -> string list
