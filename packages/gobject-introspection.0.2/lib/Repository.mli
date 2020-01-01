(*
 * Copyright 2017-2019 Cedric LE MOIGNE, cedlemo@gmx.com
 * This file is part of OCaml-GObject-Introspection.
 *
 * OCaml-GObject-Introspection is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * OCaml-GObject-Introspection is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with OCaml-GObject-Introspection.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** Repository — GObject Introspection repository manager module
  Repository is used to manage repositories of namespaces. Namespaces are
  represented on disk by type libraries (.typelib files).
*)

open Ctypes

(** Repository — GObject Introspection repository manager module
  Repository is used to manage repositories of namespaces. Namespaces are
  represented on disk by type libraries (.typelib files).
*)

type repository

type typelib

(** Returns the singleton process-global default Repository. *)
val get_default:
  unit -> repository

(** Force the namespace namespace_ to be loaded if it isn't already. If
    namespace_ is not loaded, this function will search for a ".typelib" file
    using the repository search path. In addition, a version version of
    namespace may be specified. If version is not specified, the latest will be
    used).
 *)
val require:
  ?repository:repository -> string -> ?version:string -> unit -> (typelib, string) result

(** Return the list of currently loaded namespaces. *)
val get_loaded_namespaces:
  ?repository:repository -> unit -> string list

(** Return an list of all (transitive) versioned dependencies for namespace_ .
    Returned strings are of the form namespace-version.
    Note: namespace_ must have already been loaded using a function such as
    Repository.require before calling this function. To get only the immediate
    dependencies for namespace_ , use Repository.get_immediate_dependencies.
 *)
val get_dependencies:
  ?repository:repository -> string -> string list

(** This function returns the "C prefix", or the C level namespace associated
    with the given introspection namespace. Each C symbol starts with this
    prefix, as well each GType in the library.
    Note: The namespace must have already been loaded using a function such as
    Repository.require before calling this function. *)
val get_c_prefix:
  ?repository:repository -> string -> string

(** This function returns the loaded version associated with the given
    namespace namespace_ .
    Note: The namespace must have already been loaded using a function such as
    Repository.require before calling this function. *)
val get_version:
  ?repository:repository -> string -> string

(** If namespace namespace_ is loaded, return the full path to the .typelib
    file it was loaded from. If the typelib for namespace namespace_ was
    included in a shared library, return the special string "<builtin>". *)
val get_typelib_path:
  ?repository:repository -> string -> string

(** Obtain an unordered list of versions (either currently loaded or available)
    for namespace_ in this repository . *)
val enumerate_versions:
  ?repository:repository -> string -> string list

(** Returns the current search path Repository will use when loading typelib
    files. The list is internal to GIRespository and should not be freed, nor
    should its string elements. *)
val get_search_path:
  unit -> string list

(** Prepends directory to the typelib search path. See
    Repository.get_search_path. *)
val prepend_search_path:
  string -> unit

(** Searches for a particular entry in a namespace. Before calling this
    function for a particular namespace, you must call Repository.require
    once to load the namespace, or otherwise ensure the namespace has already
    been loaded.*)
val find_by_name:
  ?repository:repository -> string -> string -> Base_info.t structure ptr option

(** This function returns the number of metadata entries in given namespace
    namespace_ . The namespace must have already been loaded before calling
    this function. *)
val get_n_infos:
  ?repository:repository -> string -> int

(** This function returns a particular metadata entry in the given namespace
    namespace_ . The namespace must have already been loaded before calling
    this function. See Repository.get_n_infos to find the maximum number of
    entries. *)
val get_info:
  ?repository:repository -> string -> int -> Base_info.t structure ptr

(** This function returns a comma-separated list of paths to the shared C
    libraries associated with the given namespace namespace_ . There may be no
    shared library path associated, in which case this function will return
    NULL.
    Note: The namespace must have already been loaded using a function such as
    Repository.require before calling this function. *)
val get_shared_library:
  ?repository:repository -> string -> string option

(** Prepends directory to the search path that is used to search shared
    libraries referenced by imported namespaces. Multiple calls to this
    function all contribute to the final list of paths. The list of paths is
    unique and shared for all Repository instances across the process, but it
    doesn't affect namespaces imported before the call.
    If the library is not found in the directories configured in this way,
    loading will fall back to the system library path (ie. LD_LIBRARY_PATH and
    DT_RPATH in ELF systems). See the documentation of your dynamic linker for
    full details. *)
val prepend_library_path:
  string -> unit

type gtype = int64
val gtype: int64 typ

 (** Searches all loaded namespaces for a particular #GType.  Note that
     in order to locate the metadata, the namespace corresponding to
     the type must first have been loaded.  There is currently no
     mechanism for determining the namespace which corresponds to an
     arbitrary GType - thus, this function will operate most reliably
     when you know the GType to originate from be from a loaded namespace. *)
val find_by_gtype:
  ?repository:repository -> gtype -> Base_info.t structure ptr option

(*
   gchar **	g_irepository_get_immediate_dependencies ()
   TODO: GOptionGroup *	g_irepository_get_option_group ()
   TODO: const char *	g_irepository_load_typelib ()
   gboolean	g_irepository_is_registered ()
   TODO: GITypelib *	g_irepository_require_private ()
   TODO: Enum_info *	g_irepository_find_by_error_domain ()
   gboolean	g_irepository_dump ()
   void	gi_cclosure_marshal_generic ()
*)
