(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

open ExtLib
open Dose_common

include Util.Logging (struct
  let label = "doseparse.stdDebug"
end)

let enable_debug = function
  | 0 -> () (* only warning messages : default *)
  | 1 -> Util.Info.all_enabled ()
  | 2 ->
      Util.Info.all_enabled () ;
      Util.Notice.all_enabled ()
  | _ ->
      Util.Info.all_enabled () ;
      Util.Notice.all_enabled () ;
      Util.Debug.all_enabled ()

let all_quiet t =
  if t then (
    Util.Info.all_disabled () ;
    Util.Notice.all_disabled () ;
    Util.Warning.all_disabled () ;
    Util.Debug.all_disabled () ;
    List.iter Util.Progress.disable (Util.Progress.available ()))

let enable_bars verbose l = if verbose then List.iter Util.Progress.enable l

let enable_timers verbose l =
  at_exit (Util.Timer.dump Format.err_formatter) ;
  if verbose then List.iter Util.Timer.enable l
