(******************************************************************************)
(*  This file is part of the Dose library http://www.irill.org/software/dose  *)
(*                                                                            *)
(*  Copyright (C) 2010,2011 Ralf Treinen <ralf.treinen@pps.jussieu.fr>        *)
(*                                                                            *)
(*  This library is free software: you can redistribute it and/or modify      *)
(*  it under the terms of the GNU Lesser General Public License as            *)
(*  published by the Free Software Foundation, either version 3 of the        *)
(*  License, or (at your option) any later version.  A special linking        *)
(*  exception to the GNU Lesser General Public License applies to this        *)
(*  library, see the COPYING file for more information.                       *)
(*                                                                            *)
(*  Work developed with the support of the Mancoosi Project                   *)
(*  http://www.mancoosi.org                                                   *)
(*                                                                            *)
(******************************************************************************)

module Pcre = Re_pcre
open ExtLib
open Dose_common

include Util.Logging (struct
  let label = "dose_deb.architecture"
end)

(* first column of /usr/share/dpkg/cputable *)
(* the line numbers correspond to the line numbers in /usr/share/dpkg/cputable
 * to be quickly able to find changes *)
let cpulist =
  ref
    [ (* lines 19..25 *)
      "i386";
      "ia64";
      "alpha";
      "amd64";
      "armeb";
      "arm";
      "arm64";
      (* lines 26..31 *)
      "avr32";
      "hppa";
      "m32r";
      "m68k";
      "mips";
      "mipsel";
      (* lines 32..36 *)
      "mipsr6";
      "mipsr6el";
      "mips64";
      "mips64el";
      "mips64r6";
      (* lines 37..41 *)
      "mips64r6el";
      "nios2";
      "or1k";
      "powerpc";
      "powerpcel";
      (* lines 42..48 *)
      "ppc64";
      "ppc64el";
      "s390";
      "s390x";
      "sh3";
      "sh3eb";
      "sh4";
      (* lines 49..52 *)
      "sh4eb";
      "sparc";
      "sparc64";
      "tilegx" ]

(* from /usr/share/dpkg/tupletable
 *
 * the line numbers correspond to the line numbers in
 * /usr/share/dpkg/tupletable to be quickly able to find changes
 *
 *   debian tuple (abi,libc,os,cpu)      debian arch *)
let tupletable =
  ref
    [ (("eabi", "uclibc", "linux", "arm"), "uclibc-linux-armel");
      (* line 8  *)
      (("base", "uclibc", "linux", "<cpu>"), "uclibc-linux-<cpu>");
      (("eabihf", "musl", "linux", "arm"), "musl-linux-armhf");
      (* line 10 *)
      (("base", "musl", "linux", "<cpu>"), "musl-linux-<cpu>");
      (("eabihf", "gnu", "linux", "arm"), "armhf");
      (("eabi", "gnu", "linux", "arm"), "armel");
      (("abin32", "gnu", "linux", "mips64r6el"), "mipsn32r6el");
      (("abin32", "gnu", "linux", "mips64r6"), "mipsn32r6");
      (* line 15 *)
      (("abin32", "gnu", "linux", "mips64el"), "mipsn32el");
      (("abin32", "gnu", "linux", "mips64"), "mipsn32");
      (("abi64", "gnu", "linux", "mips64r6el"), "mips64r6el");
      (("abi64", "gnu", "linux", "mips64r6"), "mips64r6");
      (("abi64", "gnu", "linux", "mips64el"), "mips64el");
      (* line 20 *)
      (("abi64", "gnu", "linux", "mips64"), "mips64");
      (("spe", "gnu", "linux", "powerpc"), "powerpcspe");
      (("x32", "gnu", "linux", "amd64"), "x32");
      (("base", "gnu", "linux", "<cpu>"), "<cpu>");
      (("eabihf", "gnu", "kfreebsd", "arm"), "kfreebsd-armhf");
      (* line 25 *)
      (("base", "gnu", "kfreebsd", "<cpu>"), "kfreebsd-<cpu>");
      (("base", "gnu", "knetbsd", "<cpu>"), "knetbsd-<cpu>");
      (("base", "gnu", "kopensolaris", "<cpu>"), "kopensolaris-<cpu>");
      (("base", "gnu", "hurd", "<cpu>"), "hurd-<cpu>");
      (("base", "bsd", "dragonflybsd", "<cpu>"), "dragonflybsd-<cpu>");
      (* line 30 *)
      (("base", "bsd", "freebsd", "<cpu>"), "freebsd-<cpu>");
      (("base", "bsd", "openbsd", "<cpu>"), "openbsd-<cpu>");
      (("base", "bsd", "netbsd", "<cpu>"), "netbsd-<cpu>");
      (("base", "bsd", "darwin", "<cpu>"), "darwin-<cpu>");
      (("base", "sysv", "aix", "<cpu>"), "aix-<cpu>");
      (* line 35 *)
      (("base", "sysv", "solaris", "<cpu>"), "solaris-<cpu>");
      (("eabi", "uclibc", "uclinux", "arm"), "uclinux-armel");
      (("base", "uclibc", "uclinux", "<cpu>"), "uclinux-<cpu>");
      (("base", "tos", "mint", "m68k"), "mint-m68k");
      (("base", "gnu", "linux", "<cpu>"), "linux-<cpu>")
      (* this entry is not from /usr/share/dpkg/tupletable *)
      (* the "linux-" prefix is commented in scripts/Dpkg/Arch.pm with "XXX: Might disappear in the future, not sure yet." *)
    ]

let debarch_to_debtuple =
  Hashtbl.create (List.length !tupletable * List.length !cpulist)

let tupletable_done = ref false

let mangle_cpu_placeholder ((abi, libc, os, cpu), debarch) =
  if cpu = "<cpu>" then
    List.iter
      (fun c ->
        let dt = (abi, libc, os, c) in
        let (_, da) = String.replace ~str:debarch ~sub:"<cpu>" ~by:c in
        Hashtbl.replace debarch_to_debtuple da dt)
      !cpulist
  else Hashtbl.replace debarch_to_debtuple debarch (abi, libc, os, cpu)

let read_tupletable ?(ttfile = None) ?(ctfile = None) () =
  if !tupletable_done && ttfile = None && ctfile = None then ()
  else (
    (* if cputable file was given, overwrite built-in table *)
    (match ctfile with
    | Some fn ->
        cpulist := [] ;
        let ic = open_in fn in
        if input_line ic <> "# Version=1.0" then
          fatal "Require cputable version 1.0" ;
        (* to stay most compatible with dpkg, it would be best to use its
         * regex from from scripts/Dpkg/Arch.pm to parse this file.
         * Unfortunately Re.pcre doesnt support look-ahead/look-behind
         * assertions *)
        let aux line =
          if line.[0] = '#' || not (String.contains line '\t') then ()
          else
            let spacei = String.index line '\t' in
            let cpu = String.sub line 0 spacei in
            if not (List.mem cpu !cpulist) then cpulist := cpu :: !cpulist
        in
        List.iter aux (Std.input_list ic) ;
        close_in ic
    | None -> ()) ;
    (* if tupletable was given, overwrite built-in table, otherwise parse
     * built-in table *)
    (match ttfile with
    | Some fn ->
        (* this is an implicit assumption of dpkg *)
        mangle_cpu_placeholder (("base", "gnu", "linux", "<cpu>"), "linux-<cpu>") ;
        let ic = open_in fn in
        if input_line ic <> "# Version=1.0" then
          fatal "Require tupletable version 1.0" ;
        (* to stay most compatible with dpkg, it would be best to use its
         * regex from from scripts/Dpkg/Arch.pm to parse this file.
         * Unfortunately Re.pcre doesnt support look-ahead/look-behind
         * assertions *)
        let aux line =
          if line.[0] = '#' || not (String.contains line '\t') then ()
          else
            let spaceli = String.index line '\t' in
            let spaceri = String.rindex line '\t' in
            let debtuple = String.sub line 0 spaceli in
            let debarch =
              String.sub line (spaceri + 1) (String.length line - spaceri - 1)
            in
            match String.nsplit debtuple "-" with
            | [abi; libc; os; cpu] ->
                mangle_cpu_placeholder ((abi, libc, os, cpu), debarch)
            | _ -> fatal "Cannot parse debtuple: %s" debtuple
        in
        List.iter aux (Std.input_list ic) ;
        close_in ic
    | None -> List.iter mangle_cpu_placeholder !tupletable) ;
    tupletable_done := true)

(* this function performs what debarch_is form libdpkg-perl does *)
let src_matches_arch alias real =
  read_tupletable () ;
  if alias = real || alias = "any" || alias = "all" then true
  else
    let real = Hashtbl.find_option debarch_to_debtuple real in
    (* see libdpkg-perl function debwildcard_to_debtuple *)
    let alias =
      match String.nsplit alias "-" with
      | ["any"; libc; os; cpu] -> Some ("any", libc, os, cpu)
      | [abi; "any"; os; cpu] -> Some (abi, "any", os, cpu)
      | [abi; libc; "any"; cpu] -> Some (abi, libc, "any", cpu)
      | [abi; libc; os; "any"] -> Some (abi, libc, os, "any")
      | ["any"; os; cpu] -> Some ("any", "any", os, cpu)
      | [libc; "any"; cpu] -> Some ("any", libc, "any", cpu)
      | [libc; os; "any"] -> Some ("any", libc, os, "any")
      | ["any"; cpu] -> Some ("any", "any", "any", cpu)
      | [os; "any"] -> Some ("any", "any", os, "any")
      | ["any"] -> Some ("any", "any", "any", "any")
      | _ ->
          (* only look up in the table if none of the parts is "any" *)
          Hashtbl.find_option debarch_to_debtuple alias
    in
    match (real, alias) with
    | (Some (r1, r2, r3, r4), Some (a1, a2, a3, a4)) ->
        (a1 = r1 || a1 = "any")
        && (a2 = r2 || a2 = "any")
        && (a3 = r3 || a3 = "any")
        && (a4 = r4 || a4 = "any")
    | _ -> false
