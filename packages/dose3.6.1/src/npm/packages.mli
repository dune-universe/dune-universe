open Dose_common

type request =
  { install : Dose_pef.Packages_types.vpkg list;
    remove : Dose_pef.Packages_types.vpkg list;
    upgrade : Dose_pef.Packages_types.vpkg list;
    preferences : string
  }

val default_request : request

val parse_request_stanza : Format822.stanza -> request

val parse_dependlist : Format822.field -> Dose_pef.Packages_types.vpkgformula

val parse_depends : Format822.field -> Dose_pef.Packages_types.vpkgformula

val parse_depend : Format822.field -> Dose_pef.Packages_types.vpkg list list

class package :
  ?name:string * Dose_pef.Packages_types.name option
  -> ?version:string * Dose_pef.Packages_types.version option
  -> ?depends:string * Dose_pef.Packages_types.vpkgformula option
  -> ?conflicts:string * Dose_pef.Packages_types.vpkglist option
  -> ?provides:string * Dose_pef.Packages_types.vpkglist option
  -> ?extras:
       (string * Dose_pef.Packages.parse_extras_f option) list
       * (string * string) list option
  -> Format822.stanza
  -> object ('a)
       method name : Dose_pef.Packages_types.name

       method version : Dose_pef.Packages_types.version

       method conflicts : Dose_pef.Packages_types.vpkglist

       method depends : Dose_pef.Packages_types.vpkgformula

       method provides : Dose_pef.Packages_types.vpkglist

       method installed : Dose_pef.Packages_types.installed

       method recommends : Dose_pef.Packages_types.vpkgformula

       method extras : (string * string) list

       method get_extra : string -> string

       method set_extras : (string * string) list -> 'a

       method set_installed : Dose_pef.Packages_types.installed -> 'a

       method add_extra : string -> string -> 'a

       method pp : out_channel -> unit
     end

val parse_package_stanza :
  ?extras:(string * Dose_pef.Packages.parse_extras_f option) list ->
  Format822.stanza ->
  package option

val packages_parser :
  ?request:bool ->
  request * package list ->
  Format822.f822_parser ->
  request * package list

val input_raw_in : IO.input -> request * package list

val input_raw : string -> request * package list
