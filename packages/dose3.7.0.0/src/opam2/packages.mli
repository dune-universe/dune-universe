open Dose_extra

type request =
  { install : Dose_pef.Packages_types.vpkg list;
    remove : Dose_pef.Packages_types.vpkg list;
    upgrade : Dose_pef.Packages_types.vpkg list;
    dist_upgrade : bool;
    switch : string;
    switches : string list;
    profiles : string list;
    preferences : string
  }

type options =
  Dose_pef.Packages_types.architecture
  * Dose_pef.Packages_types.architecture list
  * Dose_pef.Packages_types.buildprofile list

val default_request : request

val parse_request_stanza : Format822.stanza -> request

val vpkglist_filter :
  options ->
  Dose_pef.Packages_types.builddepslist ->
  Dose_pef.Packages_types.vpkglist

val vpkgformula_filter :
  options ->
  Dose_pef.Packages_types.builddepsformula ->
  Dose_pef.Packages_types.vpkgformula

class package :
  ?name:string * Dose_pef.Packages_types.name option
  -> ?version:string * Dose_pef.Packages_types.version option
  -> ?depends:string * Dose_pef.Packages_types.vpkgformula option
  -> ?conflicts:string * Dose_pef.Packages_types.vpkglist option
  -> ?provides:string * Dose_pef.Packages_types.vpkglist option
  -> ?depopts:string * Dose_pef.Packages_types.vpkgformula option
  -> ?switch:string * string list option
  -> ?installedlist:string * string list option
  -> ?pinnedlist:string * string list option
  -> ?baselist:string * string list option
  -> ?extras:
       (string * Dose_pef.Packages.parse_extras_f option) list
       * (string * string) list option
  -> Format822.stanza
  -> object ('a)
       method name : Dose_pef.Packages_types.name

       method version : Dose_pef.Packages_types.version

       method switch : string list

       method installed : Dose_pef.Packages_types.installed

       method installedlist : string list

       method pinnedlist : string list

       method baselist : string list

       method conflicts : Dose_pef.Packages_types.vpkglist

       method depends : Dose_pef.Packages_types.vpkgformula

       method depopts : Dose_pef.Packages_types.vpkgformula

       method provides : Dose_pef.Packages_types.vpkglist

       method recommends : Dose_pef.Packages_types.vpkgformula

       method extras : (string * string) list

       method get_extra : string -> string

       method set_extras : (string * string) list -> 'a

       method set_installed : Dose_pef.Packages_types.installed -> 'a

       method add_extra : string -> string -> 'a

       method pp : out_channel -> unit
     end

val parse_package_stanza :
  options ->
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
