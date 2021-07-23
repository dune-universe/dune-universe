open Dose_extra
open Dose_pef

val parse_multiarch : string * ('a * string) -> Packages_types.multiarch

val parse_source : Format822.field -> Packages_types.source

val parse_binarylist : Format822.field -> Packages_types.vpkglist

class package :
  ?name:string * Packages_types.name option
  -> ?version:string * Packages_types.version option
  -> ?depends:string * Packages_types.vpkgformula option
  -> ?conflicts:string * Packages_types.vpkglist option
  -> ?provides:string * Packages_types.vpkglist option
  -> ?recommends:string * Packages_types.vpkgformula option
  -> ?architecture:string * Packages_types.architecture option
  -> ?multiarch:string * Packages_types.multiarch option
  -> ?source:string * Packages_types.source option
  -> ?essential:string * bool option
  -> ?extra_source_only:string * bool option
  -> ?priority:string * string option
  -> ?pre_depends:string * Packages_types.vpkgformula option
  -> ?suggests:string * Packages_types.vpkgformula option
  -> ?enhances:string * Packages_types.vpkgformula option
  -> ?breaks:string * Packages_types.vpkglist option
  -> ?replaces:string * Packages_types.vpkglist option
  -> ?extras:
       (string * Packages.parse_extras_f option) list
       * (string * string) list option
  -> Format822.stanza
  -> object ('a)
       (** {4 Access methods } *)
       method name : Packages_types.name

       method version : Packages_types.version

       method conflicts : Packages_types.vpkglist

       method depends : Packages_types.vpkgformula

       method provides : Packages_types.vpkglist

       method recommends : Packages_types.vpkgformula

       method installed : Packages_types.installed

       method extras : (string * string) list

       (** {4 Debian specific methods } *)
       method architecture : Packages_types.architecture

       method breaks : Packages_types.vpkglist

       method enhances : Packages_types.vpkgformula

       method essential : bool

       method extra_source_only : bool

       method extras : (string * string) list

       method multiarch : Packages_types.multiarch

       method pre_depends : Packages_types.vpkgformula

       method priority : string

       method replaces : Packages_types.vpkglist

       method source : Packages_types.name * Packages_types.version option

       method suggests : Packages_types.vpkgformula

       (** {4 low level val } *)
       val name : string * Packages_types.name

       val version : string * Packages_types.version

       val conflicts : string * Packages_types.vpkglist

       val depends : string * Packages_types.vpkgformula

       val provides : string * Packages_types.vpkglist

       val recommends : string * Packages_types.vpkgformula

       val installed : string * Packages_types.installed

       (** {4 Debian specific val } *)
       val architecture : string * Packages_types.architecture

       val breaks : string * Packages_types.vpkglist

       val enhances : string * Packages_types.vpkgformula

       val essential : string * bool

       val extra_source_only : string * bool

       val multiarch : string * Packages_types.multiarch

       val pre_depends : string * Packages_types.vpkgformula

       val priority : string * string

       val replaces : string * Packages_types.vpkglist

       val source :
         string * (Packages_types.name * Packages_types.version option)

       val suggests : string * Packages_types.vpkgformula

       (** {4 get/set specific fields of the object} *)
       method get_extra : string -> string

       method add_extra : string -> string -> 'a

       method set_extras : (string * string) list -> 'a

       method set_installed : Packages_types.installed -> 'a

       (** {4 Debian specific methods } *)
       method set_essential : bool -> 'a

       method set_multiarch : Packages_types.multiarch -> 'a

       (* Print the object as a 822 stanza to the given channel *)
       method pp : out_channel -> unit

       method pp : out_channel -> unit
     end

val parse_package_stanza :
  (Format822.stanza -> bool) option ->
  Packages_types.architecture list ->
  (string * Packages.parse_extras_f option) list ->
  Format822.stanza ->
  package option

val parse_packages_in :
  ?filter:(Format822.stanza -> bool) ->
  ?archs:Packages_types.architecture list ->
  ?extras:(string * Packages.parse_extras_f option) list ->
  string ->
  IO.input ->
  package list

val merge : package list -> package list -> package list

val is_installed : package -> bool

val is_on_hold : package -> bool

val default_extras : (string * 'a option) list

val input_raw :
  ?filter:(Format822.stanza -> bool) ->
  ?archs:Packages_types.architecture list ->
  ?extras:(string * Packages.parse_extras_f option) list ->
  string list ->
  package list

val input_raw_in :
  ?filter:(Format822.stanza -> bool) ->
  ?archs:Packages_types.architecture list ->
  ?extras:(string * Packages.parse_extras_f option) list ->
  IO.input ->
  package list
