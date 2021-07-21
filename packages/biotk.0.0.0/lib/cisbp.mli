module TF_information : sig
  type item = {
    tf_id : string ;
    family_id : string ;
    tsource_id : string ;
    motif_id : string option ;
    msource_id : string option ;
    dbid : string ;
    tf_name : string ;
    tf_species : string ;
    tf_status : string ;
    family_name : string ;
    dbds : string list ;
    dbd_count : int ;
    cutoff : float ;
    dbid2 : string option ;
    motif_type : string option ;
    msource_identifier : string option ;
    msource_type : string option ;
    msource_author : string option ;
    msource_year : int option ;
    pmid : string option ;
    msource_version : string option ;
    tfsource_name : string ;
    tfsource_url : string ;
    tfsource_year : int ;
    tfsource_month : string ;
    tfsource_day : int ;
  }
  type t = item list

  val from_file : string -> t
end

module Motif : sig
  type t = float array array

  val from_file : string -> t
  val read_all_in_dir : string -> (string * t) list

  val pwm : t -> Pwm.t
end

val pwm_archive_url : string
val tf_information_archive_url : string
