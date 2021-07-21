open Core

module Tsv = struct
  type t = {
    id : string ;
    chr : string ;
    lo : int ;
    hi : int ;
    strand : string ;
    length : int ;
    count : int ;
  } [@@deriving fields, csv]

  let load fn =
    In_channel.read_lines fn
    |> Fn.flip List.drop 2
    |> List.map ~f:(fun l -> try t_of_row (String.split ~on:'\t' l) with _ -> failwith l)

  let loc { chr ; lo ; hi ; _ } = { GLoc.chr ; lo ; hi }
end

module Summary = struct
  type t = {
    assigned : int ;
    unassigned_unmapped : int ;
    unassigned_mappingquality : int ;
    unassigned_chimera : int ;
    unassigned_fragmentlength : int ;
    unassigned_duplicate : int ;
    unassigned_multimapping : int ;
    unassigned_secondary : int ;
    unassigned_nonjunction : int ;
    unassigned_nofeatures : int ;
    unassigned_overlapping_length : int ;
    unassigned_ambiguity : int ;
  }

  let load fn =
    match In_channel.read_lines fn
          |> List.map ~f:(String.split ~on:'\t') with
    | _ ::
      [ "Assigned" ; assigned ] ::
      [ "Unassigned_Unmapped" ; unassigned_unmapped ] ::
      [ "Unassigned_MappingQuality" ; unassigned_mappingquality ] ::
      [ "Unassigned_Chimera" ; unassigned_chimera ] ::
      [ "Unassigned_FragmentLength" ; unassigned_fragmentlength ] ::
      [ "Unassigned_Duplicate" ; unassigned_duplicate ] ::
      [ "Unassigned_MultiMapping" ; unassigned_multimapping ] ::
      [ "Unassigned_Secondary" ; unassigned_secondary ] ::
      [ "Unassigned_Nonjunction" ; unassigned_nonjunction ] ::
      [ "Unassigned_NoFeatures" ; unassigned_nofeatures ] ::
      [ "Unassigned_Overlapping_Length" ; unassigned_overlapping_length ] ::
      [ "Unassigned_Ambiguity" ; unassigned_ambiguity ] :: [] ->
      { assigned = Int.of_string assigned ;
        unassigned_unmapped = Int.of_string unassigned_unmapped ;
        unassigned_mappingquality = Int.of_string unassigned_mappingquality ;
        unassigned_chimera = Int.of_string unassigned_chimera ;
        unassigned_fragmentlength = Int.of_string unassigned_fragmentlength ;
        unassigned_duplicate = Int.of_string unassigned_duplicate ;
        unassigned_multimapping = Int.of_string unassigned_multimapping ;
        unassigned_secondary = Int.of_string unassigned_secondary ;
        unassigned_nonjunction = Int.of_string unassigned_nonjunction ;
        unassigned_nofeatures = Int.of_string unassigned_nofeatures ;
        unassigned_overlapping_length = Int.of_string unassigned_overlapping_length ;
        unassigned_ambiguity = Int.of_string unassigned_ambiguity }
    | _ -> failwith "FeatureCounts.Summary.load: unexpected syntax"

end
