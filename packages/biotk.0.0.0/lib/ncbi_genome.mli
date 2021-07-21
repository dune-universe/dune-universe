module Assembly_summary : sig
  type t = {
    assembly_accession : string ;
    bioproject : string ;
    biosample : string ;
    wgs_master : string ;
    refseq_category : string ;
    taxid : string ;
    species_taxid : string ;
    organism_name : string ;
    infraspecific_name : string ;
    isolate : string ;
    version_status : string ;
    assembly_level : string ;
    release_type : string ;
    genome_rep : string ;
    seq_rel_date : string ;
    asm_name : string ;
    submitter : string ;
    gbrs_paired_asm : string ;
    paired_asm_comp : string ;
    ftp_path : string ;
    excluded_from_refseq : string ;
    relation_to_type_material : string ;
  }
  [@@deriving csv]
end

val refseq_assembly_summary_url : string
