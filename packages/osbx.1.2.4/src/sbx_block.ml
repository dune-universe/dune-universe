open Stdint
open Crcccitt
open Sbx_specs

module Helper : sig
  val pad_header_or_block_string : string      -> int          -> string

  val crc_ccitt_sbx              : ver:version -> input:string -> string

end = struct

  let pad_header_or_block_string (old_string:string) (new_len:int) : string =
    Misc_utils.pad_string ~filler:(Char.chr 0x1a) old_string new_len
  ;;

  let crc_ccitt_sbx ~(ver:version) ~(input:string) : string =
    let res = crc_ccitt_generic_uint16 ~input ~start_val:(ver_to_uint16 ver) in
    Conv_utils.uint16_to_string res
  ;;
end

module Header : sig
  exception Invalid_uid_length
  exception Missing_alt_seq_num
  exception Invalid_bytes
  exception Invalid_seq_num

  type common_fields =
    { signature  : string
    ; version    : version
    ; file_uid   : string
    }

  type t

  type raw_header =
    { version   : version
    ; crc_ccitt : uint16
    ; file_uid  : string
    ; seq_num   : uint32
    }

  val common_fields_to_ver : common_fields             -> version

  val make_common_fields   : ?uid:string               -> version       -> common_fields

  val make_metadata_header : common_fields             -> t

  val make_data_header     : ?seq_num:uint32           -> common_fields      -> t

  val of_string            : string                    -> raw_header

  val to_string            : alt_seq_num:uint32 option -> header:t      -> data:string    -> string

  val header_to_ver        : t -> version

  val header_to_file_uid   : t -> string

  val header_to_seq_num    : t -> uint32 option

  val raw_header_is_meta   : raw_header -> bool

  val raw_header_is_data   : raw_header -> bool

end = struct

  exception Invalid_uid_length
  exception Missing_alt_seq_num
  exception Invalid_bytes
  exception Invalid_seq_num

  type common_fields =
    { signature  : string
    ; version    : version
    ; file_uid   : string
    }

  type t =
      Meta of { common     : common_fields
              }
    | Data of { common  : common_fields
              ; seq_num : uint32 option
              }
              
  let header_to_common (header:t) : common_fields =
    match header with
    | Meta { common; _ } -> common
    | Data { common; _ } -> common
  ;;

  let header_to_signature (header:t) : string  = (header_to_common header).signature;;

  let header_to_ver       (header:t) : version = (header_to_common header).version;;

  let header_to_file_uid  (header:t) : string  = (header_to_common header).file_uid;;

  let header_to_seq_num (header:t) : uint32 option =
    match header with
    | Meta _              -> Some (Uint32.of_int 0)
    | Data { seq_num; _ } -> seq_num
  ;;

  let gen_file_uid () : string =
    let len = sbx_file_uid_len in
    Random_utils.gen_string ~len
  ;;

  let common_fields_to_ver (common:common_fields) : version =
    common.version
  ;;

  let make_common_fields ?(uid:string option) (ver:version) : common_fields =
    let uid : string = match uid with
      | Some x ->
        let len = sbx_file_uid_len in
        if String.length x = len then
          x
        else
          raise Invalid_uid_length
      | None   -> gen_file_uid () in
    { signature = sbx_signature
    ; version   = ver
    ; file_uid  = uid }
  ;;

  let make_metadata_header (common:common_fields) : t =
    Meta { common }
  ;;

  let make_data_header ?(seq_num:uint32 option) (common:common_fields) : t =
    let seq_num =
      match seq_num with
      | None   -> None
      | Some n ->
        if (Uint32.to_int n) = 0 then
          raise Invalid_seq_num
        else
          Some n in
    Data { common; seq_num }
  ;;

  let to_string ~(alt_seq_num:uint32 option) ~(header:t) ~(data:string) : string =
    let seq_num =
      match (alt_seq_num, (header_to_seq_num header)) with
      | (Some _, Some s) -> Some s    (* prefer existing one over provided one *)
      | (None,   Some s) -> Some s
      | (Some s, None)   -> Some s
      | (None,   None)   -> None   in
    match seq_num with
    | Some seq_num ->
      let seq_num_string : string      = Conv_utils.uint32_to_string seq_num in
      let things_to_crc  : string list = [ header_to_file_uid header
                                         ; seq_num_string
                                         ; data
                                         ] in
      let string_to_crc  : string      = String.concat "" things_to_crc in
      let crc_result     : string      = Helper.crc_ccitt_sbx ~ver:(header_to_ver header) ~input:string_to_crc in
      let header_parts   : string list = [ header_to_signature header
                                         ; ver_to_string (header_to_ver header)
                                         ; crc_result
                                         ; header_to_file_uid header
                                         ; seq_num_string
                                         ] in
      String.concat "" header_parts
    | None ->
      raise Missing_alt_seq_num
  ;;

  type raw_header =
    { version   : version
    ; crc_ccitt : uint16
    ; file_uid  : string
    ; seq_num   : uint32
    }

  let make_raw_header version crc_ccitt file_uid seq_num =
    { version
    ; crc_ccitt
    ; file_uid
    ; seq_num
    }
  ;;

  module Parser = struct
    open Angstrom

    let sig_p : string Angstrom.t =
      string "SBx"
    ;;

    let crc_p : Stdint.uint16 Angstrom.t =
      BE.uint16 >>| Uint16.of_int
    ;;

    let uid_p : string Angstrom.t =
      take 6
    ;;

    let seq_p : Stdint.uint32 Angstrom.t =
      BE.int32 >>| Uint32.of_int32
    ;;

    let header_p : raw_header Angstrom.t =
      sig_p *> lift4 make_raw_header Sbx_specs.Parser.ver_p crc_p uid_p seq_p
    ;;
  end

  let of_string (data:string) : raw_header =
    match Angstrom.parse_string Parser.header_p data with
    | Ok header -> header
    | Error _   -> raise Invalid_bytes
  ;;

  let raw_header_is_meta : raw_header -> bool =
    let uint32_0 : uint32 = Uint32.of_int 0 in
    (fun raw_header ->
       raw_header.seq_num = uint32_0
    )
  ;;

  let raw_header_is_data (raw_header:raw_header) : bool =
    not (raw_header_is_meta raw_header)
  ;;
end

module Metadata : sig
  exception Too_much_data of string
  exception Invalid_bytes

  type t =
      FNM of string
    | SNM of string
    | FSZ of uint64
    | FDT of uint64
    | SDT of uint64
    | HSH of Multihash.hash_bytes  (* should ALWAYS store the RAW hash, to bytes should automatically convert it to multihash *)
    | PID of string

  val dedup    : t list      -> t list

  val to_string : ver:version -> fields:t list -> string

  val of_string : string      -> t list

end = struct

  exception Too_much_data of string
  (* exception Invalid_entry of string *)
  exception Invalid_bytes

  type t =
      FNM of string
    | SNM of string
    | FSZ of uint64
    | FDT of uint64
    | SDT of uint64
    | HSH of Multihash.hash_bytes
    | PID of string

  type id =
    [ `FNM
    | `SNM
    | `FSZ
    | `FDT
    | `SDT
    | `HSH
    | `PID
    ]

  let id_to_string (id:id) : string =
    match id with
    | `FNM -> "FNM"
    | `SNM -> "SNM"
    | `FSZ -> "FSZ"
    | `FDT -> "FDT"
    | `SDT -> "SDT"
    | `HSH -> "HSH"
    | `PID -> "PID"
  ;;

  let length_distribution (lst:(id * string) list) : string =
    let rec length_distribution_helper (lst:(id * string) list) (acc:string list) : string =
      match lst with
      | []              -> (String.concat "\n" (List.rev acc))
      | (id, data) :: vs -> let str = Printf.sprintf "id : %s, len : %3d" (id_to_string id) (String.length data) in
        length_distribution_helper vs (str :: acc) in
    let distribution_str = length_distribution_helper lst [] in
    String.concat "\n" ["The length distribution of the metadata:"; distribution_str]
  ;;

  let same_id (a:t) (b:t) : bool =
    match (a, b) with
    | (FNM _, FNM _) -> true
    | (SNM _, SNM _) -> true
    | (FSZ _, FSZ _) -> true
    | (FDT _, FDT _) -> true
    | (SDT _, SDT _) -> true
    | (HSH _, HSH _) -> true
    | (PID _, PID _) -> true
    | (_    , _    ) -> false
  ;;

  let dedup (fields:t list) : t list =
    let rec dedup_internal (acc:t list) (fields:t list) : t list =
      match fields with
      | []      -> acc
      | v :: vs ->
        let new_acc = if List.exists (same_id v) acc then acc else v :: acc in
        dedup_internal new_acc vs in
    dedup_internal [] fields
  ;;

  let single_to_string (entry:t) : string =
    match entry with
    | FNM v | SNM v         -> v
    | FSZ v | FDT v | SDT v -> Conv_utils.uint64_to_string v
    | HSH hash_bytes        -> Multihash.hash_bytes_to_multihash hash_bytes
    | PID v                 -> v
  ;;

  let to_id_and_string (entry:t) : id * string =
    let res_string = single_to_string entry in
    match entry with
    | FNM _ -> (`FNM, res_string)
    | SNM _ -> (`SNM, res_string)
    | FSZ _ -> (`FSZ, res_string)
    | FDT _ -> (`FDT, res_string)
    | SDT _ -> (`SDT, res_string)
    | HSH _ -> (`HSH, res_string)
    | PID _ -> (`PID, res_string)
  ;;

  let id_and_bytes_to_bytes (entry:id * string) : string =
    let (id, data) = entry in
    let id_str     = id_to_string id in
    let len        = Uint8.of_int (String.length data) in
    let len_bytes  = Conv_utils.uint8_to_string len in
    String.concat "" [id_str; len_bytes; data]
  ;;

  let to_string ~(ver:version) ~(fields:t list) : string =
    let max_data_size  = ver_to_data_size ver in
    let id_string_list  = List.map to_id_and_string fields in
    let string_list    = List.map id_and_bytes_to_bytes id_string_list in
    let all_string     = String.concat "" string_list in
    let all_string_len = String.length all_string in
    if all_string_len <= max_data_size then
      Helper.pad_header_or_block_string all_string max_data_size
    else
      raise (Too_much_data (Printf.sprintf "Metadata is too long when converted to bytes\n%s" (length_distribution id_string_list)))
  ;;

  module Parser = struct
    type metadata = t (* to work around the shadowing binding of t in Angstrom *)
    open Angstrom

    let arb_len_data_p : string Angstrom.t =
      any_uint8 >>= (fun x -> take x)
    ;;

    let uint64_data_p : Stdint.uint64 Angstrom.t =
      char '\008' *> Angstrom.BE.int64 >>| Uint64.of_int64
    ;;

    let fnm_p : metadata Angstrom.t =
      string "FNM" *> arb_len_data_p
      >>| (fun x -> FNM x)
    ;;
    let snm_p : metadata Angstrom.t =
      string "SNM" *> arb_len_data_p
      >>| (fun x -> SNM x)
    ;;
    let fsz_p : metadata Angstrom.t =
      string "FSZ" *> uint64_data_p
      >>| (fun x -> FSZ x)
    ;;
    let fdt_p : metadata Angstrom.t =
      string "FDT" *> uint64_data_p
      >>| (fun x -> FDT x)
    ;;
    let sdt_p : metadata Angstrom.t =
      string "SDT" *> uint64_data_p
      >>| (fun x -> SDT x)
    ;;

    let gen_hash_parser (hash_type:Multihash.hash_type) =
      let open Multihash in
      let total_len = Specs.hash_type_to_total_length hash_type in
      let len_bytes = Conv_utils.uint8_to_string (Uint8.of_int total_len) in
      string "HSH" *> string len_bytes *> Parser.gen_parser hash_type
    ;;

    let hsh_p : metadata Angstrom.t =
      choice (List.map (fun hash_type -> gen_hash_parser hash_type) Multihash.all_hash_types)
      >>| (fun x -> HSH x)
    ;;
    (*let pid_p : metadata Angstrom.t =
      string "PID" *> arb_len_data_p
      >>| (fun x -> PID x)
    ;;*)

    let field_p : metadata Angstrom.t =
      choice [ fnm_p
             ; snm_p
             ; fsz_p
             ; fdt_p
             ; sdt_p
             ; hsh_p
               (* ; pid_p *)
             ]
    ;;

    let filler_p =
      (many (char '\x1A')) *> end_of_input
    ;;

    let fields_p : (metadata list) Angstrom.t =
      (many field_p) <* filler_p
    ;;
  end

  let of_string (data:string) : t list =
    match Angstrom.parse_string Parser.fields_p data with
    | Ok fields -> dedup fields
    | Error _   -> raise Invalid_bytes
  ;;
end

module Block : sig
  exception Too_much_data
  exception Invalid_bytes
  exception Invalid_size
  exception Invalid_seq_num

  type t

  type block_type = [ `Meta | `Data | `Any ]

  val make_metadata_block : Header.common_fields   -> fields:Metadata.t list -> t

  val make_data_block     : ?seq_num:uint32        -> Header.common_fields   -> data:string             -> t

  val to_string           : ?alt_seq_num:uint32           -> t                      -> string

  val of_string           : ?raw_header:Header.raw_header -> ?skipped_already:bool  -> string -> t

  val block_to_ver        : t -> version

  val block_to_file_uid   : t -> string

  val block_to_seq_num    : t -> uint32 option

  val block_to_data       : t -> string

  val block_to_meta       : t -> Metadata.t list

  val block_to_block_type : t -> block_type

  val is_meta             : t -> bool

  val is_data             : t -> bool

end = struct

  exception Too_much_data
  exception Invalid_bytes
  exception Invalid_size
  exception Invalid_seq_num

  type t =
      Data of { header : Header.t
              ; data   : string }
    | Meta of { header : Header.t
              ; fields : Metadata.t list
              ; data   : string }

  type block_type = [ `Meta | `Data | `Any ]

  let make_metadata_block (common:Header.common_fields) ~(fields:Metadata.t list) : t =
    (* encode once to make sure the size is okay *)
    let ver              = common.version in
    let encoded_metadata = Metadata.to_string ~ver ~fields in
    Meta { header = Header.make_metadata_header common
         ; fields
         ; data   = encoded_metadata}
  ;;

  let make_data_block ?(seq_num:uint32 option) (common:Header.common_fields) ~(data:string) : t =
    let ver           = common.version in
    let max_data_size = ver_to_data_size ver in
    let len           = String.length data in
    if len <= max_data_size then
      try
        Data { header = Header.make_data_header ?seq_num common
             ; data   = Helper.pad_header_or_block_string data max_data_size }
      with
      | Header.Invalid_seq_num -> raise Invalid_seq_num
    else
      raise Too_much_data
  ;;

  let to_string ?(alt_seq_num:uint32 option) (block:t) : string =
    let (header, data) =
      match block with
      | Data { header; data }    ->
        begin
          match alt_seq_num with
          | None   -> (header, data)
          | Some n ->
            if (Uint32.to_int n) = 0 then
              raise Invalid_seq_num
            else
              (header, data)
        end
      | Meta { header; data; _ } ->
        begin
          match alt_seq_num with
          | None   -> (header, data)
          | Some n ->
            if (Uint32.to_int n) <> 0 then
              raise Invalid_seq_num
            else
              (header, data)
        end in
    let header_string = Header.to_string ~alt_seq_num ~header ~data in
    String.concat "" [header_string; data]
  ;;

  type raw_block =
    { header : Header.raw_header
    ; data   : string
    }

  module Checker = struct
    let check_data_length ({header; data}:raw_block) : unit =
      let data_size         = String.length data in
      let correct_data_size = ver_to_data_size header.version in
      if data_size <> correct_data_size then
        raise Invalid_size
    ;;

    let check_crc_ccitt ({header; data}:raw_block) : unit =
      let crc_ccitt                    = Conv_utils.uint16_to_string header.crc_ccitt in
      let parts_to_check : string list = [ header.file_uid
                                        ; Conv_utils.uint32_to_string header.seq_num
                                        ; data
                                        ] in
      let string_to_check              = String.concat "" parts_to_check in
      let correct_crc_ccitt            = Helper.crc_ccitt_sbx ~ver:header.version ~input:string_to_check in
      if crc_ccitt <> correct_crc_ccitt then
        raise Invalid_bytes
    ;;
  end

  let raw_block_to_block (raw_block:raw_block) : t =
    Checker.check_data_length raw_block;
    Checker.check_crc_ccitt   raw_block;
    let {header = raw_header; data = raw_data} = raw_block in
    let common = Header.make_common_fields ~uid:raw_header.file_uid raw_header.version in
    if Header.raw_header_is_meta raw_header then
      let fields = Metadata.of_string raw_data in
      make_metadata_block common ~fields
    else
      make_data_block     ~seq_num:raw_header.seq_num common ~data:raw_data
  ;;

  let of_string ?(raw_header:Header.raw_header option) ?(skipped_already:bool = false) (raw_data:string) : t =
    try
      let (header, data_offset) =
        match (raw_header, skipped_already) with
        (* skip over header bytes if a header is given and if not skipped already *)
        | (Some h, true)  -> (h, 0)
        | (Some h, false) -> (h, 16)
        | (None,   _)     ->
          let header_string = Misc_utils.get_sub_string raw_data ~pos:0 ~len:16 in
          (Header.of_string header_string, 16) in
      let data      = Misc_utils.get_sub_string_exc_range raw_data ~start_at:data_offset ~end_before:(String.length raw_data) in
      let raw_block = {header; data} in
      raw_block_to_block raw_block
    with
    | Misc_utils.Invalid_range -> raise Invalid_size
  ;;

  let block_to_header (block:t) : Header.t =
    match block with
    | Data {header; _} -> header
    | Meta {header; _} -> header
  ;;

  let block_to_ver      (block:t) : version =
    Header.header_to_ver (block_to_header block)
  ;;

  let block_to_file_uid (block:t) : string =
    Header.header_to_file_uid (block_to_header block)
  ;;

  let block_to_seq_num  (block:t) : uint32 option =
    Header.header_to_seq_num (block_to_header block)
  ;;

  let block_to_data     (block:t) : string =
    match block with
    | Data {data; _} -> data
    | Meta {data; _} -> data
  ;;

  let block_to_meta     (block:t) : Metadata.t list =
    match block with
    | Data _           -> []
    | Meta {fields; _} -> fields
  ;;

  let block_to_block_type (block:t) : block_type =
    match block with
    | Data _ -> `Data
    | Meta _ -> `Meta
  ;;

  let is_meta (block:t) : bool =
    match block with
    | Meta _ -> true
    | Data _ -> false
  ;;

  let is_data (block:t) : bool =
    not (is_meta block)
  ;;
end
