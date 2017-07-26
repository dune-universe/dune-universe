open Stdint
open Crcccitt
open Sbx_specs

module Helper : sig
  val pad_header_or_block_bytes : bytes       -> int         -> bytes

  val crc_ccitt_sbx             : ver:version -> input:bytes -> bytes

end = struct

  let pad_header_or_block_bytes (old_bytes:bytes) (new_len:int) : bytes =
    Misc_utils.pad_bytes ~filler:(Uint8.of_int 0x1a) old_bytes new_len
  ;;

  let crc_ccitt_sbx ~(ver:version) ~(input:bytes) : bytes =
    let res = crc_ccitt_generic_uint16 ~input ~start_val:(ver_to_uint16 ver) in
    Conv_utils.uint16_to_bytes res
  ;;
end

module Header : sig
  exception Invalid_uid_length
  exception Missing_alt_seq_num
  exception Invalid_bytes
  exception Invalid_seq_num

  type common_fields =
    { signature  : bytes
    ; version    : version
    ; file_uid   : bytes
    }

  type t

  type raw_header =
    { version   : version
    ; crc_ccitt : uint16
    ; file_uid  : bytes
    ; seq_num   : uint32
    }

  val common_fields_to_ver : common_fields             -> version

  val make_common_fields   : ?uid:bytes                -> version       -> common_fields

  val make_metadata_header : common_fields             -> t

  val make_data_header     : ?seq_num:uint32           -> common_fields      -> t

  val of_bytes             : bytes                     -> raw_header

  val to_bytes             : alt_seq_num:uint32 option -> header:t      -> data:bytes    -> bytes

  val header_to_ver        : t -> version

  val header_to_file_uid   : t -> bytes

  val header_to_seq_num    : t -> uint32 option

  val raw_header_is_meta   : raw_header -> bool

  val raw_header_is_data   : raw_header -> bool

end = struct

  exception Invalid_uid_length
  exception Missing_alt_seq_num
  exception Invalid_bytes
  exception Invalid_seq_num

  type common_fields =
    { signature  : bytes
    ; version    : version
    ; file_uid   : bytes
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

  let header_to_signature (header:t) : bytes   = (header_to_common header).signature;;

  let header_to_ver       (header:t) : version = (header_to_common header).version;;

  let header_to_file_uid  (header:t) : bytes   = (header_to_common header).file_uid;;

  let header_to_seq_num (header:t) : uint32 option =
    match header with
    | Meta _              -> Some (Uint32.of_int 0)
    | Data { seq_num; _ } -> seq_num
  ;;

  let gen_file_uid () : bytes =
    let len = sbx_file_uid_len in
    Random_utils.gen_bytes ~len
  ;;

  let common_fields_to_ver (common:common_fields) : version =
    common.version
  ;;

  let make_common_fields ?(uid:bytes option) (ver:version) : common_fields =
    let uid : bytes = match uid with
      | Some x ->
        let len = sbx_file_uid_len in
        if Bytes.length x == len then
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

  let to_bytes ~(alt_seq_num:uint32 option) ~(header:t) ~(data:bytes) : bytes =
    let seq_num =
      match (alt_seq_num, (header_to_seq_num header)) with
      | (Some _, Some s) -> Some s    (* prefer existing one over provided one *)
      | (None,   Some s) -> Some s
      | (Some s, None)   -> Some s
      | (None,   None)   -> None   in
    match seq_num with
    | Some seq_num ->
      let seq_num_bytes : bytes      = Conv_utils.uint32_to_bytes seq_num in
      let things_to_crc : bytes list = [ header_to_file_uid header
                                       ; seq_num_bytes
                                       ; data
                                       ] in
      let bytes_to_crc  : bytes      = Bytes.concat "" things_to_crc in
      let crc_result    : bytes      = Helper.crc_ccitt_sbx ~ver:(header_to_ver header) ~input:bytes_to_crc in
      let header_parts  : bytes list = [ header_to_signature header
                                       ; ver_to_bytes (header_to_ver header)
                                       ; crc_result
                                       ; header_to_file_uid header
                                       ; seq_num_bytes
                                       ] in
      Bytes.concat "" header_parts
    | None ->
      raise Missing_alt_seq_num
  ;;

  type raw_header =
    { version   : version
    ; crc_ccitt : uint16
    ; file_uid  : bytes
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

    let uid_p : bytes Angstrom.t =
      take 6
    ;;

    let seq_p : Stdint.uint32 Angstrom.t =
      BE.uint32 >>| Uint32.of_int32
    ;;

    let header_p : raw_header Angstrom.t =
      sig_p *> lift4 make_raw_header Sbx_specs.Parser.ver_p crc_p uid_p seq_p
    ;;
  end

  let of_bytes (data:bytes) : raw_header =
    match Angstrom.parse_only Parser.header_p (`String data) with
    | Ok header -> header
    | Error _   -> raise Invalid_bytes
  ;;

  let raw_header_is_meta (raw_header:raw_header) : bool =
    (Uint32.to_int raw_header.seq_num) = 0
  ;;

  let raw_header_is_data (raw_header:raw_header) : bool =
    not (raw_header_is_meta raw_header)
  ;;
end

module Metadata : sig
  exception Too_much_data of string
  exception Invalid_entry of string
  exception Invalid_bytes

  type t =
      FNM of string
    | SNM of string
    | FSZ of uint64
    | FDT of uint64
    | SDT of uint64
    | HSH of Multihash.hash_bytes  (* should ALWAYS store the RAW hash, to bytes should automatically convert it to multihash *)
    | PID of bytes

  val dedup    : t list      -> t list

  val to_bytes : ver:version -> fields:t list -> bytes

  val of_bytes : bytes       -> t list

end = struct

  exception Too_much_data of string
  exception Invalid_entry of string
  exception Invalid_bytes

  type t =
      FNM of string
    | SNM of string
    | FSZ of uint64
    | FDT of uint64
    | SDT of uint64
    | HSH of Multihash.hash_bytes
    | PID of bytes

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

  let length_distribution (lst:(id * bytes) list) : string =
    let rec length_distribution_helper (lst:(id * bytes) list) (acc:string list) : string =
      match lst with
      | []              -> (String.concat "\n" (List.rev acc))
      | (id, data) :: vs -> let str = Printf.sprintf "id : %s, len : %d" (id_to_string id) (Bytes.length data) in
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

  let single_to_bytes (entry:t) : bytes =
    match entry with
    | FNM v | SNM v         -> Conv_utils.string_to_bytes v
    | FSZ v | FDT v | SDT v -> Conv_utils.uint64_to_bytes v
    | HSH hash_bytes        -> Multihash.hash_bytes_to_multihash hash_bytes
    | PID v                 -> v
  ;;

  let to_id_and_bytes (entry:t) : id * bytes =
    let res_bytes = single_to_bytes entry in
    match entry with
    | FNM _ -> (`FNM, res_bytes)
    | SNM _ -> (`SNM, res_bytes)
    | FSZ _ -> (`FSZ, res_bytes)
    | FDT _ -> (`FDT, res_bytes)
    | SDT _ -> (`SDT, res_bytes)
    | HSH _ -> (`HSH, res_bytes)
    | PID _ -> (`PID, res_bytes)
  ;;

  let id_and_bytes_to_bytes (entry:id * bytes) : bytes =
    let (id, data) = entry in
    let id_str     = id_to_string id in
    let len        = Uint8.of_int (Bytes.length data) in
    let len_bytes  = Conv_utils.uint8_to_bytes len in
    Bytes.concat (Bytes.create 0) [id_str; len_bytes; data]
  ;;

  let to_bytes ~(ver:version) ~(fields:t list) : bytes =
    let max_data_size = ver_to_data_size ver in
    let id_bytes_list = List.map to_id_and_bytes fields in
    let bytes_list    = List.map id_and_bytes_to_bytes id_bytes_list in
    let all_bytes     = Bytes.concat (Bytes.create 0) bytes_list in
    let all_bytes_len = Bytes.length all_bytes in
    if all_bytes_len <= max_data_size then
      Helper.pad_header_or_block_bytes all_bytes max_data_size
    else
      raise (Too_much_data (Printf.sprintf "Metadata is too long when converted to bytes\n%s" (length_distribution id_bytes_list)))
  ;;

  module Parser = struct
    type metadata = t (* to work around the shadowing binding of t in Angstrom *)
    open Angstrom

    let arb_len_data_p : bytes Angstrom.t =
      any_uint8 >>= (fun x -> take x)
    ;;

    let uint64_data_p : Stdint.uint64 Angstrom.t =
      char '\008' *> Angstrom.BE.uint64 >>| Uint64.of_int64
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
      let len_bytes = Conv_utils.uint8_to_bytes (Uint8.of_int total_len) in
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

  let of_bytes (data:bytes) : t list =
    match Angstrom.parse_only Parser.fields_p (`String data) with
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

  val make_metadata_block : Header.common_fields   -> fields:Metadata.t list -> t

  val make_data_block     : ?seq_num:uint32        -> Header.common_fields   -> data:bytes             -> t

  val to_bytes            : ?alt_seq_num:uint32           -> t                      -> bytes

  val of_bytes            : ?raw_header:Header.raw_header -> ?skipped_already:bool  -> bytes -> t

  val block_to_ver        : t -> version

  val block_to_file_uid   : t -> bytes

  val block_to_seq_num    : t -> uint32 option

  val block_to_data       : t -> bytes

  val block_to_meta       : t -> Metadata.t list

  val is_meta             : t -> bool

  val is_data             : t -> bool

end = struct

  exception Too_much_data
  exception Invalid_bytes
  exception Invalid_size
  exception Invalid_seq_num

  type t =
      Data of { header : Header.t
              ; data   : bytes }
    | Meta of { header : Header.t
              ; fields : Metadata.t list
              ; data   : bytes }

  let make_metadata_block (common:Header.common_fields) ~(fields:Metadata.t list) : t =
    (* encode once to make sure the size is okay *)
    let ver              = common.version in
    let encoded_metadata = Metadata.to_bytes ~ver ~fields in
    Meta { header = Header.make_metadata_header common
         ; fields
         ; data   = encoded_metadata}
  ;;

  let make_data_block ?(seq_num:uint32 option) (common:Header.common_fields) ~(data:bytes) : t =
    let ver           = common.version in
    let max_data_size = ver_to_data_size ver in
    let len           = Bytes.length data in
    if len <= max_data_size then
      try
        Data { header = Header.make_data_header ?seq_num common
             ; data   = Helper.pad_header_or_block_bytes data max_data_size }
      with
      | Header.Invalid_seq_num -> raise Invalid_seq_num
    else
      raise Too_much_data
  ;;

  let to_bytes ?(alt_seq_num:uint32 option) (block:t) : bytes =
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
      | Meta { header; data; _ } -> (header, data) in
    let header_bytes = Header.to_bytes ~alt_seq_num ~header ~data in
    Bytes.concat "" [header_bytes; data]
  ;;

  (*module Parser = struct
    open Angstrom

  end*)

  type raw_block =
    { header : Header.raw_header
    ; data   : bytes
    }

  module Checker = struct
    let check_data_length ({header; data}:raw_block) : unit =
      let data_size         = Bytes.length data in
      let correct_data_size = ver_to_data_size header.version in
      if data_size != correct_data_size then
        raise Invalid_size
    ;;

    let check_crc_ccitt ({header; data}:raw_block) : unit =
      let crc_ccitt                   = Conv_utils.uint16_to_bytes header.crc_ccitt in
      let parts_to_check : bytes list = [ header.file_uid
                                        ; Conv_utils.uint32_to_bytes header.seq_num
                                        ; data
                                        ] in
      let bytes_to_check              = Bytes.concat "" parts_to_check in
      let correct_crc_ccitt           = Helper.crc_ccitt_sbx ~ver:header.version ~input:bytes_to_check in
      if (Bytes.compare crc_ccitt correct_crc_ccitt) != 0 then
        raise Invalid_bytes
    ;;
  end

  let raw_block_to_block (raw_block:raw_block) : t =
    Checker.check_data_length raw_block;
    Checker.check_crc_ccitt   raw_block;
    let {header = raw_header; data = raw_data} = raw_block in
    let common = Header.make_common_fields ~uid:raw_header.file_uid raw_header.version in
    if raw_header.seq_num = (Uint32.of_int 0) then
      let fields = Metadata.of_bytes raw_data in
      make_metadata_block common ~fields
    else
      make_data_block     ~seq_num:raw_header.seq_num common ~data:raw_data
  ;;

  let of_bytes ?(raw_header:Header.raw_header option) ?(skipped_already:bool = false) (raw_data:bytes) : t =
    try
      let (header, data_offset) =
        match (raw_header, skipped_already) with
        (* skip over header bytes if a header is given and if not skipped already *)
        | (Some h, true)  -> (h, 0)
        | (Some h, false) -> (h, 16)
        | (None,   _)     ->
          let header_bytes = Misc_utils.get_bytes raw_data ~pos:0 ~len:16 in
          (Header.of_bytes header_bytes, 16) in
      let data         = Misc_utils.get_bytes_exc_range raw_data ~start_at:data_offset ~end_before:(Bytes.length raw_data) in
      let raw_block    = {header; data} in
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

  let block_to_file_uid (block:t) : bytes =
    Header.header_to_file_uid (block_to_header block)
  ;;

  let block_to_seq_num  (block:t) : uint32 option =
    Header.header_to_seq_num (block_to_header block)
  ;;

  let block_to_data     (block:t) : bytes =
    match block with
    | Data {data; _} -> data
    | Meta {data; _} -> data
  ;;

  let block_to_meta     (block:t) : Metadata.t list =
    match block with
    | Data _           -> []
    | Meta {fields; _} -> fields

  let is_meta (block:t) : bool =
    match block with
    | Meta _ -> true
    | Data _ -> false
  ;;

  let is_data (block:t) : bool =
    not (is_meta block)
  ;;
end

(*
let test_metadata_block () : unit =
  let open Metadata in
  let fields : t list = [ FNM (String.make 10000 'a')
                        ; SNM "filename.sbx"
                        ; FSZ (Uint64.of_int 100)
                        ; FDT (Uint64.of_int 100000)
                        ; SDT (Uint64.of_int 100001)
                        ; HSH "1220edeaaff3f1774ad2888673770c6d64097e391bc362d7d6fb34982ddf0efd18cb"
                        ] in
  try
    let common = Header.make_common_fields `V1 in
    let metadata_block = Block.make_metadata_block ~common ~fields in
    let bytes = Block.to_bytes metadata_block in
    Printf.printf "Okay :\n%s\n" (Hex.hexdump_s (Hex.of_string bytes))
  with
  | Metadata.Too_much_data str -> print_endline str
;;

let test_data_block () : unit =
  let data = (Bytes.make 496 '\x00') in
  let common = Header.make_common_fields `V1 in
  let data_block = Block.make_data_block ~common ~data in
  let bytes = Block.to_bytes ~alt_seq_num:(Uint32.of_int 0) data_block in
  Printf.printf "Okay :\n%s\n" (Hex.hexdump_s (Hex.of_string bytes))
;;

test_metadata_block ();
test_data_block ()
   *)
