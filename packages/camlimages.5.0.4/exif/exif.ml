open Exifutil

module Numbers = struct
  type rational = int64 * int64
  type srational = int32 * int32
  let float_of_rational (x,y) = Int64.to_float x /. Int64.to_float y
  let float_of_srational (x,y) = Int32.to_float x /. Int32.to_float y
  let string_of_rational i1 i2 = Printf.sprintf "%Ld/%Ld" i1 i2
  let string_of_srational i1 i2 = Printf.sprintf "%ld/%ld" i1 i2
end

open Numbers

module Endian = struct
  type t = Big (* Motorola *) | Little (* Intel *)

  let to_string = function
    | Big -> "Big"
    | Little -> "Little"

  let sys = if Sys.big_endian then Big else Little
end

module IFD = struct
  type t = 
    | IFD_0   (** Info of the main image *)
    | IFD_1   (** Info of the thumbnail *)
    | EXIF    (** camera info *)
    | GPS     (** location *)
    | Interop (** exif format interoperability info *)
end
 
module Date = struct
  (* for GPSDateStamp *)
  type t = {
    year  : int;
    month : int; (** 1-12, I guess *)
    day   : int;
  }

  let to_string t = Printf.sprintf "%04d:%02d:%02d"
    t.year t.month t.day

  let of_string s =
    try
      if String.length s <> 10 then raise Exit;
      let check_colon n = if s.[n] <> ':' then raise Exit in
      let get_int from len =
        (* "0x12" is parsable, but who cares? *)
        int_of_string (String.sub s from len )
      in
      check_colon 4;
      check_colon 7;
      let year  = get_int 0 4 in
      let month = get_int 5 2 in
      let day   = get_int 8 2 in
      `Ok { year; month; day; }
    with _ -> `Error s

end

module DateTime = struct
  type t = {
    year  : int;
    month : int; (** 1-12, I guess *)
    day   : int;
    hour  : int;
    min   : int;
    sec   : int
  }

  let to_string t = Printf.sprintf "%04d:%02d:%02d %02d:%02d:%02d"
    t.year t.month t.day t.hour t.min t.sec

  let of_string s =
    try
      if String.length s <> 19 then raise Exit;
      let check_colon n = if s.[n] <> ':' then raise Exit in
      let get_int from len =
        (* "0x12" is parsable, but who cares? *)
        int_of_string (String.sub s from len )
      in
      check_colon 4;
      check_colon 7;
      check_colon 13;
      check_colon 16;
      let year  = get_int 0 4 in
      let month = get_int 5 2 in
      let day   = get_int 8 2 in
      let hour  = get_int 11 2 in
      let min   = get_int 14 2 in
      let sec   = get_int 17 2 in
      `Ok { year; month; day; hour; min; sec }
    with _ -> `Error s

  (* I had an Android phone which created DateTime tag with
     a little endian encoded unsigned int32 of unix time!
     This function tries to fix the issue.
  *)
  let of_string_packed_unix_time s =
    try
      let float_code c = float (Char.code c) in
      let converter sec =
        let open Unix in
        let tm = Unix.gmtime sec in
        { year  = tm.tm_year;
          month = tm.tm_mon + 1;
          day   = tm.tm_mday;
          hour  = tm.tm_hour;
          min   = tm.tm_min;
          sec   = tm.tm_sec }
        in
      if s.[4] = '\000' then
        let sec = 
          float_code s.[0]
          +. float_code s.[1] *. 256.0
          +. float_code s.[2] *. 65536.0
          +. float_code s.[3] *. 16777216.0
        in
        (* \000\000\000\000\000 is treated as an error 
           rather than 1970-01-01T00:00:00
        *)
        if sec = 0.0 then `Error s else `Ok (converter sec)
      else `Error s
    with
    | _ -> `Error s
end

module Tag = struct

  type t = int

  external to_string : t -> IFD.t -> string = "caml_exif_tag_get_name_in_ifd"
end

module Entry = struct

  type t 

  module Pack = struct
    type format = 
      | ILLEGAL
      | BYTE       (*=  1, *)
      | ASCII      (*=  2, *)
      | SHORT      (*=  3, *)
      | LONG       (*=  4, *)
      | RATIONAL   (*=  5, *)
      | SBYTE      (*=  6, *)
      | UNDEFINED  (*=  7, *)
      | SSHORT     (*=  8, *)
      | SLONG      (*=  9, *)
      | SRATIONAL  (*= 10, *)
      | FLOAT      (*= 11, *)
      | DOUBLE     (*= 12  *) 
    
    let string_of_format = function
      | ILLEGAL -> assert false
      | BYTE       -> "BYTE"       
      | ASCII      -> "ASCII"      
      | SHORT      -> "SHORT"      
      | LONG       -> "LONG"       
      | RATIONAL   -> "RATIONAL"   
      | SBYTE      -> "SBYTE"      
      | UNDEFINED  -> "UNDEFINED"  
      | SSHORT     -> "SSHORT"     
      | SLONG      -> "SLONG"      
      | SRATIONAL  -> "SRATIONAL"  
      | FLOAT      -> "FLOAT"      
      | DOUBLE     -> "DOUBLE"     
  
    type unpacked = 
      | Bytes      of int array
      | Asciis     of string
      | Shorts     of int array 
      | Longs      of int64 array
      | Rationals  of (int64 * int64) array
      | SBytes     of int array
      | Undefined  of string
      | SShorts    of int array
      | SLongs     of int32 array
      | SRationals of (int32 * int32) array
      | Floats     of float array
      | Doubles    of float array
  
    external decode_bytes      : string -> int -> int array       = "Val_ExifBytes"
    external decode_shorts     : string -> int -> int array       = "Val_ExifShorts"
    external decode_longs      : string -> int -> int64 array     = "Val_ExifLongs"
    external decode_rationals  : string -> int -> rational array  = "Val_ExifRationals"
    external decode_sbytes     : string -> int -> int array       = "Val_ExifSBytes"
    external decode_sshorts    : string -> int -> int array       = "Val_ExifSShorts"
    external decode_slongs     : string -> int -> int32 array     = "Val_ExifSLongs"
    external decode_srationals : string -> int -> srational array = "Val_ExifSRationals"
    external decode_floats     : string -> int -> float array     = "Val_ExifFloats"
    external decode_doubles    : string -> int -> float array     = "Val_ExifDoubles"
    
    let unpack format components content =
      match format with
      | ILLEGAL -> assert false
      | BYTE       (*=  1, *) -> 
          Bytes (decode_bytes content components)
      | ASCII      (*=  2, *) -> 
          (* remove the last \000 *)
          let content = 
            let len = String.length content in
            if content.[len-1] = '\000' then
              String.sub content 0 (len-1)
            else content
          in
          Asciis content
      | SHORT      (*=  3, *) -> 
          Shorts (decode_shorts content components)
      | LONG       (*=  4, *) ->
          Longs (decode_longs content components)
      | RATIONAL   (*=  5, *) ->
          Rationals (decode_rationals content components)
      | SBYTE      (*=  6, *) -> 
          Bytes (decode_sbytes content components)
      | UNDEFINED  (*=  7, *) -> Undefined content
      | SSHORT     (*=  8, *) ->
          SShorts (decode_sshorts content components)
      | SLONG      (*=  9, *) ->
          SLongs (decode_slongs content components)
      | SRATIONAL  (*= 10, *) ->
          SRationals (decode_srationals content components)
      | FLOAT      (*= 11, *) ->
          Floats (decode_floats content components)
      | DOUBLE     (*= 12  *) ->
          Doubles (decode_doubles content components)
  
    open Format
  
    let format ppf v = 
      begin match v with
      | Asciis     _ -> () 
      | Undefined  _ -> fprintf ppf "Undefined "
      | Bytes      _ -> fprintf ppf "Bytes "
      | SBytes     _ -> fprintf ppf "SBytes "
      | Shorts     _ -> fprintf ppf "Shorts "
      | Longs      _ -> fprintf ppf "Longs "
      | Rationals  _ -> fprintf ppf "Rationals "
      | SShorts    _ -> fprintf ppf "SShorts "
      | SLongs     _ -> fprintf ppf "SLongs "
      | SRationals _ -> fprintf ppf "SRationals "
      | Floats     _ -> fprintf ppf "Floats "
      | Doubles    _ -> ()
      end;
      match v with
      | Asciis     s 
      | Undefined  s -> fprintf ppf "%S" s
      | Bytes      is
      | SBytes     is
      | Shorts     is -> Format.array (fun ppf -> fprintf ppf "%d") ppf is
      | Longs      is -> Format.array (fun ppf -> fprintf ppf "%Ld") ppf is
      | Rationals  rs -> Format.array (fun ppf (i1,i2) -> fprintf ppf "%Ld/%Ld" i1 i2) ppf rs
      | SShorts    is -> Format.array (fun ppf -> fprintf ppf "%d") ppf is
      | SLongs     is -> Format.array (fun ppf -> fprintf ppf "%ld") ppf is
      | SRationals  rs -> Format.array (fun ppf (i1,i2) -> fprintf ppf "%ld/%ld" i1 i2) ppf rs
      | Floats     fs
      | Doubles    fs -> Format.array (fun ppf -> fprintf ppf "%.20g") ppf fs
  
  end

  external unref : t -> unit = "caml_exif_entry_unref"

  module Decoded = struct
    type t = {
      tag : int;
      format : Pack.format;
      components : int; (* hope it will not overflow *)
      data : string;
    }

  end

  external decode : t -> Decoded.t = "caml_exif_decode_entry"

  type unpacked_entry = Tag.t * Pack.unpacked

  let unpack : Decoded.t -> unpacked_entry = fun d ->
    d.Decoded.tag,
    Pack.unpack d.Decoded.format d.Decoded.components d.Decoded.data

  let format_unpacked_entry ifd ppf (tag, p) =
    Format.fprintf ppf "%s(%x): %a"
      (Tag.to_string tag ifd) tag
      Pack.format p

  let format ifd ppf t =
    format_unpacked_entry ifd ppf (unpack (decode t))

end

module Content = struct
  type t

  external unref : t -> unit = "caml_exif_content_unref"

  external entries : t -> Entry.t list = "caml_exif_content_entries"

  let entries t = 
    let es = entries t in
    let finalise v =
      Gc.finalise (fun v -> 
        Entry.unref v) v
    in
    List.iter finalise es;
    es

  let format ifd ppf t =
    let ents = entries t in
    Format.fprintf ppf "@[[ @[%a@] ]@]"
      (Format.list ";@ " (Entry.format ifd)) ents
    
end

module Data = struct
  type t

  external from_string : string -> t = "caml_val_exif_data"
  external unref : t -> unit = "caml_exif_data_unref"

  external get_byte_order : t -> Endian.t = "caml_exif_get_byte_order"
  external set_byte_order : t -> Endian.t -> unit = "caml_exif_set_byte_order"
  external fix : t -> unit = "caml_exif_data_fix"
  external dump : t -> unit = "caml_exif_data_dump"

  let from_string data = 
    let t = from_string data in
    set_byte_order t Endian.sys ; (* Destructively fix the endianess *)
    Gc.finalise (fun v -> 
      unref v) t;
    t
      
  type contents = {
    ifd_0   : Content.t option;
    ifd_1   : Content.t option;
    exif    : Content.t option;
    gps     : Content.t option;
    interop : Content.t option
  }

  external contents : t -> contents = "caml_exif_data_contents"

  let contents t = 
    let cs = contents t in
    let finalise = function
      | None -> ()
      | Some v -> 
          Gc.finalise (fun v -> 
            Content.unref v) v
    in
    finalise cs.ifd_0;
    finalise cs.ifd_1;
    finalise cs.exif;
    finalise cs.gps;
    finalise cs.interop;
    cs

  let get_ifd_0    t = (contents t).ifd_0 
  let get_ifd_1    t = (contents t).ifd_1
  let get_exif     t = (contents t).exif
  let get_gps      t = (contents t).gps
  let get_interop  t = (contents t).interop

  let unpack_gen f t = match f t with
    | None -> None
    | Some content ->
        Some (List.map (fun x -> Entry.unpack (Entry.decode x))
                (Content.entries content))

  let unpack_ifd_0   = unpack_gen get_ifd_0
  let unpack_ifd_1   = unpack_gen get_ifd_1
  let unpack_exif    = unpack_gen get_exif
  let unpack_gps     = unpack_gen get_gps
  let unpack_interop = unpack_gen get_interop

  open Format

  let format ppf t =
    let conts = contents t in
    fprintf ppf "{ @[ifd_0=%a;@ ifd_1=%a;@ exif=%a;@ gps=%a;@ inter=%a@] }"
      (Format.opt (Content.format IFD.IFD_0)  ) conts.ifd_0
      (Format.opt (Content.format IFD.IFD_1)  ) conts.ifd_1
      (Format.opt (Content.format IFD.EXIF)   ) conts.exif
      (Format.opt (Content.format IFD.GPS)    ) conts.gps
      (Format.opt (Content.format IFD.Interop)) conts.interop
    
end


module Analyze = struct
  (* Exif data analyzer 
  
     Due to its updated-on-demand and lots-of-tags nature,
     This module is implemented in a separate file from exif.ml
     and its interface file is auto created.
  *)
  
  open Numbers
  open Entry.Pack
  
  type datetime = 
    [ `EncodedInUnixTime of DateTime.t
    | `Error of string
    | `Ok of DateTime.t 
    ]
  (** I have some photos from my old Android with non Ascii datetime.
      They have encoded 32 bit int in Unix time instead! :-(
  *)

  let parse_datetime s =
    match DateTime.of_string s with
    | (`Ok _ as r) -> r
    | `Error s -> 
        match DateTime.of_string_packed_unix_time s with
        | `Ok v -> `EncodedInUnixTime v
        | (`Error _ as e) -> e 

  let analyze_ifd (tag, pack) = match tag, pack with
      
    | 0x10f, Asciis s -> `Make s
    | 0x110, Asciis s -> `Model s
    | 0x112, Shorts [| 1 |] -> `Orientation `TopLeft
    | 0x112, Shorts [| 2 |] -> `Orientation `TopRight
    | 0x112, Shorts [| 3 |] -> `Orientation `BottomRight
    | 0x112, Shorts [| 4 |] -> `Orientation `BottomLeft
    | 0x112, Shorts [| 5 |] -> `Orientation `LeftTop
    | 0x112, Shorts [| 6 |] -> `Orientation `RightTop
    | 0x112, Shorts [| 7 |] -> `Orientation `RightBottom
    | 0x112, Shorts [| 8 |] -> `Orientation `LeftBottom
    | 0x11a, Rationals [| r |] -> `XResolution r
    | 0x11b, Rationals [| r |] -> `YResolution r
    | 0x128, Shorts [| 2 |] -> `ResolutionUnit `Inches
    | 0x128, Shorts [| 3 |] -> `ResolutionUnit `Centimeters
    | 0x131, s -> `Software s
    | 0x132, Asciis s -> `DateTime (parse_datetime s)
    | _ -> `Unknown (tag, pack)
  
  let analyze_exif (tag, pack) = match tag, pack with
  
    | 0x9000, Undefined s -> `ExifVersion s
    | 0x927c, Undefined s -> `MakerNote s
    | 0x9286, Undefined s -> `UserComment s
        (* The first 8 bytes indicate char code:
           ASCII 41.H, 53.H, 43.H, 49.H, 49.H, 00.H, 00.H, 00.H 
           JIS   4A.H, 49.H, 53.H, 00.H, 00.H, 00.H, 00.H, 00.H JIS X0208-1990
           Unicode 55.H, 4E.H, 49.H, 43.H, 4F.H, 44.H, 45.H, 00.H Unicode Standard
           Undefined 00.H, 00.H, 00.H, 00.H, 00.H, 00.H, 00.H, 00.H          
        *)
    | 0x9003, Asciis s -> `DateTimeOriginal (parse_datetime s)
    | 0x9004, Asciis s -> `DateTimeDigitized (parse_datetime s)
    | 0x9290, Asciis s -> `SubsecTime s
    | 0x9291, Asciis s -> `SubsecTimeOriginal s
    | 0x9292, Asciis s -> `SubsecTimeDigitized s
  
    | _ -> `Unknown (tag, pack)
  
(* CR jfuruse: unused
  module GPS = struct
    type latitude = [ `North | `South ] * rational
    type longitude = [ `East | `West ] * rational
    type altitude = [ `AboveSeaLevel | `BelowSeaLevel ] * rational
    type time_stamp_utc = { 
      hour : rational;
      min  : rational;
      sec  : rational; 
    }
    type direction = [ `True | `Magnetic ] * rational
    type map_datum = string
  
    type t = {
      version        : (int * int * int * int) option;
      latitude       : latitude option;
      longitude      : longitude option;
      altitude       : altitude option;
      time_stamp_utc : time_stamp_utc option;
      direction      : direction option;
      map_datum      : map_datum option
    }
  end
*)
        
  let analyze_gps (tag, v) = match tag, v with
      
    | 00, Bytes [|x;y;z;w|] -> `GPSVersion (x,y,z,w)
    | 01, Asciis "N" -> `NorthLatitude
    | 01, Asciis "S" -> `SouthLatitude
    | 02, Rationals [|r|] -> `Latitude r
    | 03, Asciis "E" -> `EastLongitude
    | 03, Asciis "W" -> `WestLongitude
    | 04, Rationals [|r|] -> `Longitude r
    | 05, Bytes [|0|] -> `AboveSeaLevel
    | 05, Bytes [|1|] -> `BelowSeaLevel
    | 06, Rationals [|r|] -> `Altitude r
    | 07, Rationals [|h;m;s|] -> 
        `TimeStampUTC (float_of_rational h,
                       float_of_rational m,
                       float_of_rational s)
    | 07, SRationals [|h;m;s|] -> 
        (* It is illegal in the spec but I see some photos with SRationals *)
        `TimeStampUTCinSRationals (float_of_srational h,
                                   float_of_srational m,
                                   float_of_srational s) 
  
    | 16, Asciis "T" -> `ImgDirectionTrue
    | 16, Asciis "M" -> `ImgDirectionMagnetic
    | 17, Rationals [|r|] -> `ImgDirection r
    | 18, Asciis s -> `GPSMapDatum s
    | 29, Asciis s -> `GPSDate (Date.of_string s)
  
    | _ -> `Unknown (tag, v)
  
  
  let exif_datetime t = match Data.unpack_exif t with
    | Some entries -> 
        List.find_map_opt (function
          | `DateTimeOriginal t -> Some t
          | _ -> None)
          (List.map analyze_exif entries)
    | None -> None
  
  let ifd_0_datetime t = match Data.unpack_ifd_0 t with
    | Some entries -> 
        List.find_map_opt (function
          | `DateTime t -> Some t
          | _ -> None)
          (List.map analyze_ifd entries)
    | None -> None
  
  let datetime t = match exif_datetime t with
    | (Some _ as res) -> res
    | None -> ifd_0_datetime t
end
