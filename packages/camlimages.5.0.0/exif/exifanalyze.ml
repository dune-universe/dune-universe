(** Exif data analyzer 

    Due to its updated-on-demand and lots-of-tags nature,
    This module is implemented in a separate file from exif.ml
    and its interface file is auto created.
*)

open Exifutil

open Exif
open Exif.Numbers
open Exif.Entry.Pack

(* I have some photos from my old Android with non Ascii datetime.
   They have encoded 32 bit int in Unix time instead! :-(
*)
let analyze_datetime s =
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
  | 0x132, Asciis s -> `DateTime (analyze_datetime s)
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
  | 0x9003, Asciis s -> `DateTimeOriginal (analyze_datetime s)
  | 0x9004, Asciis s -> `DateTimeDigitized (analyze_datetime s)
  | 0x9290, Asciis s -> `SubsecTime s
  | 0x9291, Asciis s -> `SubsecTimeOriginal s
  | 0x9292, Asciis s -> `SubsecTimeDigitized s

  | _ -> `Unknown (tag, pack)

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
