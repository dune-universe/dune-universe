(** EXIF which is often found in photo images.

  See https://en.wikipedia.org/wiki/Exchangeable_image_file_format

  An example at test/jpgexif.ml
*)

module Numbers : sig
  type rational  = int64 * int64 (** unsigned 32bits int rational *)

  type srational = int32 * int32 (** signed 32bits int rational *)

  val float_of_rational   : int64 * int64 -> float
  val float_of_srational  : int32 * int32 -> float
  val string_of_rational  : int64 -> int64 -> string
  val string_of_srational : int32 -> int32 -> string
end

module Endian : sig
  type t = Big | Little 
  val to_string : t -> string 

  val sys : t
    (** Endian of the system.  Based on [Sys.big_endian] *)
end

module IFD : sig
  type t = 
    | IFD_0   (** Info of the main image *)
    | IFD_1   (** Info of the thumbnail *)
    | EXIF    (** camera info *)
    | GPS     (** location *)
    | Interop (** exif format interoperability info *)
end

module Date : sig
  type t = { year : int; month : int; day : int; }
  (** Date for GPSDateStamp *)

  val to_string : t -> string
  val of_string : string -> [> `Error of string | `Ok of t ]
end

module DateTime : sig
  type t = {
    year : int;
    month : int;
    day : int;
    hour : int;
    min : int;
    sec : int;
  }

  val to_string : t -> string
  val of_string : string -> [> `Error of string | `Ok of t ]

  val of_string_packed_unix_time : string -> [> `Error of string | `Ok of t ]
  (** I had an old Android phone which created DateTime tag with
      a little endian encoded unsigned int32 of unix time, which
      seems not following the spec of EXIF.

      This function tries to fix the issue.
  *)
  
end
    
module Tag : sig
  type t = int

  val to_string : t -> IFD.t -> string
  (** Tag name requires IFD.t since the same tag number has different
      meaning in IFD and GPS *)
end

module Entry : sig
  type t
  
  module Pack : sig

    type format =
      | ILLEGAL (** do not used it *)
      | BYTE
      | ASCII
      | SHORT
      | LONG
      | RATIONAL
      | SBYTE
      | UNDEFINED
      | SSHORT
      | SLONG
      | SRATIONAL
      | FLOAT
      | DOUBLE

    val string_of_format : format -> string

    type unpacked =
      | Bytes of int array
      | Asciis of string
      | Shorts of int array
      | Longs of int64 array
      | Rationals of (int64 * int64) array
      | SBytes of int array
      | Undefined of string
      | SShorts of int array
      | SLongs of int32 array
      | SRationals of (int32 * int32) array
      | Floats of float array
      | Doubles of float array
    (** Constructors start with "S" are signed. *)

    val unpack : format -> int -> string -> unpacked
    (** [unpack format components packed] 
        [components] are the number of elements in [packed],
        not the bytes of [packed].
    *)

    val format : Format.formatter -> unpacked -> unit

  end
  
  module Decoded : sig
    type t = {
      tag : int;
      format : Pack.format;
      components : int;
      data : string;
    }
  end
  
  val decode : t -> Decoded.t

  type unpacked_entry = Tag.t * Pack.unpacked
  val unpack : Decoded.t -> unpacked_entry

  val format_unpacked_entry :
    IFD.t -> Exifutil.Format.formatter -> unpacked_entry -> unit
  
  val format : IFD.t -> Exifutil.Format.formatter -> t -> unit
    (** [format] does decode + unpack *)

end

module Content : sig
  type t

  val entries : t -> Entry.t list
  val format : IFD.t -> Exifutil.Format.formatter -> t -> unit
end

module Data : sig
  type t
  (** Raw EXIF data in C *)
    
  val get_byte_order : t -> Endian.t
  val set_byte_order : t -> Endian.t -> unit
  val fix : t -> unit
  val dump : t -> unit

  val from_string : string -> t
  (** Parse the raw string of EXIF data which starts with "Exif\000\000". *)

  val format : Format.formatter -> t -> unit
  
  type contents = {
    ifd_0   : Content.t option;
    ifd_1   : Content.t option;
    exif    : Content.t option;
    gps     : Content.t option;
    interop : Content.t option;
  }
  (** Partially parsed EXIF data *)
  
  val contents : t -> contents
  (** Partially parse the raw EXIF to bunch of Contents.t *)

  val get_ifd_0   : t -> Content.t option
  val get_ifd_1   : t -> Content.t option
  val get_exif    : t -> Content.t option
  val get_gps     : t -> Content.t option
  val get_interop : t -> Content.t option
  (** Get Contents.t of the specific field *)

  val unpack_ifd_0 : t -> Entry.unpacked_entry list option
  val unpack_ifd_1 : t -> Entry.unpacked_entry list option
  val unpack_exif  : t -> Entry.unpacked_entry list option
  val unpack_gps   : t -> Entry.unpacked_entry list option
  val unpack_interop : t -> Entry.unpacked_entry list option
  (** Get and parse the specific field *)

end

module Analyze : sig

  type datetime = 
    [ `EncodedInUnixTime of DateTime.t
         (** Photos from some old Androids have non Ascii datetime.
             They have encoded 32 bit int in Unix time instead! :-(
         *)
    | `Error of string
    | `Ok of DateTime.t 
    ]
        
  val parse_datetime : string -> [> datetime ]

  val analyze_ifd :
    Entry.unpacked_entry 
    -> [> `DateTime of [> datetime ]
       | `Make of string
       | `Model of string
       | `Orientation of [> `BottomLeft
                         | `BottomRight
                         | `LeftBottom
                         | `LeftTop
                         | `RightBottom
                         | `RightTop
                         | `TopLeft
                         | `TopRight ]
       | `ResolutionUnit of [> `Centimeters | `Inches ]
       | `Software of Entry.Pack.unpacked
       | `Unknown of Entry.unpacked_entry
       | `XResolution of int64 * int64
    | `YResolution of int64 * int64 ]
  (** Analyze IFD.  The unpacked_entry must come from ifd_0 or ifd_1 sub-IFDs *)
    
  val analyze_exif :
    Entry.unpacked_entry 
    -> [> `DateTimeDigitized of [> datetime ]
       | `DateTimeOriginal of [> datetime ]
       | `ExifVersion of string
       | `MakerNote of string
       | `SubsecTime of string
       | `SubsecTimeDigitized of string
       | `SubsecTimeOriginal of string
       | `Unknown of Entry.unpacked_entry
       | `UserComment of string ]
  (** Analyze EXIF.  The unpacked_entry must come from exif sub-IFD *)

  val analyze_gps :
    Entry.unpacked_entry 
    -> [> `AboveSeaLevel
       | `Altitude of int64 * int64
       | `BelowSeaLevel
       | `EastLongitude
       | `GPSDate of [> `Error of string | `Ok of Date.t ]
       | `GPSMapDatum of string
       | `GPSVersion of int * int * int * int
       | `ImgDirection of int64 * int64
       | `ImgDirectionMagnetic
       | `ImgDirectionTrue
       | `Latitude of int64 * int64
       | `Longitude of int64 * int64
       | `NorthLatitude
       | `SouthLatitude
       | `TimeStampUTC of float * float * float
       | `TimeStampUTCinSRationals of float * float * float
       | `Unknown of Entry.unpacked_entry
       | `WestLongitude 
       ]
  (** Analyze GPS.  The unpacked_entry must come from gps sub-IFD *)

  val ifd_0_datetime : Data.t -> [> datetime ] option
  (** Get ifd_0 DateTime *)

  val exif_datetime : Data.t -> [> datetime ] option
  (** Get exif DateTimeOriginal *)

  val datetime : Data.t -> [> datetime ] option
  (** Get one of the first finding of the followings:
      * exif DateTimeOriginal
      * ifd_0 DateTime
   *)
end
