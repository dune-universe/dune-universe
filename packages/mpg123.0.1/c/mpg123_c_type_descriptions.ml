module Types (F : Ctypes.TYPE) = struct
  open Ctypes
  open F
  let mpg123_api_version = constant "MPG123_API_VERSION" int
  let mpg123_ok = constant "MPG123_OK" int
  let mpg123_done = constant "MPG123_DONE" int

  let mpg123_id3 = constant "MPG123_ID3" int
  let mpg123_new_id3 = constant "MPG123_NEW_ID3" int
  let mpg123_icy = constant "MPG123_ICY" int
  let mpg123_new_icy = constant "MPG123_NEW_ICY" int

  (* these come from fmt123.h, included by mpg123.h *)
  let mpg123_enc_signed16 = constant "MPG123_ENC_SIGNED_16" int
  let mpg123_enc_float32 = constant "MPG123_ENC_FLOAT_32" int

  module Handle = struct
    type t = [`Handle] structure
    let t : t typ = typedef (structure "`Handle") "mpg123_handle"
  end

  module Mpg123_string = struct
    type t = [`Mpg123_string] structure
    let t : t typ = typedef (structure "`Mpg123_string") "mpg123_string"
    let p    = field t "p" (ptr char)
    let size = field t "size" int
    let fill = field t "fill" int
    let () = seal t
  end

  module Id3v1 = struct
    type t = [`Id3v1] structure
    let t : t typ = typedef (structure "`Id3v1") "mpg123_id3v1"
    let tag     = field t "tag" (array 3 char)
    let title   = field t "title" (array 30 char)
    let artist  = field t "artist" (array 30 char)
    let album   = field t "album" (array 30 char)
    let year    = field t "year" (array 4 char)
    let comment = field t "comment" (array 30 char)
    let genre   = field t "genre" char
    let () = seal t
  end

  module Id3v2 = struct
    type t = [`Id3v2] structure
    let t : t typ = typedef (structure "`Id3v2") "mpg123_id3v2"
    let version = field t "version" char
    let title   = field t "title" (ptr Mpg123_string.t)
    let artist  = field t "artist" (ptr Mpg123_string.t)
    let album   = field t "album" (ptr Mpg123_string.t)
    let year    = field t "year" (ptr Mpg123_string.t)
    let genre   = field t "genre" (ptr Mpg123_string.t)
    let comment = field t "comment" (ptr Mpg123_string.t)
    let () = seal t
  end
end
