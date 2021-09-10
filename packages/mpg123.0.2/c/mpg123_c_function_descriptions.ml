open Ctypes

module Types = Mpg123_c_types

module Functions (F : Ctypes.FOREIGN) = struct
  open F

  let mpg123_init = foreign "mpg123_init" (void @-> returning int)
  let mpg123_exit = foreign "mpg123_exit" (void @-> returning void)

  let mpg123_new =
    foreign "mpg123_new" (string_opt @-> ptr int @-> returning (ptr Types.Handle.t))
  let mpg123_delete =
    foreign "mpg123_delete" (ptr Types.Handle.t @-> returning void)

  let mpg123_plain_strerror =
    foreign "mpg123_plain_strerror" (int @-> returning string)
  let mpg123_strerror =
    foreign "mpg123_strerror" (ptr Types.Handle.t @-> returning string)
  let mpg123_errcode =
    foreign "mpg123_errcode" (ptr Types.Handle.t @-> returning int)

  let mpg123_decoders =
    foreign "mpg123_decoders" (void @-> returning (ptr string_opt))
  let mpg123_supported_decoders =
    foreign "mpg123_supported_decoders" (void @-> returning (ptr string_opt))
  let mpg123_decoder =
    foreign "mpg123_decoder" (ptr Types.Handle.t @-> string @-> returning int)
  let mpg123_current_decoder =
    foreign "mpg123_current_decoder" (ptr Types.Handle.t @-> returning string)

  let mpg123_open =
    foreign "mpg123_open" (ptr Types.Handle.t @-> string @-> returning int)
  let mpg123_close =
    foreign "mpg123_close" (ptr Types.Handle.t @-> returning int)
  let mpg123_read =
    foreign "mpg123_read"
      (ptr Types.Handle.t @-> ptr char @-> int @-> ptr int @-> returning int)

  let mpg123_scan =
    foreign "mpg123_scan" (ptr Types.Handle.t @-> returning int)

  let mpg123_meta_check =
    foreign "mpg123_meta_check" (ptr Types.Handle.t @-> returning int)

  let mpg123_length =
    foreign "mpg123_length" (ptr Types.Handle.t @-> returning int)

  let mpg123_meta_free =
    foreign "mpg123_meta_free" (ptr Types.Handle.t @-> returning void)

  let mpg123_id3 =
    foreign "mpg123_id3" (ptr Types.Handle.t @-> (ptr (ptr Types.Id3v1.t))
      @-> (ptr (ptr Types.Id3v2.t)) @-> returning int)

  let mpg123_getformat =
    foreign "mpg123_getformat" (ptr Types.Handle.t @-> ptr int
      @-> ptr int @-> ptr int @-> returning int)
  let mpg123_format_none =
    foreign "mpg123_format_none" (ptr Types.Handle.t @-> returning int)
  let mpg123_format =
    foreign "mpg123_format" (ptr Types.Handle.t @-> int @-> int @-> int @-> returning int)
end
