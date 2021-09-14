module Constants = struct
  open C.Types

  let api_version = mpg123_api_version
  let ok = mpg123_ok
  let done_ = mpg123_done
  let flag_id3 = mpg123_id3
  let flag_new_id3 = mpg123_new_id3
  let flag_icy = mpg123_icy
  let flag_new_icy = mpg123_new_icy
  let enc_signed16 = mpg123_enc_signed16
  let enc_float32 = mpg123_enc_float32
end

type error_code = int
type flags = int
type enc = int

include Constants

type id3_v1 =
  { tag : string;
    title : string;
    artist : string;
    album : string;
    year : string;
    comment : string;
    genre : char
  }

type id3_v2_text =
  { lang : string;
    id : string;
    description : string;
    text : string
  }

type id3_v2_picture =
  { type_ : char;
    description : string;
    mime_type : string;
    size : int;
    data : string
  }

type id3_v2 =
  { version : char;
    title : string;
    artist : string;
    album : string;
    year : string;
    genre : string;
    comment : string;
    comment_list : id3_v2_text list;
    text : id3_v2_text list;
    extra : id3_v2_text list;
    picture : id3_v2_picture list
  }

type output_format =
  { rate : int;
    channels : int;
    encoding : int
  }

(*
let memcpy ~dest ~src n =
  let cast p = from_voidp (array n uchar) p in
  cast dest <-@ !@(cast src)
*)

(*
let char_array_as_string a =
	let len = Ctypes.CArray.length a in
	let b = Buffer.create len in
	try
		for i = 0 to len -1 do
			let c = Ctypes.CArray.get a i in
			if c = '\x00'
			then raise Exit
			else Buffer.add_char b c
		done;
		Buffer.contents b
	with Exit ->
		Buffer.contents b
*)
let char_array_as_string a = Ctypes.(string_from_ptr (CArray.start a) ~length:(CArray.length a))

module Functions = struct
  open Ctypes
  open C.Functions

  type handle = C.Types.Handle.t ptr

  let ok_unit_or_err err = if err = ok then Ok () else Error (err : error_code)
  let init () = ok_unit_or_err (mpg123_init ())
  let exit = mpg123_exit

  let new_ ?decoder () =
    let errp = allocate int 0 in
    let h = mpg123_new decoder errp in
    let err = !@errp in
    if err = ok then Ok (h : handle) else Error (err : error_code)

  let delete mh = mpg123_delete mh
  let plain_strerror = mpg123_plain_strerror
  let strerror = mpg123_strerror
  let errcode = mpg123_errcode

  let rec copy_char_pp acc cpp =
    match !@cpp with
    | Some s -> copy_char_pp (s :: acc) (cpp +@ 1)
    | None -> acc

  let decoders () =
    let cpp = mpg123_decoders () in
    copy_char_pp [] cpp

  let supported_decoders () =
    let cpp = mpg123_supported_decoders () in
    copy_char_pp [] cpp

  let decoder mh ~decoder_name = ok_unit_or_err (mpg123_decoder mh decoder_name)
  let current_decoder = mpg123_current_decoder
  let open_ mh ~path = ok_unit_or_err (mpg123_open mh path)
  let close mh = ok_unit_or_err (mpg123_close mh)

  type buf = char CArray.t

  let create_buf len = CArray.make char ~initial:'\x00' len

  let copy_buf_to_bytes buf bytes =
    (* We could eliminate this copy by switching to ocaml_bytes_start
       and using the ocaml_bytes type in foreign, but it force us to
       hold the runtime lock during the read, which would be worse
       for app latency. *)
    assert (CArray.length buf = Bytes.length bytes);
    for i = 0 to pred (CArray.length buf) do
      Bytes.unsafe_set bytes i (CArray.unsafe_get buf i)
    done

  let read mh ~buf ~len =
    let bytes_read = allocate int 0 in
    let retval = mpg123_read mh (CArray.start buf) len bytes_read in
    if retval = ok
    then Ok !@bytes_read
    else if retval = done_
    then if !@bytes_read = 0 then Error (retval : error_code) else Ok !@bytes_read
    else Error (retval : error_code)

  let scan mh = ok_unit_or_err (mpg123_scan mh)

  let length mh =
    let result = mpg123_length mh in
    if result >= 0 then Ok result else Error (result : error_code)

  let meta_check mh : flags = mpg123_meta_check mh
  let meta_free = mpg123_meta_free

  let id3 mh =
    let null_v1 = from_voidp C.Types.Id3v1.t null in
    let id3v1 = allocate (ptr C.Types.Id3v1.t) null_v1 in
    let null_v2 = from_voidp C.Types.Id3v2.t null in
    let id3v2 = allocate (ptr C.Types.Id3v2.t) null_v2 in
    let err = mpg123_id3 mh id3v1 id3v2 in
    if ok <> err
    then Error err
    else (
      let v1 =
        if is_null id3v1
        then None
        else (
          let v1 = !@id3v1 in
          if is_null v1
          then None
          else (
            let v1 = !@(v1 +@ 0) in
            let module ID = C.Types.Id3v1 in
            let cass = char_array_as_string in
            Some
              { tag = cass @@ getf v1 ID.tag;
                title = cass @@ getf v1 ID.title;
                artist = cass @@ getf v1 ID.artist;
                album = cass @@ getf v1 ID.album;
                year = cass @@ getf v1 ID.year;
                comment = cass @@ getf v1 ID.comment;
                genre = getf v1 ID.genre
              }))
      in
      let v2 =
        if is_null id3v2
        then None
        else (
          let v2 = !@id3v2 in
          if is_null v2
          then None
          else (
            let v2 = !@(v2 +@ 0) in
            let module ID = C.Types.Id3v2 in
            let module MS = C.Types.Mpg123_string in
            let module MT = C.Types.Mpg123_text in
            let module MP = C.Types.Mpg123_picture in
            let read_string ms =
              let len = getf ms MS.fill in
              if len < 1
              then ""
              else (
                let a = CArray.from_ptr (getf ms MS.p) (len - 1) in
                char_array_as_string a)
            in
            let get_mpg123_string x =
              let ms = getf v2 x in
              if is_null ms then "" else read_string !@ms
            in
            let get_mpg123_texts num x =
              let len = getf v2 num in
              let ms = getf v2 x in
              if is_null ms
              then []
              else if len = 0
              then []
              else (
                let ms = CArray.from_ptr ms len in
                let cass = char_array_as_string in
                let f i acc =
                  if i = len
                  then List.rev acc
                  else (
                    let mt = CArray.get ms i in
                    { lang = cass @@ getf mt MT.lang;
                      id = cass @@ getf mt MT.id;
                      description = read_string (getf mt MT.description);
                      text = read_string (getf mt MT.text)
                    }
                    :: acc)
                in
                f 0 [])
            in
            let get_mpg123_pictures num x =
              let len = getf v2 num in
              let ms = getf v2 x in
              if is_null ms
              then []
              else if len = 0
              then []
              else (
                let ms = CArray.from_ptr ms len in
                let cass = char_array_as_string in
                let f i acc =
                  if i = len
                  then List.rev acc
                  else (
                    let mp = CArray.get ms i in
                    let size = getf mp MP.size in
                    let data = CArray.from_ptr (getf mp MP.data) (size - 1) in
                    { type_ = getf mp MP.type_;
                      description = read_string (getf mp MP.description);
                      mime_type = read_string (getf mp MP.mime_type);
                      size = getf mp MP.size;
                      data = cass data
                    }
                    :: acc)
                in
                f 0 [])
            in
            let get = get_mpg123_string in
            let get_texts = get_mpg123_texts in
            let get_pics = get_mpg123_pictures in
            Some
              { version = getf v2 ID.version;
                title = get ID.title;
                artist = get ID.artist;
                album = get ID.album;
                year = get ID.year;
                genre = get ID.genre;
                comment = get ID.comment;
                comment_list = get_texts ID.comments ID.comment_list;
                text = get_texts ID.texts ID.text;
                extra = get_texts ID.extras ID.extra;
                picture = get_pics ID.pictures ID.picture
              }))
      in
      Ok (v1, v2))

  let getformat mh =
    let rate = allocate int 0 in
    let channels = allocate int 0 in
    let encoding = allocate int 0 in
    let retval = mpg123_getformat mh rate channels encoding in
    if retval = ok
    then Ok { rate = !@rate; channels = !@channels; encoding = !@encoding }
    else Error (retval : error_code)

  let format_none mh = ok_unit_or_err (mpg123_format_none mh)

  let format_ mh ~rate ~channels ~encodings =
    ok_unit_or_err (mpg123_format mh rate channels encodings)
end

include Functions
