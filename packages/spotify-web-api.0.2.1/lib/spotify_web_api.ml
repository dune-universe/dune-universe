module Common = struct
  let base_uri = "https://api.spotify.com/v1"

  let uid_length = 22

  type mode = [ `album | `artist | `track ]

  let string_of_mode = function
    | `album -> "album"
    | `artist -> "artist"
    | `track -> "track"

  exception Invalid_href

  let check_href mode href =
    try
      Scanf.sscanf href "spotify:%s@:%s"
        (fun mode_string uid ->
          if mode_string <> (string_of_mode mode)
          then raise Invalid_href;
          if (String.length uid) <> uid_length
          then raise Invalid_href)
    with Scanf.Scan_failure _ ->
      raise Invalid_href
end

module Remote = struct
  module C = Cohttp_lwt_unix

  let read_uri uri parse_fn =
    let open Cohttp_lwt_unix.IO in
    C.Client.call ~chunked:false `GET uri
    >>= (fun (_, body) ->
      Cohttp_lwt__Body.to_string body
      >>= (fun data -> return (parse_fn data)))
end

module Search = struct
  open Lwt

  let search mode query parse_fn =
    let uri = Uri.of_string
      (Printf.sprintf "%s/search?type=%s&q=%s"
        Common.base_uri (Common.string_of_mode mode) query)
    in
    Remote.read_uri uri parse_fn

  let search_albums query =
    search `album query Album_j.search_wrapper_of_string
    >|= (fun wrapper -> wrapper.Album_t.albums)

  let search_artists query =
    search `artist query Artist_j.search_wrapper_of_string
    >|= (fun wrapper -> wrapper.Artist_t.artists)

  let search_tracks query =
    search `track query Track_j.search_wrapper_of_string
    >|= (fun wrapper -> wrapper.Track_t.tracks)
end
