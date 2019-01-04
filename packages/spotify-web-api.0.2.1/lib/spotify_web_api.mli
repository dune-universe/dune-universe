module Common : sig
  val base_uri : string

  exception Invalid_href

  type mode = [ `album | `artist | `track ]

  val string_of_mode : mode -> string

  val check_href : mode -> string -> unit
end

module Search : sig
  val search_albums: string -> Album_t.album_simplified Paging_t.paging Lwt.t
  val search_artists: string -> Artist_t.artist Paging_t.paging Lwt.t
  val search_tracks: string -> Track_t.track Paging_t.paging Lwt.t
end
