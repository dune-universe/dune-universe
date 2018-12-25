let make_proxy () =
  Mpris_client_common.make_proxy "org.mpris.MediaPlayer2.spotify"

(* Spotify doesn't implement the optional fullscreen and
 * can_set_fullscreen commands. *)
module Org_mpris_MediaPlayer2 = struct
  open Mpris_client_generic.Org_mpris_MediaPlayer2

  let quit = quit

  let raise = raise

  let can_quit = can_quit

  let can_raise = can_raise

  let has_track_list = has_track_list

  let identity = identity

  let desktop_entry = desktop_entry

  let supported_uri_schemes = supported_uri_schemes

  let supported_mime_types = supported_mime_types
end

module Org_mpris_MediaPlayer2_Player =
  Mpris_client_generic.Org_mpris_MediaPlayer2_Player
