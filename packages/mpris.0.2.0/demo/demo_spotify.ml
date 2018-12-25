open Lwt
open Lwt_io
module Spotify = Mpris_spotify.Org_mpris_MediaPlayer2_Player

let print_playback_status proxy =
  let%lwt playback_status =
    OBus_property.get (Spotify.playback_status proxy) in
  printlf "Playback status = %s" playback_status

let print_metadata_title proxy =
  let%lwt metadata =
    OBus_property.get (Spotify.metadata proxy) in
  let title = List.assoc "xesam:title" metadata in
  match title with
  | OBus_value.V.Basic (OBus_value.V.String s) ->
    printlf "Now playing: %s" s
  | _ ->
    printl "Couldn't get title - unexpected type in metadata"

    let%lwt _ =
      let%lwt proxy = Mpris_spotify.make_proxy () in

  try%lwt
    let%lwt () = Spotify.stop proxy in
    let%lwt () = print_playback_status proxy in
    let%lwt () = Spotify.open_uri proxy ~uri:"spotify:track:1R2SZUOGJqqBiLuvwKOT2Y" in
    let%lwt () = Lwt_unix.sleep 1.0 in
    let%lwt () = print_playback_status proxy in
    let%lwt () = print_metadata_title proxy in
    let%lwt () = Lwt_unix.sleep 5.0 in
    let%lwt () = Spotify.stop proxy in
    let%lwt () = Lwt_unix.sleep 1.0 in
    print_playback_status proxy
  with
    | OBus_bus.Name_has_no_owner _
    | OBus_bus.Service_unknown _ ->
      let%lwt () = printl "Spotify service not found - is it running?" in return ()
    | e -> raise e
