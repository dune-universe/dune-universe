open Lwt
open Mpris_interfaces

module Org_mpris_MediaPlayer2 =
struct
  open Org_mpris_MediaPlayer2


  let quit proxy =
    OBus_method.call m_Quit proxy ()

  let raise proxy =
    OBus_method.call m_Raise proxy ()

  let can_quit proxy =
    OBus_property.make p_CanQuit proxy

  let fullscreen proxy =
    OBus_property.make p_Fullscreen proxy

  let can_set_fullscreen proxy =
    OBus_property.make p_CanSetFullscreen proxy

  let can_raise proxy =
    OBus_property.make p_CanRaise proxy

  let has_track_list proxy =
    OBus_property.make p_HasTrackList proxy

  let identity proxy =
    OBus_property.make p_Identity proxy

  let desktop_entry proxy =
    OBus_property.make p_DesktopEntry proxy

  let supported_uri_schemes proxy =
    OBus_property.make p_SupportedUriSchemes proxy

  let supported_mime_types proxy =
    OBus_property.make p_SupportedMimeTypes proxy
end

module Org_mpris_MediaPlayer2_Player =
struct
  open Org_mpris_MediaPlayer2_Player


  let next proxy =
    OBus_method.call m_Next proxy ()

  let previous proxy =
    OBus_method.call m_Previous proxy ()

  let pause proxy =
    OBus_method.call m_Pause proxy ()

  let play_pause proxy =
    OBus_method.call m_PlayPause proxy ()

  let stop proxy =
    OBus_method.call m_Stop proxy ()

  let play proxy =
    OBus_method.call m_Play proxy ()

  let seek proxy ~offset =
    OBus_method.call m_Seek proxy offset

  let set_position proxy ~trackid ~position =
    let trackid = OBus_proxy.path trackid in
    OBus_method.call m_SetPosition proxy (trackid, position)

  let open_uri proxy ~uri =
    OBus_method.call m_OpenUri proxy uri

  let seeked proxy =
    OBus_signal.make s_Seeked proxy

  let can_control proxy =
    OBus_property.make p_CanControl proxy

  let can_go_next proxy =
    OBus_property.make p_CanGoNext proxy

  let can_go_previous proxy =
    OBus_property.make p_CanGoPrevious proxy

  let can_pause proxy =
    OBus_property.make p_CanPause proxy

  let can_play proxy =
    OBus_property.make p_CanPlay proxy

  let can_seek proxy =
    OBus_property.make p_CanSeek proxy

  let minimum_rate proxy =
    OBus_property.make p_MinimumRate proxy

  let maximum_rate proxy =
    OBus_property.make p_MaximumRate proxy

  let rate proxy =
    OBus_property.make p_Rate proxy

  let shuffle proxy =
    OBus_property.make p_Shuffle proxy

  let loop_status proxy =
    OBus_property.make p_LoopStatus proxy

  let playback_status proxy =
    OBus_property.make p_PlaybackStatus proxy

  let metadata proxy =
    OBus_property.make p_Metadata proxy

  let volume proxy =
    OBus_property.make p_Volume proxy

  let position proxy =
    OBus_property.make p_Position proxy
end

module Org_mpris_MediaPlayer2_Playlists =
struct
  open Org_mpris_MediaPlayer2_Playlists


  let activate_playlist proxy ~playlist_id =
    let playlist_id = OBus_proxy.path playlist_id in
    OBus_method.call m_ActivatePlaylist proxy playlist_id

  let get_playlists proxy ~index ~max_count ~order ~reverse_order =
    let index = Int32.of_int index in
    let max_count = Int32.of_int max_count in
    let%lwt (context, ret) = OBus_method.call_with_context m_GetPlaylists proxy (index, max_count, order, reverse_order) in
    let ret = List.map (fun (x1, x2, x3) ->
      (OBus_proxy.make ~peer:(OBus_context.sender context) ~path:x1, x2, x3)) ret in
    return ret

  let playlist_changed proxy =
    OBus_signal.map_with_context
      (fun context playlist ->
         let playlist = (fun (x1, x2, x3) ->
           (OBus_proxy.make ~peer:(OBus_context.sender context) ~path:x1, x2, x3)) playlist in
         playlist)
      (OBus_signal.make s_PlaylistChanged proxy)

  let playlist_count proxy =
    OBus_property.map_r
      (fun x -> Int32.to_int x)
      (OBus_property.make p_PlaylistCount proxy)

  let orderings proxy =
    OBus_property.make p_Orderings proxy

  let active_playlist proxy =
    OBus_property.map_r_with_context
      (fun context x -> (fun (x1, x2) -> (x1, (fun (x1, x2, x3) ->
        (OBus_proxy.make ~peer:(OBus_context.sender context) ~path:x1, x2, x3)) x2)) x)
      (OBus_property.make p_ActivePlaylist proxy)
end
