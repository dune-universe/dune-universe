let make_proxy () =
  Mpris_client_common.make_proxy "org.mpris.MediaPlayer2.banshee"

module Org_mpris_MediaPlayer2 =
  Mpris_client_generic.Org_mpris_MediaPlayer2

module Org_mpris_MediaPlayer2_Player =
  Mpris_client_generic.Org_mpris_MediaPlayer2_Player

module Org_mpris_MediaPlayer2_Playlists =
  Mpris_client_generic.Org_mpris_MediaPlayer2_Playlists
