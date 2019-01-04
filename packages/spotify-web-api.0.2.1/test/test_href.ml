open OUnit
open Spotify_web_api

type check_href_output =
  | Exception of exn
  | Ok

let test_check_href =
  let test ~msg ~mode ~input ~expected_output () =
    match expected_output with
    | Exception e ->
      assert_raises ~msg e (fun () -> Common.check_href mode input)
    | Ok ->
      Common.check_href mode input
  in
  "test_check_href" >:::
    [
      "test_valid_album" >::
        test ~msg:"Checking a valid album href"
          ~mode:`album
          ~input:"spotify:album:0123456789012345678901"
          ~expected_output:Ok;
      "test_valid_artist" >::
        test ~msg:"Checking a valid artist href"
          ~mode:`artist
          ~input:"spotify:artist:0123456789012345678901"
          ~expected_output:Ok;
      "test_valid_track" >::
        test ~msg:"Checking a valid track href"
          ~mode:`track
          ~input:"spotify:track:0123456789012345678901"
          ~expected_output:Ok;
      "test_garbage_album" >::
        test ~msg:"Checking that garbage fails to parse as a album href"
          ~mode:`album
          ~input:"spotify:wmnermfg:diobvdfjk"
          ~expected_output:(Exception Common.Invalid_href);
      "test_garbage_artist" >::
        test ~msg:"Checking that garbage fails to parse as a artist href"
          ~mode:`artist
          ~input:"spotify:wmnermfg:diobvdfjk"
          ~expected_output:(Exception Common.Invalid_href);
      "test_garbage_track" >::
        test ~msg:"Checking that garbage fails to parse as a track href"
          ~mode:`track
          ~input:"spotify:wmnermfg:diobvdfjk"
          ~expected_output:(Exception Common.Invalid_href);
    ]

let suite =
  "test_href" >:::
    [
      test_check_href;
    ]
