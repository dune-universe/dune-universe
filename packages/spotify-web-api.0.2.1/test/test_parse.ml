open OUnit

let data_dir = "../../../test/data"

let string_of_file filename =
  let path = Filename.concat data_dir filename in
  let chan = open_in path in
  try
    let length = in_channel_length chan in
    let buffer = Bytes.create length in
    really_input chan buffer 0 length;
    close_in chan;
    Bytes.to_string buffer
  with e ->
    close_in chan;
    raise e

let test_parse_album_search () =
  let results =
    string_of_file "album_search.json"
    |> Album_j.search_wrapper_of_string
  in
  assert_equal
    results
    Album_j.({
      albums = Paging_j.({
        href = "https://api.spotify.com/v1/search?query=cavalcade+of+dadaist&offset=0&limit=20&type=album";
        items = [
          {
            album_type = "album";
            available_markets = ["AR"; "BO"; "BR"; "CA"; "CL"; "CO"; "CR"; "DO"; "EC"; "GT"; "HN"; "MX"; "NI"; "PA"; "PE"; "PY"; "SV"; "US"; "UY"];
            external_urls = [
              "spotify", "https://open.spotify.com/album/5v4vDmq8pNO3TbegZmK5bL";
            ];
            href = "https://api.spotify.com/v1/albums/5v4vDmq8pNO3TbegZmK5bL";
            id = "5v4vDmq8pNO3TbegZmK5bL";
            images = Image_j.([
              {
                height = 640;
                url = "https://i.scdn.co/image/d2c396974cd76ab68e95deb6c996eb2c34033341";
                width = 640;
              };
              {
                height = 300;
                url = "https://i.scdn.co/image/2e4646f11b710bae7802d1dd85c634c6aa5f5ff0";
                width = 300;
              };
              {
                height = 64;
                url = "https://i.scdn.co/image/a389ad01fe08f0d1d994f55ac28d8a417c047158";
                width = 64;
              };
            ]);
            name = "Cavalcade Of Glee & Dadaist Happy Hardcore Pom Poms";
            uri = "spotify:album:5v4vDmq8pNO3TbegZmK5bL";
          };
        ];
        limit = 20;
        offset = 0;
        total = 1;
      })
    })

let test_parse_artist_search () =
  let results =
    string_of_file "artist_search.json"
    |> Artist_j.search_wrapper_of_string
  in
  assert_equal
    results
    Artist_j.({
      artists = Paging_j.({
        href = "https://api.spotify.com/v1/search?query=tania+bowra&offset=0&limit=20&type=artist";
        items = [
          {
            external_urls = [
              "spotify", "https://open.spotify.com/artist/08td7MxkoHQkXnWAYD8d6Q"
            ];
            followers = Followers_j.({
              href = None;
              total = 30;
            });
            genres = [];
            href = "https://api.spotify.com/v1/artists/08td7MxkoHQkXnWAYD8d6Q";
            id = "08td7MxkoHQkXnWAYD8d6Q";
            images = Image_j.([
              {
                height = 640;
                url = "https://i.scdn.co/image/f2798ddab0c7b76dc2d270b65c4f67ddef7f6718";
                width = 640;
              };
              {
                height = 300;
                url = "https://i.scdn.co/image/b414091165ea0f4172089c2fc67bb35aa37cfc55";
                width = 300;
              };
              {
                height = 64;
                url = "https://i.scdn.co/image/8522fc78be4bf4e83fea8e67bb742e7d3dfe21b4";
                width = 64;
              };
            ]);
            name = "Tania Bowra";
            popularity = 3;
            uri = "spotify:artist:08td7MxkoHQkXnWAYD8d6Q";
          }
        ];
        limit = 20;
        offset = 0;
        total = 1;
      })
    })

let test_parse_track_search () =
  let results =
    string_of_file "track_search.json"
    |> Track_j.search_wrapper_of_string
  in
  assert_equal
    results
    Track_j.({
      tracks = Paging_j.({
        href = "https://api.spotify.com/v1/search?query=will_smith&offset=0&limit=20&type=track";
        items = [
          {
            album = Album_j.({
              album_type = "album";
              available_markets = ["AR"; "AT"; "BE"; "BO"; "BR"; "BG"; "CA"; "CL"; "CO"; "CR"; "CY"; "CZ"; "DK"; "DO"; "DE"; "EC"; "EE"; "SV"; "FI"; "FR"; "GR"; "GT"; "HN"; "HK"; "HU"; "IS"; "IE"; "IT"; "LV"; "LT"; "LU"; "MY"; "MT"; "MX"; "NL"; "NI"; "NO"; "PA"; "PY"; "PE"; "PH"; "PL"; "PT"; "SG"; "SK"; "ES"; "SE"; "CH"; "TW"; "TR"; "UY"; "US"; "GB"; "AD"; "LI"; "MC"; "ID"];
              external_urls = [
                "spotify", "https://open.spotify.com/album/1eMrRFFbPPC2zgKGojsVxX";
              ];
              href = "https://api.spotify.com/v1/albums/1eMrRFFbPPC2zgKGojsVxX";
              id = "1eMrRFFbPPC2zgKGojsVxX";
              images = Image_j.([
                {
                  height = 640;
                  url = "https://i.scdn.co/image/a50c2eae749b0f4c5125ca639e4b34dda5f8db72";
                  width = 640;
                };
                {
                  height = 300;
                  url = "https://i.scdn.co/image/9ab898ff26fd366b925a34185cbd180f6a840b49";
                  width = 300;
                };
                {
                  height = 64;
                  url = "https://i.scdn.co/image/a947b4743b6fd4c36be1256811ea62c3e5853ff9";
                  width = 64;
                };
              ]);
              name = "OCTOPUS4";
              uri = "spotify:album:1eMrRFFbPPC2zgKGojsVxX";
            });
            artists = Artist_j.([
              {
                external_urls = [
                  "spotify", "https://open.spotify.com/artist/14u4KXVp0iXQil79EpxXGc";
                ];
                href = "https://api.spotify.com/v1/artists/14u4KXVp0iXQil79EpxXGc";
                id = "14u4KXVp0iXQil79EpxXGc";
                name = "The Algorithm";
                uri = "spotify:artist:14u4KXVp0iXQil79EpxXGc";
              };
            ]);
            available_markets = ["AR"; "AT"; "BE"; "BO"; "BR"; "BG"; "CA"; "CL"; "CO"; "CR"; "CY"; "CZ"; "DK"; "DO"; "DE"; "EC"; "EE"; "SV"; "FI"; "FR"; "GR"; "GT"; "HN"; "HK"; "HU"; "IS"; "IE"; "IT"; "LV"; "LT"; "LU"; "MY"; "MT"; "MX"; "NL"; "NI"; "NO"; "PA"; "PY"; "PE"; "PH"; "PL"; "PT"; "SG"; "SK"; "ES"; "SE"; "CH"; "TW"; "TR"; "UY"; "US"; "GB"; "AD"; "LI"; "MC"; "ID"];
            disc_number = 1;
            duration_ms = 261346;
            explicit = false;
            external_ids = [
              "isrc", "UK2D51400006";
            ];
            external_urls = [
              "spotify", "https://open.spotify.com/track/7FPxCA9KaWXV18r1Za878d";
            ];
            href = "https://api.spotify.com/v1/tracks/7FPxCA9KaWXV18r1Za878d";
            id = "7FPxCA9KaWXV18r1Za878d";
            name = "will_smith";
            popularity = 30;
            preview_url = "https://p.scdn.co/mp3-preview/860ab99fe6dfe6b0455b91f5a8039d08308fae0f";
            track_number = 4;
            uri = "spotify:track:7FPxCA9KaWXV18r1Za878d";
          };
        ];
        limit = 20;
        offset = 0;
        total = 1;
      })
    })

let suite =
  "test_parse" >:::
    [
      "test_parse_album_search" >:: test_parse_album_search;
      "test_parse_artist_search" >:: test_parse_artist_search;
      "test_parse_track_search" >:: test_parse_track_search;
    ]
