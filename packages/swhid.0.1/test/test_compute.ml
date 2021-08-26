open Swhid.Lang

(* test content_identifier *)
let () =
  let test_cases =
    [| ("coucou\n", "swh:1:cnt:28d0af969b32e69a389087d7a267a2ecc05f1350")
     ; ("coucoucou\n", "swh:1:cnt:ffacc412c4e5ff55cafccd6e58bc58072c17ff6b")
    |]
  in
  Array.iter
    (fun (content, expected_identifier) ->
      let result = Swhid.Compute.content_identifier content in
      let result =
        match result with
        | None -> assert false
        | Some result -> result
      in
      let result = Format.asprintf "%a" Swhid.Pp.identifier result in
      let ok = result = expected_identifier in
      if not ok then
        Format.eprintf
          "error: expected identifier `%s` from content `%s` but got \
           identifier `%s`@."
          expected_identifier content result;
      assert ok )
    test_cases

(* test release_identifier *)
let () =
  let counter =
    let count = ref (-1) in
    fun () ->
      incr count;
      !count
  in
  let test_cases =
    [| ( "741b2252a5e14d6c60a913c77a6099abe73a854a"
       , Revision
       , "v2.6.14"
       , Some "Linus Torvalds <torvalds@g5.osdl.org>"
       , Some
           { Swhid.Compute.timestamp = 1130457753
           ; tz_offset = -420
           ; negative_utc = false
           }
       , Some
           "Linux 2.6.14 release\n\
            -----BEGIN PGP SIGNATURE-----\n\
            Version: GnuPG v1.4.1 (GNU/Linux)\n\n\
            iD8DBQBDYWq6F3YsRnbiHLsRAmaeAJ9RCez0y8rOBbhSv344h86l/VVcugCeIhO1\n\
            wdLOnvj91G4wxYqrvThthbE=\n\
            =7VeT\n\
            -----END PGP SIGNATURE-----\n"
       , "swh:1:rel:2b10839e32c4c476e9d94492756bb1a3e1ec4aa8" )
       (* No author *)
     ; ( "9ee1c939d1cb936b1f98e8d81aeffab57bae46ab"
       , Revision
       , "v2.6.12"
       , None
       , Some { timestamp = 1130457753; tz_offset = -420; negative_utc = false }
       , Some
           "This is the final 2.6.12 release\n\
            -----BEGIN PGP SIGNATURE-----\n\
            Version: GnuPG v1.2.4 (GNU/Linux)\n\n\
            iD8DBQBCsykyF3YsRnbiHLsRAvPNAJ482tCZwuxp/bJRz7Q98MHlN83TpACdHr37\n\
            o6X/3T+vm8K3bf3driRr34c=\n\
            =sBHn\n\
            -----END PGP SIGNATURE-----\n"
       , "swh:1:rel:26791a8bcf0e6d33f43aef7682bdb555236d56de" )
       (* No message *)
     ; ( "9ee1c939d1cb936b1f98e8d81aeffab57bae46ab"
       , Revision
       , "v2.6.12"
       , Some "Linus Torvalds <torvalds@g5.osdl.org>"
       , Some { timestamp = 1130457753; tz_offset = -420; negative_utc = false }
       , None
       , "swh:1:rel:b6f4f446715f7d9543ef54e41b62982f0db40045" )
       (* Empty message *)
     ; ( "9ee1c939d1cb936b1f98e8d81aeffab57bae46ab"
       , Revision
       , "v2.6.12"
       , Some "Linus Torvalds <torvalds@g5.osdl.org>"
       , Some { timestamp = 1130457753; tz_offset = -420; negative_utc = false }
       , Some ""
       , "swh:1:rel:71a0aea72444d396575dc25ac37fec87ee3c6492" )
       (* Negative utc *)
     ; ( "54e9abca4c77421e2921f5f156c9fe4a9f7441c7"
       , Revision
       , "20081029"
       , Some "Otavio Salvador <otavio@debian.org>"
       , Some { timestamp = 1225281976; tz_offset = 0; negative_utc = true }
       , Some "tagging version 20081029\n\nr56558\n"
       , "swh:1:rel:97c8d2573a001f88e72d75f596cf86b12b82fd01" )
       (* newline in author *)
     ; ( "c06aa3d93b78a2865c4935170030f8c2d7396fd3"
       , Revision
       , "0.3.2"
       , Some "Eugene Janusov\n<esycat@gmail.com>"
       , Some { timestamp = 1377480558; tz_offset = 600; negative_utc = false }
       , Some "Release of v0.3.2."
       , "swh:1:rel:5c98f559d034162de22d3ebeb95433e6f8885231" )
    |]
  in

  Array.iter
    (fun (target, target_type, name, author, date, message, expected_identifier) ->
      let result =
        Swhid.Compute.release_identifier target target_type name ~author date
          ~message
      in
      let result =
        match result with
        | None -> assert false
        | Some result -> result
      in
      let count = counter () in
      let result = Format.asprintf "%a" Swhid.Pp.identifier result in
      let ok = result = expected_identifier in
      if not ok then
        Format.eprintf
          "Test number: %d@.error: expected_identifier `%s` from release but \
           got identifier `%s`.@."
          count expected_identifier result;
      assert ok )
    test_cases

(* test revision_identifier *)
let () =
  let counter =
    let count = ref (-1) in
    fun () ->
      incr count;
      !count
  in
  let test_cases =
    [| ( "swh:1:rev:bc0195aad0daa2ad5b0d76cce22b167bc3435590"
       , "85a74718d377195e1efd0843ba4f3260bad4fe07"
       , [ "01e2d0627a9a6edb24c37db45db5ecb31e9de808" ]
       , "Linus Torvalds <torvalds@linux-foundation.org>"
       , Some
           { Swhid.Compute.timestamp = 1436739030
           ; tz_offset = -420
           ; negative_utc = false
           }
       , "Linus Torvalds <torvalds@linux-foundation.org>"
       , Some
           { Swhid.Compute.timestamp = 1436739030
           ; tz_offset = -420
           ; negative_utc = false
           }
       , Some "Linux 4.2-rc2\n"
       , [||] )
       (* synthetic rev *)
     ; ( "swh:1:rev:b2a7e1260492e344fab3cbf91bc13c91e05426fd"
       , "d11f00a6a0fea6055341d25584b5a96516c0d2b8"
       , []
       , "Software Heritage <robot@softwareheritage.org>"
       , Some { timestamp = 1437047495; tz_offset = 0; negative_utc = false }
       , "Software Heritage <robot@softwareheritage.org>"
       , Some { timestamp = 1437047495; tz_offset = 0; negative_utc = false }
       , Some "synthetic revision message\n"
       , [||] )
       (* with extra headers *)
     ; ( "swh:1:rev:010d34f384fa99d047cdd5e2f41e56e5c2feee45"
       , "85a74718d377195e1efd0843ba4f3260bad4fe07"
       , [ "01e2d0627a9a6edb24c37db45db5ecb31e9de808" ]
       , "Linus Torvalds <torvalds@linux-foundation.org>"
       , Some { timestamp = 1436739030; tz_offset = -420; negative_utc = false }
       , "Linus Torvalds <torvalds@linux-foundation.org>"
       , Some { timestamp = 1436739030; tz_offset = -420; negative_utc = false }
       , Some "Linux 4.2-rc2\n"
       , [| ("svn-repo-uuid", "046f1af7-66c2-d61b-5410-ce57b7db7bff")
          ; ("svn-revision", "10")
         |] )
       (* with_gpg_sign *)
     ; ( "swh:1:rev:44cc742a8ca17b9c279be4cc195a93a6ef7a320e"
       , "b134f9b7dc434f593c0bab696345548b37de0558"
       , [ "689664ae944b4692724f13b709a4e4de28b54e57"
         ; "c888305e1efbaa252d01b4e5e6b778f865a97514"
         ]
       , "Jiang Xin <worldhello.net@gmail.com>"
       , Some { timestamp = 1428538899; tz_offset = 480; negative_utc = false }
       , "Jiang Xin <worldhello.net@gmail.com>"
       , Some { timestamp = 1428538899; tz_offset = 480; negative_utc = false }
       , Some
           "Merge branch 'master' of git://github.com/alexhenrie/git-po\n\n\
            * 'master' of git://github.com/alexhenrie/git-po:\n\
           \  l10n: ca.po: update translation\n"
       , [| ( "gpgsig"
            , "-----BEGIN PGP SIGNATURE-----\n\
               Version: GnuPG v1.4.13 (Darwin)\n\n\
               iQIcBAABAgAGBQJVJcYsAAoJEBiY3kIkQRNJVAUQAJ8/XQIfMqqC5oYeEFfHOPYZ\n\
               L7qy46bXHVBa9Qd8zAJ2Dou3IbI2ZoF6/Et89K/UggOycMlt5FKV/9toWyuZv4Po\n\
               L682wonoxX99qvVTHo6+wtnmYO7+G0f82h+qHMErxjP+I6gzRNBvRr+SfY7VlGdK\n\
               wikMKOMWC5smrScSHITnOq1Ews5pe3N7qDYMzK0XVZmgDoaem4RSWMJs4My/qVLN\n\
               e0CqYWq2A22GX7sXl6pjneJYQvcAXUX+CAzp24QnPSb+Q22Guj91TcxLFcHCTDdn\n\
               qgqMsEyMiisoglwrCbO+D+1xq9mjN9tNFWP66SQ48mrrHYTBV5sz9eJyDfroJaLP\n\
               CWgbDTgq6GzRMehHT3hXfYS5NNatjnhkNISXR7pnVP/obIi/vpWh5ll6Gd8q26z+\n\
               a/O41UzOaLTeNI365MWT4/cnXohVLRG7iVJbAbCxoQmEgsYMRc/pBAzWJtLfcB2G\n\
               jdTswYL6+MUdL8sB9pZ82D+BP/YAdHe69CyTu1lk9RT2pYtI/kkfjHubXBCYEJSG\n\
               +VGllBbYG6idQJpyrOYNRJyrDi9yvDJ2W+S0iQrlZrxzGBVGTB/y65S8C+2WTBcE\n\
               lf1Qb5GDsQrZWgD+jtWTywOYHtCBwyCKSAXxSARMbNPeak9WPlcW/Jmu+fUcMe2x\n\
               dg1KdHOa34shrKDaOVzW\n\
               =od6m\n\
               -----END PGP SIGNATURE-----" )
         |] )
       (* No message *)
     ; ( "swh:1:rev:4cfc623c9238fa92c832beed000ce2d003fd8333"
       , "b134f9b7dc434f593c0bab696345548b37de0558"
       , [ "689664ae944b4692724f13b709a4e4de28b54e57"
         ; "c888305e1efbaa252d01b4e5e6b778f865a97514"
         ]
       , "Jiang Xin <worldhello.net@gmail.com>"
       , Some { timestamp = 1428538899; tz_offset = 480; negative_utc = false }
       , "Jiang Xin <worldhello.net@gmail.com>"
       , Some { timestamp = 1428538899; tz_offset = 480; negative_utc = false }
       , None
       , [||] )
       (* Empty message *)
     ; ( "swh:1:rev:7442cd78bd3b4966921d6a7f7447417b7acb15eb"
       , "b134f9b7dc434f593c0bab696345548b37de0558"
       , [ "689664ae944b4692724f13b709a4e4de28b54e57"
         ; "c888305e1efbaa252d01b4e5e6b778f865a97514"
         ]
       , "Jiang Xin <worldhello.net@gmail.com>"
       , Some { timestamp = 1428538899; tz_offset = 480; negative_utc = false }
       , "Jiang Xin <worldhello.net@gmail.com>"
       , Some { timestamp = 1428538899; tz_offset = 480; negative_utc = false }
       , Some ""
       , [||] )
       (* Only full name *)
     ; ( "swh:1:rev:010d34f384fa99d047cdd5e2f41e56e5c2feee45"
       , "85a74718d377195e1efd0843ba4f3260bad4fe07"
       , [ "01e2d0627a9a6edb24c37db45db5ecb31e9de808" ]
       , "Linus Torvalds <torvalds@linux-foundation.org>"
       , Some { timestamp = 1436739030; tz_offset = -420; negative_utc = false }
       , "Linus Torvalds <torvalds@linux-foundation.org>"
       , Some { timestamp = 1436739030; tz_offset = -420; negative_utc = false }
       , Some "Linux 4.2-rc2\n"
       , [| ("svn-repo-uuid", "046f1af7-66c2-d61b-5410-ce57b7db7bff")
          ; ("svn-revision", "10")
         |] )
    |]
  in
  Array.iter
    (fun ( expected_identifier
         , directory
         , parents
         , author
         , author_date
         , committer
         , committer_date
         , message
         , extra_headers ) ->
      let result =
        Swhid.Compute.revision_identifier directory parents ~author ~author_date
          ~committer ~committer_date extra_headers message
      in
      let result =
        match result with
        | None -> assert false
        | Some result -> result
      in
      let count = counter () in
      let result = Format.asprintf "%a" Swhid.Pp.identifier result in
      let ok = result = expected_identifier in
      if not ok then
        Format.eprintf
          "Test number: %d@.error: expected_identifier `%s` from revision but \
           got identifier `%s`@."
          count expected_identifier result;
      assert ok )
    test_cases

(* test directory identifier *)
let () =
  let test_cases =
    [| (* empty directory *)
       ("swh:1:dir:4b825dc642cb6eb9a060e54bf8d69288fbee4904", [])
     ; (* swh example *)
       ( "swh:1:dir:d7ed3d2c31d608823be58b1cbe57605310615231"
       , [ { Swhid.Compute.typ = "file"
           ; permissions = 33188
           ; name = "README"
           ; target = "37ec8ea2110c0b7a32fbb0e872f6e7debbf95e21"
           }
         ; { typ = "file"
           ; permissions = 33188
           ; name = "Rakefile"
           ; target = "3bb0e8592a41ae3185ee32266c860714980dbed7"
           }
         ; { typ = "dir"
           ; permissions = 16384
           ; name = "app"
           ; target = "61e6e867f5d7ba3b40540869bc050b0c4fed9e95"
           }
         ; { typ = "file"
           ; permissions = 33188
           ; name = "1.megabyte"
           ; target = "7c2b2fbdd57d6765cdc9d84c2d7d333f11be7fb3"
           }
         ; { typ = "dir"
           ; permissions = 16384
           ; name = "config"
           ; target = "591dfe784a2e9ccc63aaba1cb68a765734310d98"
           }
         ; { typ = "dir"
           ; permissions = 16384
           ; name = "public"
           ; target = "9588bf4522c2b4648bfd1c61d175d1f88c1ad4a5"
           }
         ; { typ = "file"
           ; permissions = 33188
           ; name = "development.sqlite3"
           ; target = "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"
           }
         ; { typ = "dir"
           ; permissions = 16384
           ; name = "doc"
           ; target = "154705c6aa1c8ead8c99c7915373e3c44012057f"
           }
         ; { typ = "dir"
           ; permissions = 16384
           ; name = "db"
           ; target = "85f157bdc39356b7bc7de9d0099b4ced8b3b382c"
           }
         ; { typ = "dir"
           ; permissions = 16384
           ; name = "log"
           ; target = "5e3d3941c51cce73352dff89c805a304ba96fffe"
           }
         ; { typ = "dir"
           ; permissions = 16384
           ; name = "script"
           ; target = "1b278423caf176da3f3533592012502aa10f566c"
           }
         ; { typ = "dir"
           ; permissions = 16384
           ; name = "test"
           ; target = "035f0437c080bfd8711670b3e8677e686c69c763"
           }
         ; { typ = "dir"
           ; permissions = 16384
           ; name = "vendor"
           ; target = "7c0dc9ad978c1af3f9a4ce061e50f5918bd27138"
           }
         ; { typ = "rev"
           ; permissions = 57344
           ; name = "will_paginate"
           ; target = "3d531e169db92a16a9a8974f0ae6edf52e52659e"
           }
         ; { typ = "dir"
           ; permissions = 16384
           ; name = "order"
           ; target = "62cdb7020ff920e5aa642c3d4066950dd1f01f4d"
           }
         ; { typ = "file"
           ; permissions = 16384
           ; name = "order."
           ; target = "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33"
           }
         ; { typ = "file"
           ; permissions = 16384
           ; name = "order0"
           ; target = "bbe960a25ea311d21d40669e93df2003ba9b90a2"
           }
         ] )
    |]
  in
  Array.iter
    (fun (expected_identifier, entries) ->
      let result = Swhid.Compute.directory_identifier entries in
      let result =
        match result with
        | None -> assert false
        | Some result -> result
      in
      let result = Format.asprintf "%a" Swhid.Pp.identifier result in
      let ok = result = expected_identifier in
      if not ok then
        Format.eprintf
          "error: expected_identifier `%s` from directory but got identifier \
           `%s`@."
          expected_identifier result;
      assert ok )
    test_cases

(* test snapshot identifier *)
let () =
  let test_cases =
    [| (* empty snapshot *)
       ("swh:1:snp:1a8893e6a86f444e8be8e7bda6cb34fb1735a00e", [])
     ; (* dangling branch *)
       ("swh:1:snp:c84502e821eb21ed84e9fd3ec40973abc8b32353", [ ("HEAD", None) ])
     ; (* unresolved *)
       ( "swh:1:snp:84b4548ea486e4b0a7933fa541ff1503a0afe1e0"
       , [ ("foo", Some ("bar", "alias")) ] )
     ; (* all types*)
       ( "swh:1:snp:6e65b86363953b780d92b0a928f3e8fcdd10db36"
       , [ ( "directory"
           , Some ("1bd0e65f7d2ff14ae994de17a1e7fe65111dcad8", "directory") )
         ; ( "content"
           , Some ("fe95a46679d128ff167b7c55df5d02356c5a1ae1", "content") )
         ; ("alias", Some ("revision", "alias"))
         ; ( "revision"
           , Some ("aafb16d69fd30ff58afdd69036a26047f3aebdc6", "revision") )
         ; ( "release"
           , Some ("7045404f3d1c54e6473c71bbb716529fbad4be24", "release") )
         ; ( "snapshot"
           , Some ("1a8893e6a86f444e8be8e7bda6cb34fb1735a00e", "snapshot") )
         ; ("dangling", None)
         ] )
    |]
  in
  Array.iter
    (fun (expected_identifier, branches) ->
      let result = Swhid.Compute.snapshot_identifier branches in
      let result =
        match result with
        | None -> assert false
        | Some result -> result
      in
      let result = Format.asprintf "%a" Swhid.Pp.identifier result in
      let ok = result = expected_identifier in
      if not ok then
        Format.eprintf
          "error: expected_identifier `%s` from snapshot but got identifier \
           `%s`@."
          expected_identifier result;
      assert ok )
    test_cases

(* test failures *)
let () =
  begin
    try
      let _id =
        Swhid.Compute.directory_identifier
          [ { typ = "rambo"; permissions = 3; name = "rambo"; target = "bine" }
          ]
      in
      assert false
    with
    | Invalid_argument _ -> ()
  end;

  begin
    try
      let _id =
        Swhid.Compute.release_identifier "rust" Release "" ~author:None None
          ~message:None
      in
      assert false
    with
    | Invalid_argument _ -> ()
  end;

  begin
    try
      let _id =
        Swhid.Compute.revision_identifier "yo" [ "lo" ] ~author:"Bach"
          ~author_date:None ~committer:"Hélène Grimaud" ~committer_date:None
          [||] None
      in
      assert false
    with
    | Invalid_argument _ -> ()
  end;
  try
    let _id =
      Swhid.Compute.snapshot_identifier [ ("do u know", Some ("bar", "àvin")) ]
    in
    assert false
  with
  | Invalid_argument _ -> ()
