let () =
  Crowbar.add_test ~name:"never fail" Crowbar.[ bytes ] @@ fun input ->
  let _ = Unstrctrd.safely_decode input in ()
