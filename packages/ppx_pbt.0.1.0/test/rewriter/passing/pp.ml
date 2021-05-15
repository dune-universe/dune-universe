let ignore () =
  Array.exists (fun x -> Filename.check_suffix x ".expected.ml") Sys.argv

let () =
  if ignore () then Ppx_pbt.ignore := true ;
  Ppxlib.Driver.standalone ()
