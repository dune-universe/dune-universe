let id = "swh:1:rev:db21f0afdb54c16b265754ca599869fda0ca4bfc"

let url =
  match Swhid.Parse.from_string id with
  | Error e -> Error e
  | Ok id -> Swhid.Download.revision id

let () =
  match url with
  | Error e -> Format.eprintf "can't get download URL: `%s`@." e
  | Ok url ->
    Format.printf "you can download the revision at the URL: `%s`@." url
