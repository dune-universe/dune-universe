(* A given identifier we want to download, here it's a file from the source
    code of FreeDink *)
let id = "swh:1:cnt:80131a360f0ae3d4d643f9e222591db8d4aa744c"

let url =
  (* We parse the string identifier to get a Swhid.Lang.identifier *)
  match Swhid.Parse.from_string id with
  | Error _e -> None
  | Ok id -> (
    (* We ask software heritage for an URL from which the object can be downloaded *)
    match Swhid.Download.content id with
    | Ok url -> Some url
    | Error _e -> None )

let () =
  match url with
  | None ->
    (* we didn't get an URL... *)
    Format.eprintf
      "Can't get a download URL, maybe the file hasn't been archived on SWH or \
       was invalid ?@.";
    exit 1
  | Some url ->
    (* we got a valid URL ! :D *)
    Format.printf "The file can be downloaded at url `%s`.@." url

(* The ouput of this executable should be:
 * `The file can be downloaded at url `https://archive.softwareheritage.org/api/1/content/sha1_git:80131a360f0ae3d4d643f9e222591db8d4aa744c/raw/`.`
 * *)
