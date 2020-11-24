type protocol_version =
    Protocol_version_40
  | Protocol_version_41

let protocol_version_to_string v = 
  let version = 
    match v with
    | Protocol_version_40 -> "4.0"
    | Protocol_version_41 -> "4.1"
  in
  version
