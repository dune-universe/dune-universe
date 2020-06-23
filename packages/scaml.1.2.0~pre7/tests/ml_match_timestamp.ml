open SCaml

(* Wow we can pattern-match with timestamps.  Not sure it is a good thing or not *)
let [@entry] main () () = 
  [],
  assert (
    match Global.get_now () with
    | Timestamp "1970-01-01T00:00:00Z" -> false
    | _ -> true
  )

