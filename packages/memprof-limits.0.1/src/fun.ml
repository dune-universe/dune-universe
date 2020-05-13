(* needs one with masking! see thread_map.ml *)
let with_resource ~acquire arg f ~(release : _ -> unit) =
  let r = acquire arg in
  match f r with
  | y -> release r ; y
  | exception e -> release r ; raise e
