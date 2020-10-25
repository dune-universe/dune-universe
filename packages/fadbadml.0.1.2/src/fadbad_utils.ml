let user_assert b s =
  if not b then begin Printf.eprintf "User assertion failed: %s\n" s; exit 1 end
let internal_assert b s =
  if not b then begin Printf.eprintf "Internal error: %s\n" s; exit 1 end
