let _ = Target.E (* ? Target.E *)

let _ = try true with Target.E (* ? Target.E *) -> false
