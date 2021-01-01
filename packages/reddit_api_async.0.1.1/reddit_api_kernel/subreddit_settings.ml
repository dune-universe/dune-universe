open! Core_kernel
include Json_object.Utils

include Json_object.Make_kinded_simple (struct
  let kind = "subreddit_settings"
end)
