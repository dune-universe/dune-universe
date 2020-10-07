let has_prefix ~prefix s =
  let prefix_len = String.length prefix in
  let len = String.length s in
  prefix_len <= len && String.sub s 0 prefix_len = prefix

let has_suffix ~suffix s =
  let suffix_len = String.length suffix in
  let len = String.length s in
  suffix_len <= len && String.sub s (len - suffix_len) suffix_len = suffix

let remove_prefix ~prefix s =
  let ls = String.length s and prefix_len = String.length prefix in
  if prefix_len <= ls && String.sub s 0 prefix_len = prefix then
    Some (String.sub s prefix_len (ls - prefix_len))
  else
    None

let remove_suffix ~suffix s =
  let ls = String.length s and suffix_len = String.length suffix in
  if suffix_len <= ls &&
    String.sub s (ls - suffix_len) (suffix_len) = suffix then
    Some (String.sub s 0 (ls - suffix_len))
  else
    None
