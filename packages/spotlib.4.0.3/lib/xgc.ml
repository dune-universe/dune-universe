open Gc

let used_words () =
  let c = get () in
  set { c with minor_heap_size = 1000; (* 1k words = 8k bytes *) };
  compact ();
  let s = stat () in
  let res = s.live_words in
  set c;
  res

let with_compacts f v =
  let used_before = used_words () in
  Exn.protect_with f v
    ~finally:(fun _ ->
      let used_after = used_words () in
      used_before, used_after)
