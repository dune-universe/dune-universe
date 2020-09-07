
(* quick test of Binary_heap *)

module E = struct
  type t = int
  let compare = Stdlib.compare
end
module H = Binary_heap.Make(E)

let dummy = 1729
let h = H.create ~dummy 0
let () = assert (H.is_empty h)
let () = assert (H.length h = 0)
let () = H.add h 42
let () = assert (not (H.is_empty h))
let () = assert (H.length h = 1)
let () = assert (H.minimum h = 42)
let x = H.pop_minimum h
let () = assert (x = 42)
let () = assert (H.is_empty h)

let () = for i = 200 downto -200 do H.add h i done
let () = assert (H.minimum h = -200)
let () = assert (H.length h = 401)
let () = for i = -200 to 200 do
           assert (H.minimum h = i);
           let x = H.pop_minimum h in
           assert (x = i);
           assert (H.length h = 200 - i)
         done

let () =
  let h = H.create ~dummy 42 in
  for _ = 1 to 1000 do
    if Random.bool () then H.add h (Random.int 1000);
    if not (H.is_empty h) && Random.int 3 = 0 then H.remove h
  done;
  for _ = 1 to 1000 do
    if Random.bool () then H.add h (Random.int 1000);
    if not (H.is_empty h) && Random.int 3 < 2 then H.remove h
  done;
  Format.eprintf "%d@." (H.length h)


