module%override Map = struct
  module%override Make (X : OrderedType) = struct
    let diff m0 m1 =
      merge (fun _key v0 v1 -> match v1 with None -> v0 | Some _ -> None) m0 m1
  end
end

module String_map = Map.Make (String)

let test () =
  let m0 = String_map.add "1" 1 (String_map.singleton "2" 2) in
  let m1 = String_map.singleton "2" () in
  let m_diff = String_map.diff m0 m1 in
  assert (String_map.find "1" m_diff = 1);
  assert (not (String_map.mem "2" m_diff))

let () = test ()
