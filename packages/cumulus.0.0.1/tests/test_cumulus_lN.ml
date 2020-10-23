(* Copyright (C) 2020  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

let (%) f g x = f (g x)

module List = struct
  include List

  let foldi =
    let rec loop f i = function
     | [] -> Fun.id
     | x :: xs -> loop f (i + 1) xs % f i x
    in
    fun f -> loop f 0
end

module C = Cumulus

module Label = struct
  type t = string
  let compare = String.compare
  let random () = String.init (Random.int 6) (fun _ -> Char.chr (Random.int 26 + 0x61))
end

module Label_map = struct
  include Map.Make (Label)

  let add_int k v =
    update k (function None -> Some v | Some acc -> Some (acc + v))

  let random f n =
    let rec loop n acc =
      if n = 0 then acc else
      let k = Label.random () in
      acc |> add k (f ()) |> loop (n - 1)
    in
    loop n empty

(*
  let show show_value m = m |> bindings
    |> List.map (fun (k, v) -> Printf.sprintf "%s: %s" k (show_value v))
    |> String.concat ", "
    |> Printf.sprintf "{%s}"
*)
end

let test_lN r =
  let input = List.init r (fun _ -> React.E.create ()) in
  let xs = input |> List.map @@ fun (dx, _) ->
    C.integrate (Label_map.fold Label_map.add_int) dx Label_map.empty
  in
  let setters = List.map snd input in
  let expect_init = ref true in
  let y =
    let init xs =
      assert !expect_init;
      List.foldi (fun i ->
        Label_map.fold (fun k v ->
          Label_map.update k
            (function
             | None -> Some (v lsl i)
             | Some acc -> Some (acc + v lsl i))))
      xs Label_map.empty
    in
    let patch xdxs y =
      let y', dy' =
        List.foldi
          (fun i (_, dx) -> match dx with
           | None -> Fun.id
           | Some dx ->
              Label_map.fold
                (fun k dv (y, dy) ->
                  let y' = Label_map.add_int k (dv lsl i) y in
                  let dy' = Label_map.add_int k (dv lsl i) dy in
                  (y', dy'))
                dx)
          xdxs (y, Label_map.empty)
      in
      Cumulus.Patch (y', dy')
    in
    (match Random.bool (), xs with
     | true, [x1] ->
        C.l1 ~init:(fun x1 -> init [x1])
             ~patch:(fun (x1, dx1) -> patch [(x1, Some dx1)])
             x1
     | true, [x1; x2] ->
        C.l2 ~init:(fun x1 x2 -> init [x1; x2])
             ~patch:(fun xdx1 xdx2 -> patch [xdx1; xdx2])
             x1 x2
     | true, [x1; x2; x3] ->
        C.l3 ~init:(fun x1 x2 x3 -> init [x1; x2; x3])
             ~patch:(fun xdx1 xdx2 xdx3 -> patch [xdx1; xdx2; xdx3])
             x1 x2 x3
     | true, [x1; x2; x3; x4] ->
        C.l4 ~init:(fun x1 x2 x3 x4 -> init [x1; x2; x3; x4])
             ~patch:(fun xdx1 xdx2 xdx3 xdx4 -> patch [xdx1; xdx2; xdx3; xdx4])
             x1 x2 x3 x4
     | true, [x1; x2; x3; x4; x5] ->
        C.l5 ~init:(fun x1 x2 x3 x4 x5 -> init [x1; x2; x3; x4; x5])
             ~patch:(fun xdx1 xdx2 xdx3 xdx4 xdx5 ->
                      patch [xdx1; xdx2; xdx3; xdx4; xdx5])
             x1 x2 x3 x4 x5
     | true, [x1; x2; x3; x4; x5; x6] ->
        C.l6 ~init:(fun x1 x2 x3 x4 x5 x6 -> init [x1; x2; x3; x4; x5; x6])
             ~patch:(fun xdx1 xdx2 xdx3 xdx4 xdx5 xdx6 ->
                      patch [xdx1; xdx2; xdx3; xdx4; xdx5; xdx6])
             x1 x2 x3 x4 x5 x6
     | _ ->
        C.lN ~init ~patch xs)
  in
  let rec loop n_mod z =
    if n_mod = 0 then () else
    let dxs =
      List.init r @@ fun _ ->
        if Random.int 3 > 0 then None else
        Some (Label_map.random (fun _ -> Random.int 8 - 4) (Random.int 8))
    in
    let () =
      let step = React.Step.create () in
      List.iter2
        (fun set -> function None -> () | Some dx -> set ?step:(Some step) dx)
        setters dxs;
      React.Step.execute step
    in
    let z' =
      List.foldi
        (fun i -> function
         | None ->
            Fun.id
         | Some dx ->
            Label_map.fold (fun k dv -> Label_map.add_int k (dv lsl i)) dx)
        dxs z
    in
    assert (Label_map.equal Int.equal (Cumulus.value y) z');
    (* Printf.printf "%s\n%!" (Label_map.show Int.to_string z'); *)
    loop (n_mod - 1) z'
  in
  expect_init := false;
  loop 50 Label_map.empty

let () =
  for r = 1 to 400 do
    test_lN (Random.int r)
  done
