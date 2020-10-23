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

open React

let (%) f g x = f (g x)

let lN_signal ?eq f sxs =
  S.map ?eq (f % List.rev) (S.merge ~eq:(==) (fun acc x -> x :: acc) [] sxs)

type ('a, 'da) change = Init of 'a | Patch of 'a * 'da | Keep of 'a
type ('a, 'da) update = 'a -> ('a, 'da) change

let value_of_change = function Init x | Patch (x, _) | Keep x -> x

type ('a, 'da) t = ('a, 'da) change signal

let const x = S.const (Init x)

let create x = S.create ~eq:(==) (Init x)

let of_event ex = S.hold ~eq:(==) (Init ()) (E.map (fun dx -> (Patch ((), dx))) ex)

let of_signal sx =
  let is_init = ref true in
  S.map ~eq:(==)
    (fun x -> if !is_init then (is_init := false; Init x) else Patch (x, ()))
    sx

let hold x0 ex = S.hold ~eq:(==) (Init x0) (E.map (fun x -> Patch (x, ())) ex)

let value s = value_of_change (S.value s)

let signal s = S.map ~eq:(==) value_of_change s

let changes s = S.changes s

let stop ?strong s = S.stop ?strong s

let trace f s = S.trace f s

let integrate f e x0 =
  let f' acc dx = Patch (f dx (value_of_change acc), dx) in
  S.fold ~eq:(==) f' (Init x0) e

let skip_keep s =
  let last_change = ref None in
  let aux c =
    (match c, !last_change with
     | Keep _, Some last_change -> last_change
     | Keep x, None -> let c = Init x in last_change := Some c; c
     | (Init _ | Patch _), _ -> last_change := Some c; c)
  in
  S.l1 ~eq:(==) aux s

let fold f e i =
  let f' acc dx = f dx (value_of_change acc) in
  skip_keep (S.fold ~eq:(==) f' (Init i) e)

let deduplicator () =
  let seen = ref None in
  fun c ->
    (match !seen with
     | Some seen when seen == c -> Keep (value_of_change c)
     | _ -> seen := Some c; c)

let lift1 ~init ~patch sx1 =
  let state = ref None in
  let f cx =
    (match !state with
     | None ->
        let y = init (value_of_change cx) in
        let cy = Init y in
        (state := Some cy; cy)
     | Some (Init y' | Patch (y', _) | Keep y' as cy') ->
        (match patch cx y' with
         | Keep _ -> cy'
         | cy -> (state := Some cy; cy)))
  in
  S.l1 ~eq:(==) f sx1

let lift2 ~init ~patch sx1 sx2 =
  let state = ref None in
  let dd1 = deduplicator () in
  let dd2 = deduplicator () in
  let f cx1 cx2 =
    (match !state with
     | None ->
        let y = init (value_of_change cx1) (value_of_change cx2) in
        let cy = Init y in
        (state := Some cy; cy)
     | Some (Init y' | Patch (y', _) | Keep y' as cy') ->
        (match patch (dd1 cx1) (dd2 cx2) y' with
         | Keep _ -> cy'
         | cy -> (state := Some cy; cy)))
  in
  S.l2 ~eq:(==) f sx1 sx2

let lift3 ~init ~patch sx1 sx2 sx3 =
  let state = ref None in
  let dd1 = deduplicator () in
  let dd2 = deduplicator () in
  let dd3 = deduplicator () in
  let f cx1 cx2 cx3 =
    (match !state with
     | None ->
        let y = init (value_of_change cx1) (value_of_change cx2)
                     (value_of_change cx3) in
        let cy = Init y in
        (state := Some cy; cy)
     | Some (Init y' | Patch (y', _) | Keep y' as cy') ->
        (match patch (dd1 cx1) (dd2 cx2) (dd3 cx3) y' with
         | Keep _ -> cy'
         | cy -> (state := Some cy; cy)))
  in
  S.l3 ~eq:(==) f sx1 sx2 sx3

let l1 ~init ~patch sx1 =
  let state = ref None in
  let f cx1 =
    (match !state, cx1 with
     | None, (Init x1 | Patch (x1, _))
     | Some _, Init x1 ->
        let y = init x1 in
        let cy = Init y in
        (state := Some cy; cy)
     | Some (Init y' | Patch (y', _) as cy'), Patch (x, dx) ->
        (match patch (x, dx) y' with
         | Keep _ -> cy'
         | cy -> (state := Some cy; cy))
     | Some (Keep _), _
     | _, Keep _ -> assert false)
  in
  S.l1 ~eq:(==) f sx1

let dedupe cx' cx = if cx == cx' then Keep (value_of_change cx) else cx

let patch_arg = function
 | Init _ -> assert false
 | Patch (x, dx) -> (x, Some dx)
 | Keep x -> (x, None)

let l2 ~init ~patch sx1 sx2 =
  let state = ref None in
  let f cx1 cx2 =
    let do_init () =
      let y = init (value_of_change cx1) (value_of_change cx2) in
      let cy = Init y in
      (state := Some (cx1, cx2, cy); cy)
    in
    let do_patch xdx1 xdx2 cy' =
      (match patch xdx1 xdx2 (value_of_change cy') with
       | Keep _ -> cy'
       | cy -> (state := Some (cx1, cx2, cy); cy))
    in
    (match !state with
     | None -> do_init ()
     | Some (cx1', cx2', cy') ->
        (match dedupe cx1' cx1, dedupe cx2' cx2 with
         | Init _, _ | _, Init _ -> do_init ()
         | (Patch _ | Keep _ as cx1d), (Patch _ | Keep _ as cx2d) ->
            do_patch (patch_arg cx1d) (patch_arg cx2d) cy'))
  in
  S.l2 ~eq:(==) f sx1 sx2

let l3 ~init ~patch sx1 sx2 sx3 =
  let state = ref None in
  let f cx1 cx2 cx3 =
    let do_init () =
      let y =
        init (value_of_change cx1) (value_of_change cx2) (value_of_change cx3)
      in
      let cy = Init y in
      (state := Some (cx1, cx2, cx3, cy); cy)
    in
    let do_patch xdx1 xdx2 xdx3 cy' =
      (match patch xdx1 xdx2 xdx3 (value_of_change cy') with
       | Keep _ -> cy'
       | cy -> (state := Some (cx1, cx2, cx3, cy); cy))
    in
    (match !state with
     | None -> do_init ()
     | Some (cx1', cx2', cx3', cy') ->
        (match dedupe cx1' cx1, dedupe cx2' cx2, dedupe cx3' cx3 with
         | Init _, _, _ | _, Init _, _ | _, _, Init _ -> do_init ()
         | (Patch _ | Keep _ as cx1d), (Patch _ | Keep _ as cx2d),
           (Patch _ | Keep _ as cx3d) ->
            do_patch (patch_arg cx1d) (patch_arg cx2d) (patch_arg cx3d) cy'))
  in
  S.l3 ~eq:(==) f sx1 sx2 sx3

let l4 ~init ~patch sx1 sx2 sx3 sx4 =
  let state = ref None in
  let f cx1 cx2 cx3 cx4 =
    let do_init () =
      let y =
        init (value_of_change cx1) (value_of_change cx2) (value_of_change cx3)
             (value_of_change cx4)
      in
      let cy = Init y in
      (state := Some (cx1, cx2, cx3, cx4, cy); cy)
    in
    let do_patch xdx1 xdx2 xdx3 xdx4 cy' =
      (match patch xdx1 xdx2 xdx3 xdx4 (value_of_change cy') with
       | Keep _ -> cy'
       | cy -> (state := Some (cx1, cx2, cx3, cx4, cy); cy))
    in
    (match !state with
     | None -> do_init ()
     | Some (cx1', cx2', cx3', cx4', cy') ->
        (match dedupe cx1' cx1, dedupe cx2' cx2, dedupe cx3' cx3,
               dedupe cx4' cx4 with
         | Init _, _, _, _ | _, Init _, _, _ | _, _, Init _, _
         | _, _, _, Init _ ->
            do_init ()
         | (Patch _ | Keep _ as cx1d), (Patch _ | Keep _ as cx2d),
           (Patch _ | Keep _ as cx3d), (Patch _ | Keep _ as cx4d) ->
            do_patch (patch_arg cx1d) (patch_arg cx2d)
                     (patch_arg cx3d) (patch_arg cx4d) cy'))
  in
  S.l4 ~eq:(==) f sx1 sx2 sx3 sx4

let l5 ~init ~patch sx1 sx2 sx3 sx4 sx5 =
  let state = ref None in
  let f cx1 cx2 cx3 cx4 cx5 =
    let do_init () =
      let y =
        init (value_of_change cx1) (value_of_change cx2) (value_of_change cx3)
             (value_of_change cx4) (value_of_change cx5)
      in
      let cy = Init y in
      (state := Some (cx1, cx2, cx3, cx4, cx5, cy); cy)
    in
    let do_patch xdx1 xdx2 xdx3 xdx4 xdx5 cy' =
      (match patch xdx1 xdx2 xdx3 xdx4 xdx5 (value_of_change cy') with
       | Keep _ -> cy'
       | cy -> (state := Some (cx1, cx2, cx3, cx4, cx5, cy); cy))
    in
    (match !state with
     | None -> do_init ()
     | Some (cx1', cx2', cx3', cx4', cx5', cy') ->
        (match dedupe cx1' cx1, dedupe cx2' cx2, dedupe cx3' cx3,
               dedupe cx4' cx4, dedupe cx5' cx5 with
         | Init _, _, _, _, _ | _, Init _, _, _, _ | _, _, Init _, _, _
         | _, _, _, Init _, _ | _, _, _, _, Init _ ->
            do_init ()
         | (Patch _ | Keep _ as cx1d), (Patch _ | Keep _ as cx2d),
           (Patch _ | Keep _ as cx3d), (Patch _ | Keep _ as cx4d),
           (Patch _ | Keep _ as cx5d) ->
            do_patch (patch_arg cx1d) (patch_arg cx2d) (patch_arg cx3d)
                     (patch_arg cx4d) (patch_arg cx5d) cy'))
  in
  S.l5 ~eq:(==) f sx1 sx2 sx3 sx4 sx5

let l6 ~init ~patch sx1 sx2 sx3 sx4 sx5 sx6 =
  let state = ref None in
  let f cx1 cx2 cx3 cx4 cx5 cx6 =
    let do_init () =
      let y =
        init (value_of_change cx1) (value_of_change cx2) (value_of_change cx3)
             (value_of_change cx4) (value_of_change cx5) (value_of_change cx6)
      in
      let cy = Init y in
      (state := Some (cx1, cx2, cx3, cx4, cx5, cx6, cy); cy)
    in
    let do_patch xdx1 xdx2 xdx3 xdx4 xdx5 xdx6 cy' =
      (match patch xdx1 xdx2 xdx3 xdx4 xdx5 xdx6 (value_of_change cy') with
       | Keep _ -> cy'
       | cy -> (state := Some (cx1, cx2, cx3, cx4, cx5, cx6, cy); cy))
    in
    (match !state with
     | None -> do_init ()
     | Some (cx1', cx2', cx3', cx4', cx5', cx6', cy') ->
        (match dedupe cx1' cx1, dedupe cx2' cx2, dedupe cx3' cx3,
               dedupe cx4' cx4, dedupe cx5' cx5, dedupe cx6' cx6 with
         | Init _, _, _, _, _, _ | _, Init _, _, _, _, _
         | _, _, Init _, _, _, _ | _, _, _, Init _, _, _
         | _, _, _, _, Init _, _ | _, _, _, _, _, Init _ ->
            do_init ()
         | (Patch _ | Keep _ as cx1d), (Patch _ | Keep _ as cx2d),
           (Patch _ | Keep _ as cx3d), (Patch _ | Keep _ as cx4d),
           (Patch _ | Keep _ as cx5d), (Patch _ | Keep _ as cx6d) ->
            do_patch (patch_arg cx1d) (patch_arg cx2d) (patch_arg cx3d)
                     (patch_arg cx4d) (patch_arg cx5d) (patch_arg cx6d) cy'))
  in
  S.l6 ~eq:(==) f sx1 sx2 sx3 sx4 sx5 sx6

let lN ~init ~patch sxs =
  let state = ref None in
  let f cxs =
    let do_init () =
      let xs = List.map value_of_change cxs in
      let y = init xs in
      let cy = Init y in
      (state := Some (cxs, cy); cy)
    in
    let do_patch xdxs cy' =
      (match patch xdxs (value_of_change cy') with
       | Keep _ -> cy'
       | cy -> (state := Some (cxs, cy); cy))
    in
    (match !state with
     | None -> do_init ()
     | Some (cxs', cy') ->
        let cxsd = List.map2 dedupe cxs' cxs in
        if List.exists (function Init _ -> true | _ -> false) cxsd then
          do_init ()
        else
          do_patch (List.map patch_arg cxsd) cy')
  in
  lN_signal ~eq:(==) f sxs

let bind s f =
  let epoch_in = ref 0 in
  let on_switch c =
    incr epoch_in;
    let i = !epoch_in in
    S.map ~eq:(==) (fun c' -> (i, c')) (f c)
  in
  let epoch_out = ref 0 in
  let on_output (i, c) =
    if i = !epoch_out then c else
    begin
      epoch_out := i;
      (match c with
       | Init x | Patch (x, _) -> Init x
       | Keep _ -> assert false)
    end
  in
  S.map ~eq:(==) on_output (S.bind ~eq:(==) s on_switch)
