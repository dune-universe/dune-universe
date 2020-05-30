open Plebeia.Internal
open Test_utils
open Result

module RS = Random.State

let () =
  let st = RS.make_self_init () in
  test_with_context 100000 @@ fun ctxt ->
    for i = 0 to 1000 do
      let n = Gen.root 50 st in
      let sns_ = Node_tools.ls ctxt n in
      let segs_ = 
        List.sort compare
        @@ List.map (fun (seg, _) -> 
            Segment.to_sides seg) sns_
      in
      let sns = Node_tools.ls_rec ctxt n in
      let segs = 
        List.sort compare
        @@ List.map (fun (segs, _) -> 
            List.map Segment.to_sides segs) sns
      in
      let sns_', _ = from_Ok @@ Cursor_tools.ls (Cursor.(_Cursor (_Top, n, ctxt))) in
      let _, sns_'' = from_Ok @@ Cursor_tools.GenTraverse.ls (Cursor.(_Cursor (_Top, n, ctxt))) in
      (* Since the cursor is a bud, [ls] must return itself *)
      assert (List.length sns_' = 1);
      assert (List.length sns_'' = 1);

      let segs_' = 
        List.sort compare
        @@ List.map (fun (seg, _) -> 
            Segment.to_sides seg) sns_'
      in
      assert (segs_ = segs_');

      let sns', _ = from_Ok @@ Cursor_tools.ls_rec (Cursor.(_Cursor (_Top, n, ctxt))) in
      let _, sns'' = from_Ok @@ Cursor_tools.GenTraverse.ls_rec (Cursor.(_Cursor (_Top, n, ctxt))) in
      let segs' = 
        List.sort compare
        @@ List.map (fun (segs, _) -> 
            List.map Segment.to_sides segs) sns'
      in
      let segs'' = 
        List.sort compare
        @@ List.map (fun (segs, _) -> 
            List.map Segment.to_sides segs) sns''
      in
      assert (segs = segs');
      assert (segs = segs'');

      if i mod 100 = 0 then Format.eprintf "%d done@." i
    done
