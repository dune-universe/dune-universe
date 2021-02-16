open Examples
open Mlpost
open Command

let w = 2.

let a = Point.pt (Num.cm 0., Num.cm 0.)

let b = Point.pt (Num.cm w, Num.cm 0.)

let c = Point.pt (Num.cm 0., Num.cm w)

let d = Point.pt (Num.cm w, Num.cm w)

let up = Path.vec Point.up

let down = Path.vec Point.down

let left = Path.vec Point.left

let right = Path.vec Point.right

let double_headed = Arrow.add_foot Arrow.classic

let multiple_headed =
  Arrow.add_foot ~head:Arrow.head_triangle
    (Arrow.add_belt ~point:0.25
       ~head:(Arrow.head_triangle ~size:(Num.bp 10.))
       (Arrow.add_belt ~point:0.5 ~head:Arrow.head_triangle_full
          (Arrow.add_belt ~point:0.75
             ~head:(Arrow.head_triangle_full ~angle:140.)
             (Arrow.add_head
                ~head:(Arrow.head_triangle ~size:(Num.bp 15.))
                (Arrow.add_line Arrow.empty)))))

let serial_lines =
  Arrow.add_head
    (Arrow.add_line ~to_point:0.10
       ~pen:(Pen.scale (Num.bp 5.) Pen.square)
       (Arrow.add_line ~from_point:0.10 ~to_point:0.33
          (Arrow.add_line ~from_point:0.33 ~to_point:0.66 ~dashed:Dash.withdots
             (Arrow.add_line ~from_point:0.66 ~dashed:Dash.evenly Arrow.empty))))

let () =
  emit (Arrow.point_to_point a d);
  emit (Arrow.point_to_point b c);
  emit (Arrow.point_to_point c b);
  emit (Arrow.point_to_point a b);
  (* Some curved arrows *)
  emit (Arrow.point_to_point ~outd:up a d);
  emit (Arrow.point_to_point ~outd:up b c);
  emit (Arrow.point_to_point ~outd:right c b);
  emit (Arrow.point_to_point ~outd:up a b);
  (* Some double-headed arrows *)
  emit (Arrow.point_to_point ~kind:double_headed a d);
  emit (Arrow.point_to_point ~kind:double_headed ~outd:right c b);
  (* Some multiple-headed arrows *)
  emit (Arrow.point_to_point ~kind:multiple_headed a d);
  emit (Arrow.point_to_point ~kind:multiple_headed ~outd:right c b);
  (* Some arrows with multiple serial lines *)
  emit (Arrow.point_to_point ~kind:serial_lines a d);
  emit (Arrow.point_to_point ~kind:serial_lines ~outd:right c b);
  (* Some "implies" and "iff" *)
  emit (Arrow.point_to_point ~kind:Arrow.implies a d);
  emit (Arrow.point_to_point ~kind:Arrow.implies ~outd:right c b);
  emit (Arrow.point_to_point ~kind:Arrow.implies ~outd:up ~ind:up a b);
  emit (Arrow.point_to_point ~kind:Arrow.iff a d);
  emit (Arrow.point_to_point ~kind:Arrow.iff ~outd:right c b);
  emit (Arrow.point_to_point ~kind:Arrow.iff ~outd:up ~ind:up a b);

  (* A straight thick arrow *)
  emit (Arrow.draw_thick a d);
  emit (Arrow.draw_thick b c);
  emit (Arrow.draw_thick c b);
  emit (Arrow.draw_thick a b);
  (* Some curved thick arrows *)
  emit (Arrow.draw_thick ~outd:up a d);
  emit (Arrow.draw_thick ~outd:up b c);
  emit (Arrow.draw_thick ~outd:right c b);
  emit (Arrow.draw_thick ~outd:up a b);
  (* A snake thick arrow *)
  emit (Arrow.draw_thick ~outd:up ~ind:up a b);
  dump ()
