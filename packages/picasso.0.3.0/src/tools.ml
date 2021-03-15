exception BackendError of string

let foi = float_of_int

let iof = int_of_float

(** removes ending zeros of a string *)
let trail_ending_zeros str =
  let len = String.length str in
  let rec aux cpt i =
    if i >= len then len
    else aux (if str.[len - i - 1] = '0' then cpt + 1 else cpt) (i + 1)
  in
  let nb_end_zero = aux 0 0 in
  String.sub str 0 (len - nb_end_zero)

let pp_float fmt f =
  let i_c = iof (ceil f) in
  let i_f = iof (floor f) in
  let i =
    if abs_float (float i_c -. f) < abs_float (float i_f -. f) then i_c
    else i_f
  in
  if abs_float (foi i -. f) < 0.001 then Format.fprintf fmt "%i" i
  else Format.fprintf fmt "%s" (trail_ending_zeros (Format.asprintf "%f" f))

let iterate f x0 next until =
  let rec loop () cur = if not (until cur) then loop (f cur) (next cur) in
  loop () x0

(* helper : project from a value n from [a;b] to [c;d] *)
let projection (a, b) (c, d) n =
  let perc (x, y) r = x +. (r *. (y -. x))
  and to_perc (x, y) r =
    if x < 0. then (r -. x) /. (y -. x) else (r -. x) /. (y -. x)
  in
  if b = a then c else perc (c, d) (to_perc (a, b) n)
