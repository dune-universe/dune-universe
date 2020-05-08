open Overlap_stubs
open Bigarray_compat

let c_nth_dims a i = Genarray.nth_dim a i
let fortran_nth_dims a i = Genarray.nth_dim a (Genarray.num_dims a - i - 1)

let genarray
  : type c.
    ('a, 'b, c) Genarray.t ->
    ('a, 'b, c) Genarray.t ->
    (int * int array * int array) option
  = fun a b ->
  let nth_dim = match Genarray.layout a (* = layout b *) with
    | C_layout -> c_nth_dims
    | Fortran_layout -> fortran_nth_dims in
  if (Genarray.num_dims a) <> (Genarray.num_dims b)
  then invalid_arg "Bigarray.Genarray.overlap" ;

  let src_a = ptr a in
  let src_b = ptr b in
  let len_a = Nativeint.of_int (Genarray.size_in_bytes a) in
  let len_b = Nativeint.of_int (Genarray.size_in_bytes b) in

  let len =
    let ( + ) = Nativeint.add in
    let ( - ) = Nativeint.sub in
    max 0n (min (src_a + len_a) (src_b + len_b) - max src_a src_b) in
  let len = Nativeint.to_int len in
  let len = len / kind_size_in_bytes (Genarray.kind a) in

  if src_a >= src_b && src_a < Nativeint.add src_b len_b
  then begin
    let offset = ref ((Nativeint.to_int (Nativeint.sub src_a src_b))
                      / kind_size_in_bytes (Genarray.kind a)) in
    let points = Array.make (Genarray.num_dims b) 0 in
    for i = Genarray.num_dims b - 1 downto 0
    do
      points.(i) <- !offset mod (nth_dim b i);
      offset := !offset / (nth_dim b i);
    done ;
    Some (len, Array.make (Genarray.num_dims a) 0, points)
  end else if src_b >= src_a && src_b < Nativeint.add src_a len_a
  then begin
    let offset = ref ((Nativeint.to_int (Nativeint.sub src_b src_a))
                      / kind_size_in_bytes (Genarray.kind b)) in
    let points = Array.make (Genarray.num_dims a) 0 in
    for i = Genarray.num_dims a - 1 downto 0
    do
      points.(i) <- !offset mod (nth_dim a i);
      offset := !offset / (nth_dim a i);
    done ;
    Some (len, points, Array.make (Genarray.num_dims b) 0)
  end else None

let array0 a b = match genarray (genarray_of_array0 a) (genarray_of_array0 b) with
  | Some (1, [||], [||]) -> true
  | Some _ -> assert false
  | None -> false

let array1 a b = match genarray (genarray_of_array1 a) (genarray_of_array1 b) with
  | Some (len, [| x |], [| y |]) -> Some (len, x, y)
  | Some _ -> assert false
  | None -> None

let array2 a b = match genarray (genarray_of_array2 a) (genarray_of_array2 b) with
  | Some (len, [| x; y; |], [| a; b; |]) -> Some (len, (x, y), (a, b))
  | Some _ -> assert false
  | None -> None

let array3 a b = match genarray (genarray_of_array3 a) (genarray_of_array3 b) with
  | Some (len, [| x; y; z; |], [| a; b; c; |]) -> Some (len, (x, y, z), (a, b, c))
  | Some _ -> assert false
  | None -> None
