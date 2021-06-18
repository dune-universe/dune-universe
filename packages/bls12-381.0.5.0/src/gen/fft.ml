module type C = sig
  type group

  type scalar

  val mul : group -> scalar -> group

  val add : group -> group -> group

  val sub : group -> group -> group

  val inverse_exn_scalar : scalar -> scalar

  val scalar_of_z : Z.t -> scalar
end

let bitreverse n l =
  let r = ref 0 in
  let n = ref n in
  for _i = 0 to l - 1 do
    r := (!r lsl 1) lor (!n land 1) ;
    n := !n lsr 1
  done ;
  !r

let fft (type a b) (module G : C with type group = a and type scalar = b)
    ~domain ~points =
  (* See
     https://gitlab.com/dannywillems/ocaml-polynomial/-/blob/8351c266c4eae185823ab87d74ecb34c0ce70afe/src/polynomial.ml#L428
  *)
  let reorg_coefficients n logn coefficients =
    for i = 0 to n - 1 do
      let reverse_i = bitreverse i logn in
      if i < reverse_i then (
        let a_i = coefficients.(i) in
        let a_ri = coefficients.(reverse_i) in
        coefficients.(i) <- a_ri ;
        coefficients.(reverse_i) <- a_i )
    done
  in
  let n = Array.length domain in
  let logn = Z.log2 (Z.of_int n) in
  let output = Array.of_list points in
  reorg_coefficients n logn output ;
  let m = ref 1 in
  for _i = 0 to logn - 1 do
    let exponent = n / (2 * !m) in
    let k = ref 0 in
    while !k < n do
      for j = 0 to !m - 1 do
        let w = domain.(exponent * j) in
        (* odd *)
        let right = G.mul output.(!k + j + !m) w in
        output.(!k + j + !m) <- G.sub output.(!k + j) right ;
        output.(!k + j) <- G.add output.(!k + j) right
      done ;
      k := !k + (!m * 2)
    done ;
    m := !m * 2
  done ;
  Array.to_list output

let ifft (type a b) (module G : C with type group = a and type scalar = b)
    ~domain ~points =
  let power = Array.length domain in
  assert (power = List.length points) ;
  let points = fft (module G) ~domain ~points in
  let power_inv = G.inverse_exn_scalar (G.scalar_of_z (Z.of_int power)) in
  List.map (fun g -> G.mul g power_inv) points
