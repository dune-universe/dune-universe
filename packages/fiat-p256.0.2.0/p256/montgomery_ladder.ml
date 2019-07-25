let scalar_mult d p =
  let r0 = ref (Point.at_infinity ()) in
  let r1 = ref p in
  for i = 255 downto 0 do
    if Scalar.bit_at d i then (
      r0 := Point.add !r0 !r1;
      r1 := Point.double !r1 )
    else (
      r1 := Point.add !r0 !r1;
      r0 := Point.double !r0 )
  done;
  !r0
