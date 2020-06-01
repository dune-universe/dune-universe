include Base (** @inline *)

module Bits = struct
  include Hardcaml.Bits

  let to_hstr b =
    let to_char i =
      Char.of_int_exn (if i < 10 then Char.to_int '0' + i else Char.to_int 'A' + i - 10)
    in
    let blen = width b in
    let slen = (blen + 3) / 4 in
    String.init slen ~f:(fun i ->
      let i = slen - i - 1 in
      let l = i * 4 in
      let h = min blen (l + 4) - 1 in
      to_char (to_int (select b h l)))
  ;;

  (* convert to integer using arbitrary precision. *)
  let to_ustr b =
    let max = 29 in
    (* safe max positive int bits *)
    if width b <= max
    then Int.to_string (to_int b)
    else (
      (* convert with big ints *)
      let rec f b acc =
        let ( +: ) = Big_int.add_big_int in
        let ( <<: ) = Big_int.shift_left_big_int in
        let to_big b = Big_int.big_int_of_int (to_int b) in
        if width b <= max
        then (* result *)
          (acc <<: width b) +: to_big b
        else (
          let t, b = sel_top b max, drop_top b max in
          f b ((acc <<: max) +: to_big t))
      in
      Big_int.(string_of_big_int (f b zero_big_int)))
  ;;

  (* signed conversion uses unsigned conversion with detection of sign *)
  let to_sstr b =
    let max = 29 in
    (* safe max positive int bits *)
    if width b <= max
    then Int.to_string (to_sint b)
    else if to_int (msb b) = 0
    then to_ustr b
    else "-" ^ to_ustr (~:b +:. 1)
  ;;
end

module Cyclesim = Hardcaml.Cyclesim
module Out_channel = Stdio.Out_channel
module In_channel = Stdio.In_channel
