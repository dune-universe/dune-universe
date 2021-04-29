(** Read a big-endian arbitrary length number *)
let get_z_be cs off len =
  let r = ref Z.zero in
  let base = Z.of_int 0x100 in
  for i = off to off + len - 1 do
    r := Z.add (Z.mul base !r) @@ Z.of_int @@ Cstruct.get_uint8 cs i
  done;
  !r

module Rsa = struct
  (** If public exponent is not 0x10001, it is unclear how to parse the key *)
  let check_public_exponent e =
    if not (Z.equal e (Z.of_int 0x10001)) then
      invalid_arg ("RSA_LTPA: invalid public exponent: " ^ Z.to_string e)

  module Private = struct
    type t = {
      e: Derivable.Z.t;
      d: Derivable.Z.t;
      p: Derivable.Z.t;
      q: Derivable.Z.t;
    }
    [@@deriving ord,eq,show]

    let decode cs =
      try
        let d_len = Int32.to_int @@ Cstruct.BE.get_uint32 cs 0 in
        let d = get_z_be cs 4 d_len in
        let e_off = 4 + d_len in
        let e_len = 3 in
        let e = get_z_be cs e_off e_len in
        check_public_exponent e;
        let p_len = d_len / 2 + 1 in
        let p_off = e_off + 3 in
        let p = get_z_be cs p_off p_len in
        let q = get_z_be cs (p_off + p_len) p_len in
        Result.Ok { e ; d ; p ; q }
      with Invalid_argument s -> Result.Error s
  end

  module Public = struct
    type t = {
      e: Derivable.Z.t;
      n: Derivable.Z.t;
    }
    [@@deriving ord,eq,show]

    let decode cs =
      try
        let e_off = Cstruct.len cs - 3 in
        let e_len = 3 in
        let e = get_z_be cs e_off e_len in
        check_public_exponent e;
        let n_len = e_off in
        let n = get_z_be cs 0 n_len in
        Result.Ok { e ; n }
      with Invalid_argument s -> Result.Error s
  end
end

module RSA = Rsa
