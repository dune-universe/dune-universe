(* USE WITH PRECAUTION on groups
    See
    https://en.wikipedia.org/wiki/Pontryagin_dual
    inspried by
    https://github.com/ethereum/research/blob/master/kzg_data_availability/kzg_proofs.py
    https://github.com/ethereum/research/blob/master/kzg_data_availability/fk20_single.py#L53
    More generally, see
    https://gitlab.com/dannywillems/ocaml-polynomial/-/blob/8351c266c4eae185823ab87d74ecb34c0ce70afe/src/polynomial.ml#L428
  *)
module type C = sig
  type group

  type scalar

  val zero : group

  val mul : group -> scalar -> group

  val add : group -> group -> group

  val sub : group -> group -> group

  val inverse_exn_scalar : scalar -> scalar

  val scalar_of_z : Z.t -> scalar
end

val fft :
  (module C with type group = 'group and type scalar = 'scalar) ->
  domain:'scalar array ->
  points:'group array ->
  'group array

val ifft :
  (module C with type group = 'group and type scalar = 'scalar) ->
  domain:'scalar array ->
  points:'group array ->
  'group array
