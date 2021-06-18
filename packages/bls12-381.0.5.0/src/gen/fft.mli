module type C = sig
  type group

  type scalar

  val mul : group -> scalar -> group

  val add : group -> group -> group

  val sub : group -> group -> group

  val inverse_exn_scalar : scalar -> scalar

  val scalar_of_z : Z.t -> scalar
end

val fft :
  (module C with type group = 'group and type scalar = 'scalar) ->
  domain:'scalar array ->
  points:'group list ->
  'group list

val ifft :
  (module C with type group = 'group and type scalar = 'scalar) ->
  domain:'scalar array ->
  points:'group list ->
  'group list
