module Config : sig
  (** Type of stitch configuration *)
  type t

  (** Return a new config.
      [recording_dir] is the directory to which the recording files will be saved.
      It must either be an existing directory or a valid directory name in an existing directory.
      Recording is disabled if [recording_dir] is [None].
  *)
  val make :
    ?recording_dir: string ->
    unit ->
    t

  (** Return a config which field are set according to the following env variables:
      - STITCH_OCAML_RECORDING_DIR: recording_dir, do not record calls if unset
  *)
  val from_env : unit -> t
end

module Function : sig
  module Argument : sig
    type 'a t

    val make :
      to_yojson: ('a -> Yojson.Safe.json) ->
      of_yojson: (Yojson.Safe.json -> ('a, string) result) ->
      'a t
  end

  module Return_value : sig
    type 'a t

    val make :
      to_yojson: ('a -> Yojson.Safe.json) ->
      of_yojson: (Yojson.Safe.json -> ('a, string) result) ->
      equal: ('a -> 'a -> bool) ->
      show: ('a -> string) ->
      'a t
  end

  module Arity_2 : sig
    type ('a, 'b, 'ret) t

    val make :
      arg_a: 'a Argument.t ->
      arg_b: 'b Argument.t ->
      return_value: 'ret Return_value.t ->
      ('a, 'b, 'ret) t
  end

  module Arity_3 : sig
    type ('a, 'b, 'c, 'ret) t

    val make :
      arg_a: 'a Argument.t ->
      arg_b: 'b Argument.t ->
      arg_c: 'c Argument.t ->
      return_value: 'ret Return_value.t ->
      ('a, 'b, 'c, 'ret) t
  end
end

val create_2 :
  config: Config.t ->
  name: string ->
  signature: ('a, 'b, 'ret) Function.Arity_2.t ->
  ('a -> 'b -> 'ret) ->
  ('a -> 'b -> 'ret)

val create_3 :
  config: Config.t ->
  name: string ->
  signature: ('a, 'b, 'c, 'ret) Function.Arity_3.t ->
  ('a -> 'b -> 'c -> 'ret) ->
  ('a -> 'b -> 'c -> 'ret)

val verify_2 :
  config: Config.t ->
  name: string ->
  signature: ('a, 'b, 'ret) Function.Arity_2.t ->
  ('a -> 'b -> 'ret) ->
  OUnit2.test list

val verify_3 :
  config: Config.t ->
  name: string ->
  signature: ('a, 'b, 'c, 'ret) Function.Arity_3.t ->
  ('a -> 'b -> 'c -> 'ret) ->
  OUnit2.test list
