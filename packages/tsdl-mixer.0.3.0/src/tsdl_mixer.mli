(** SDL2_mixer bindings for use with Tsdl *)

module Mixer : sig

(** {1 General} *)

module Init : sig
  type t
  val ( + ) : t -> t -> t
  val test : t -> t -> bool
  val eq : t -> t -> bool
  val empty : t
  val flac : t
  val mikmod : t
  val modplug : t
  val mp3 : t
  val ogg : t
  val fluidsynth : t
end

type 'a result = 'a Tsdl.Sdl.result

val init : Init.t -> Init.t result
val quit : unit -> unit
val open_audio : int -> int -> int -> int -> unit result
val close_audio : unit -> unit
val query_spec :
  int Ctypes_static.ptr ->
  Unsigned.uint16 Ctypes_static.ptr -> int Ctypes_static.ptr -> int

val mix_channels : int
val default_frequency : int
val default_format : Tsdl.Sdl.Audio.format
val default_channels : int
val max_volume : int

type fading = NoFading | FadingOut | FadingIn
type music_type =
    None | Cmd | Wav | Mod | Mid | Ogg | Mp3 | Mp3_Mad | Flac | Modplug

(** {1 Samples} *)
type chunk
val load_wav_rw : Tsdl.Sdl.rw_ops -> int -> chunk result
val load_wav : string -> chunk result
val quickload_wav : Unsigned.uint8 Ctypes_static.ptr -> chunk result
val quickload_raw : Unsigned.uint8 Ctypes_static.ptr -> Unsigned.uint32 -> chunk result
val free_chunk : chunk -> unit
val get_num_chunk_decoders : unit -> int
val get_chunk_decoder : int -> string

(** {1 Channels} *)

val allocate_channels : int -> int
val channel_finished : (int -> unit) -> unit
val channel_post : int
val play_channel_timed :
  int -> chunk -> int -> int -> int result
val play_channel :
  int -> chunk -> int -> int result
val fade_in_channel_timed :
  int -> chunk -> int -> int -> int -> int result
val fade_in_channel :
  int -> chunk -> int -> int -> int result
val volume : int -> int -> int
val volume_chunk : chunk -> int -> int
val halt_channel : int -> unit result
val expire_channel : int -> int -> int
val fade_out_channel : int -> int -> int
val fading_channel : int -> fading
val pause : int -> unit
val resume : int -> unit
val paused : int -> bool
val playing : int option -> bool
val get_chunk : int -> chunk result

(** {1 Groups} *)

val reserve_channels : int -> unit result
val group_channel : int -> int -> bool result
val group_channels : int -> int -> int -> bool result
val group_available : int -> int result
val group_count : int -> int
val group_oldest : int -> int
val group_newer : int -> int
val fade_out_group : int -> int -> int
val halt_group : int -> unit result

(** {1 Music} *)

type music
val load_mus : string -> music result
val load_mus_rw : Tsdl.Sdl.rw_ops -> int -> music result
val load_mus_type_rw : Tsdl.Sdl.rw_ops -> music_type -> int -> music result
val free_music : music -> unit
val get_num_music_decoders : unit -> int
val get_music_decoder : int -> string
val get_music_type : music option -> music_type
val hook_music :
  (unit Ctypes_static.ptr -> Unsigned.uint8 Ctypes_static.ptr -> int -> unit) ->
  unit Ctypes_static.ptr -> unit
val hook_music_finished : (unit -> unit) -> unit
val get_music_hook_data : unit -> unit Ctypes_static.ptr

val play_music : music -> int -> int result
val fade_in_music : music -> int -> int -> int result
val fade_in_music_pos : music -> int -> int -> float -> int result

val volume_music : int -> int
val halt_music : unit -> unit result
val fade_out_music : int -> unit result
val set_music_cmd : string -> unit result
val set_synchro_value : int -> unit result
val get_synchro_value : unit -> int result
val set_sound_fonts : string -> unit result
val get_sound_fonts : unit -> string
val each_sound_font :
  (string -> unit Ctypes_static.ptr -> int) ->
  unit Ctypes_static.ptr -> unit result
val fading_music : unit -> fading
val pause_music : unit -> unit
val resume_music : unit -> unit
val rewind_music : unit -> unit
val paused_music : unit -> bool
val set_music_position : float -> int result
val playing_music : unit -> bool

(** {1 Effects} *)

val set_post_mix :
  (unit Ctypes_static.ptr ->
   Unsigned.uint8 Ctypes_static.ptr -> int -> unit) ->
  unit Ctypes_static.ptr -> unit

val effect_func_t :
  (int -> unit Ctypes_static.ptr -> int -> unit Ctypes_static.ptr -> unit)
    Ctypes.fn
val effect_done_t : (int -> unit Ctypes_static.ptr -> unit) Ctypes.fn
val register_effect :
  int ->
  (int -> unit Ctypes_static.ptr -> int -> unit Ctypes_static.ptr -> unit) ->
  (int -> unit Ctypes_static.ptr -> unit) ->
  unit Ctypes_static.ptr -> unit result
val unregister_effect :
  int ->
  (int -> unit Ctypes_static.ptr -> int -> unit Ctypes_static.ptr -> unit) ->
  unit result
val unregister_all_effects : int -> unit result
val effects_max_speed : string
val set_panning : int -> Unsigned.uint8 -> Unsigned.uint8 -> unit result
val set_position : int -> int -> Unsigned.uint8 -> unit result
val set_distance : int -> Unsigned.uint8 -> unit result
val set_reverse_stereo : int -> int -> unit result

end
