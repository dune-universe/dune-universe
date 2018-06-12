open Common

type midi = int * int * int * int * int

type action = Midi_out of midi

type t =
  { mutable beat_progress: float
  ; mutable bar_progress: float
  ; mutable beat: int option
  ; mutable bpm: int
  ; mutable bar_structure_length: int
  ; mutable bar_structure_strongs: int list
  ; mutable text_size: float
  ; mutable text_lines: Bytes.t array
  ; (* 3 Levels of mutability! *)
    mutable output_event: int * int * int * int * int -> unit
  ; mutable custom_keys: (int, action) Hashtbl.t
  ; mutable debug: bool }

let set_beat_progress t p =
  if abs_float (p -. t.beat_progress) > 0.01 then t.beat_progress <- p

let set_bar_progress t p =
  if abs_float (p -. t.bar_progress) > 0.01 then t.bar_progress <- p

let beat_on t n = t.beat <- Some n

let beat_off t = t.beat <- None
