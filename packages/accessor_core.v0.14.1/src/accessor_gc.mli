open! Core_kernel
open! Import

module Stat : sig
  type t =
    { minor_words : float
    ; promoted_words : float
    ; major_words : float
    ; minor_collections : int
    ; major_collections : int
    ; heap_words : int
    ; heap_chunks : int
    ; live_words : int
    ; live_blocks : int
    ; free_words : int
    ; free_blocks : int
    ; largest_free : int
    ; fragments : int
    ; compactions : int
    ; top_heap_words : int
    ; stack_size : int
    }
  [@@deriving accessors]
end
with type t := Gc.Stat.t
