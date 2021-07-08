(** Generate url safe slug from a [string]
    Data and general algorithm is taken from https://github.com/simov/slugify 
    
    This library assumes all strings are UTF-8.
    General algorithm:
    - Normalize input string with NFD (decompose form) to separate the accented part from base
    - Replace special characters that isn't url safe with a custom char map
    - Remove all non alphabets and numbers characters
    - Replace all whitespaces with [sep]

    Example:

    {[
      let vi = Charmap.mk_charmap [Slug_data.base; Slug_data.vi]
      
      let my_slug = slugify ~charmap: vi "my string"
    ]}    
    *)

module Charmap: sig
  type t
  
  val mk_charmap: (string * string ) list list -> t
  (** [mk_charmap [Slug_data.base; Slug_data.vi] generates charmap for use in [Slug.slugify]
      
      Default locale could be found in [Slug_data] or users could define custom replacement characters *)

  val base: t
  (** A charmap with data from [Slug_data.base] *)
end

val slugify: ?sep: string -> ?charmap: Charmap.t -> ?lowercase: bool -> string -> string
(** [slugify ?sep ?charmap ?lowercase string] generates slug from [string] 
    by replacing whitespaces with [sep] and replacement characters from [charmap] 
    
    [sep] defaults to "-" 
    [charmap] defaults to [Charmap.base] 
    [lowercase] defaults to [true] *)

module Slug_data: module type of Slug_data 