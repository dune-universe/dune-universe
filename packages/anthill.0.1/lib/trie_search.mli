module TrieEngine :
  sig
    type dict = Trie.t
    val pattern : dict -> Types.tile list -> Wordset.t
    val fit : dict -> Types.tile list -> char list
    val anagram :
      dict -> Types.tile list -> multi:bool -> all:bool -> Wordset.t
    val exists : dict -> string -> bool
  end
