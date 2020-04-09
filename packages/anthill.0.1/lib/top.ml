open Trie_search

module Env = Environment.Make (Trie)

module Eval = Evaluator.Make (Env) (TrieEngine)

