module IntMap = Map.Make(Int)

type capture = {
  capture_name : string option;
  capture_patterns : rule list;
}

and regex = Oniguruma.Encoding.utf8 Oniguruma.t

and match_ = {
  name : string option;
  pattern : regex;
  captures : capture IntMap.t;
}

and delim_kind = End | While

and delim = {
  delim_begin : regex;
  delim_end : string; (* Either an end or a while pattern *)
  delim_patterns : rule list;
  delim_name : string option;
  delim_content_name : string option;
  delim_begin_captures : capture IntMap.t;
  delim_end_captures : capture IntMap.t;
  delim_apply_end_pattern_last : bool;
  delim_kind : delim_kind;
}

and rule =
  | Match of match_
  | Delim of delim
  | Include_local of string
  | Include_scope of string
  | Include_self
  | Include_base

type repo_item_kind =
  | Repo_rule of rule
  | Repo_patterns of rule list

type repo_item = {
  repo_item_kind : repo_item_kind;
  repo_inner : (string, repo_item) Hashtbl.t;
}

type grammar = {
  name : string;
  scope_name : string;
  patterns : rule list;
  repository : (string, repo_item) Hashtbl.t;
}

type t = {
  by_name : (string, grammar) Hashtbl.t;
  by_scope_name : (string, grammar) Hashtbl.t;
}

type union =
  [ `Bool of bool
  | `Data of string
  | `Date of float * float option
  | `Float of float
  | `Int of int
  | `String of string
  | `Array of union list
  | `Dict of (string * union) list
  | `Null
  | `A of union list
  | `O of (string * union) list
  | `Assoc of (string * union) list
  | `List of union list ]

type plist =
  [ `Bool of bool
  | `Data of string
  | `Date of float * float option
  | `Float of float
  | `Int of int
  | `String of string
  | `Array of plist list
  | `Dict of (string * plist) list ]

type ezjsonm =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of ezjsonm list
  | `O of (string * ezjsonm) list ]

type yojson =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `Assoc of (string * yojson) list
  | `List of yojson list ]

exception Error of string

let create () = {
  by_name = Hashtbl.create 23;
  by_scope_name = Hashtbl.create 23;
}

let add_grammar t grammar =
  Hashtbl.add t.by_name (String.lowercase_ascii grammar.name) grammar;
  Hashtbl.add t.by_scope_name grammar.scope_name grammar

let find_by_name t name =
  Hashtbl.find_opt t.by_name (String.lowercase_ascii name)

let find_by_scope_name t = Hashtbl.find_opt t.by_scope_name

let error msg = raise (Error msg)
