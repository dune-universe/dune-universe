
(** This module implements the parsing functions for ACG data files,
    terms, lexical entries, and signature entries *)

open Logic.Lambda
open AcgData.Environment

(** [parse_data ~override:over ~output:out filename dirs e] parses the
    file [filename], looking it into the directories [dirs]. If the
    parse is successful, then it returns [Some e'] where [e'] is [e]
    with the data from [filenamne added]. Otherwise it returns
    [None]. The two first parameters indicates whether it is allowed
    to overwrite an already present signature or lexicon, and whether
    to ouput the new environment. *)
val parse_data : ?overwrite:bool -> ?output:bool -> string -> string list -> Environment.t -> Environment.t option       

(** [parse_term ~out ~col s sg] parses the term contained in sthe
   string [s] according to the signature [sg] and returns [None] if
   the parse fails and [Some (term,stype)] where [term] and [stype]
   correspond to [s]. If [col] is true, then, in case of error, the
   substring corrresponding to this error in the error message is
   colored.*)
val parse_term : ?output:bool -> ?parsing_context:(string * (Lexing.position * Lexing.position)) -> color:bool -> string -> Environment.Signature1.t -> (Lambda.term*Lambda.stype) option

(** [parse_term ~out ~col s lex] parses the term and type contained in
   sthe string [s] (in the form of [<term>:<stype>]) according to the
   lexicon [lex] and returns [None] if the parse fails and [Some
   (term,stype)] where [term] and [stype] correspond to [s]. It is
   expected that the term is to be parsed according to the object
   signature, and the type according to the abstract signature of
   [lex]. If [col] is true, then, in case of error, the substring
   corrresponding to this error in the error message is colored.*)
val parse_heterogenous_term : ?output:bool -> color:bool -> string -> Environment.Lexicon.t -> (Lambda.term*Lambda.stype) option

(** [parse_sig_entry ~col s sg e] parses the signature entry [s] and
   adds it to the signature [sg] in the environment [e] and returns
   [None] if parsing fails or [Some sg'] where [sg'] is the new
   signature. If [col] is true, then, in case of error, the substring
   corrresponding to this error in the error message is colored. *)
val parse_sig_entry : color:bool -> string -> Environment.Signature1.t -> AcgData.Environment.Environment.t -> Environment.Signature1.t option
                     
(** [parse_lex_entry ~col s lex e] parses the lexicon entry [s] and adds it
   to the lexicon [lex] in the environment [e] and returns [None] if
   parsing fails or [Some lex'] where [lex'] is the new lexicon.  If
   [col] is true, then, in case of error, the substring corrresponding
   to this error in the error message is colored.*)
val parse_lex_entry : color:bool -> string -> Environment.Lexicon.t -> AcgData.Environment.Environment.t -> Environment.Lexicon.t option
                     
                     

