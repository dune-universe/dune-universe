(*---------------------------------------------------------------------------
   Copyright (c) 2017 Tony Wuersch. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   sslconf 0.8.3 - commit 0806f5ae3774c30cc8ee18aaabe9d106d7816457
  ---------------------------------------------------------------------------*)

(** an OCaml version of Openssl's NCONF library

    Version {e 0.8.3 } — {{: https://github.com/awuersch/sslconf}homepage} *)

(** Module [Sslconf]: a parser for Openssl config files.

    Openssl config file documentation:
    {{: https://www.openssl.org/docs/manmaster/man5/config.html} config.html}
 *)

(** Openssl config files are ubiquitous in books, in documentation,
    and in Stack Overflow answers.

    Definition of X509 certificates in Openssl is usually done by
    Openssl config files.

    This module reproduces the Openssl NCONF implementation.  
    Openssl NCONF is part of the Openssl crypto library (-libcrypto).

    This module attaches no semantics to the strings and stacks it
    returns. It is similar to Openssl NCONF in this way.

    Openssl attaches semantics to the CONF data structure returned by
    Openssl NCONF,
    {e via} commands such as {e openssl x509} or {e openssl ca}.

    This implementation is backed by standard OCaml hash tables and
    stacks. Some may view this as a weakness.
    Openssl's NCONF implementation draws on custom hash tables and
    stacks defined in Openssl's crypto library.

    Openssl code:
    {{: https://github.com/openssl/openssl/blob/master/include/openssl/conf.h} conf.h}
    {{: https://github.com/openssl/openssl/blob/master/crypto/conf/conf_def.c} conf_def.c}
    {{: https://github.com/openssl/openssl/blob/master/crypto/conf/conf_api.c} conf_api.c}
    {{: https://github.com/openssl/openssl/blob/master/crypto/conf/conf_lib.c} conf_lib.c}
    {{: https://github.com/openssl/openssl/blob/master/crypto/bio/bss_fd.c} bss_fd.c}

    {1 Parser Overview}

    Lines recognized by the parser are:
    {ul {- comment lines, i.e.,
           {ul {- empty, or all whitespace;}
               {- (Win32 only) a semicolon ([';']) before non-white-space; or}
               {- whitespace, then a hash char (['#']), then arbitrary data;}}}
        {- section lines, i.e.,
           {ul {- a (["[<section>]"]) string inside square brackets; or}}}
        {- value lines, i.e.,
           {ul {- a (["<name> = <value>"]) assignment.}}}}

    Openssl config file documentation:
    {{: https://www.openssl.org/docs/manmaster/man5/config.html} config.html}

    The Openssl config file documentation is enough for most users, but
    is not a complete specification.

    What follows is detail for maintainers.

    {1 First Pass - Line Recognition}

    If the parser sees an escape char (['\\']) at the end of a line, it
    combines the line with the next line.

    The escape char may itself be escaped. If escaped, the escape char
    loses its special meaning as a line continuation mark.

    {2 Bounded Reads and Continuation Chars}

    The parser reads up to a newline, or a maximum length if no newline
    is found. If no newline is found, the parser does another read,
    and so on, until a newline is found or end of file is is detected.

    The max number of characters the parser reads in one read is 510.
    This number is dictated by a need for C code to put null bytes in a
    512 byte buffer.
    
    This implementation follows the 510 char rule.
    A config file valid in Openssl will still be valid here.

    If the parser sees an escape char (['\\']) at the end of a read,
    i.e., as the 510th character,
    it will apply line continuation logic -- even if
    a newline is not seen at the end.
    
    Both newlines and line continuation characters are removed from
    the strings passed to the parser in its second pass.

    {1 Second Pass - Configuration Recognition}

    {2 Error reporting - line and column numbers}

    Error reporting reports line and column numbers.
    Line numbers are relative to the file being read.

    The first line is line 1. The first column is column 1.

    {1 Comment lines}

    A comment line is either an empty line, a line of all whitespace,
    or whitespace followed by a hash char.

    In Win32 only, a line starting with a semicolon is also a comment.

    Comment lines are ignored and not saved.

    The end of a value line may have a comment.
    Value line comments start with a hash char.
    Any text up to end of line can follow the hash char.

    {1 Section lines}

    A section line has a (["[<section>]"]) string surrounded by square
    brackets. The string names a section.
    
    Whitespace is trimmed from both ends of the string.

    A section may contain
    alphanumerics, underscores, punctuation, or whitespace.

    Alphanumerics are any letter (from ['a'] to [z] or ['A'] to ['Z'])
    or a number (from ['0'] to ['9']).

    Underscores are one or more underscore ('_') characters.

    Punctuation is one or more of the following characters:
    (['!'], ['.'], ['%'], ['&'], ['*'], ['+'], [';'],
     ['?'], ['@'], ['^'], ['~'], ['|'], or ['-']).

    A section may also contain {e escaped} characters.
    An escaped character immediately follows an escape (['\\']) char.
    Any character may be an escaped character.

    If an escaped character is (['r'], ['n'], ['b'], or ['t']), it
    is translated into a whitespace char (["\r", "\n", "\b", or "\t"]).
    Otherwise, it is retained {e as is}.

    Escape chars are stripped out from a section. Escaped chars
    therefore appear in section names as if they were not escaped.

    The section named in a section line becomes the default section
    for lines which follow.
    It remains the default until a new section line is read.

    {1 Value lines}

    A line with a (["<name> = <value>"]) assignment is a value line.
    A value line always has a name, an equal sign, and a value.
    Whitespace before or after the equal sign is optional.

    A value line assigns a value to a name in a section.

    {2 Names}

    Lexical rules for a name (qualified or unqualified) are more
    restrictive than for a section in a section line.

    Whitespace is trimmed from both ends of a name.

    A name may contain alphanumerics, underscores, and punctuation.
    A name may not contain whitespace.

    As with section names, escaped characters are allowed.
    An escaped character immediately follows an escape (['\\']) char.
    Any character may be an escaped character.

    Unlike with section names, escape chars are not stripped.
    An escape char goes in the hash table as part of a name.

    A name can be qualified or unqualified.
    
    A qualified name splits
    into two parts, separated by a two-colon string (["::"]).
    The first part of a qualified name is the section.
    The second part is the name.

    If the section of a qualified name is not in the hash table, a
    new entry for the section is added.
    
    An unqualified name belongs to the default section.
    The default section is the section named by the most recent
    previous section line, or "default" if no section line has
    occured yet.

    {2 Values}

    A value is a sequence of parts,
    starting with the first non-whitespace character
    after the equal sign on a value line.

    A part is one of three kinds:
    {ul {- wrapped by double or single quotes;}
        {- a name substitution; or}
        {- regular text.}}

    Whitespace is stripped from the start and end of a value.
    Whitespace following the equal sign, and whitespace,
    either to the end of a line,
    or up to a comment hash char if present, is ignored.

    {3 Quote wrapped parts}

    Unix and Cygwin use default quote wrapping.

    Default quote wrapped parts accept any character {e as is}, and
    also allow quotes if preceded immediately by escape chars.

    Escape chars and
    the quotes which wrap surrounded parts
    are stripped.

    Win32 uses double quote wrapping.
    Double quote wrapped parts accept any character {e as is},
    unless it is a double quote. There is one exception.
    A double quote may appear inside a double quote wrapped
    string, if a double quote immediately precedes it.

    Wrapping double quotes, and double quotes which immediately
    precede double quotes, are stripped.

    {3 Name substitution}

    Name substitution starts with a (['$']) character.
    The (['$']) character should be followed by a name.
    The name may be optionally wrapped
    by parentheses (["()"]) or curly brackets (["{}"]).
    The [subst] evaluator replaces
    the (['$']), the optional wrapping, and the name
    with the value corresponding to the name and its section.

    If the section is not (["ENV"]),
    a value is found from a hashtable with section+name keys.
    If the section is (["ENV"]), 
    a value is found from the process environment.

    If substitution fails, the parser returns an error,
    reporting that no value could be found for the name.

    A name given for name substitution may only contain
    alphanumeric or underscore characters.
    It may not contain punctuation or whitespace,
    unless these are escaped {e via} an escape (['\\']) character.
    If an escaped character is in a name,
    its escape char is {e retained},
    along with the escaped character in the name.

    {3 Regular value text, i.e., unwrapped parts}

    Regular text accepts any character other than
    single quotes,
    double quotes, or
    the (['$']) substitution character.

    It may still have
    {ul {- escaped characters; or}
        {- hash chars, for comments to end of line.}}

    {4 Escaped chars in regular text}

    Regular text can have escaped characters.
    An escaped character immediately follows an escape (['\\']) char.

    If an escaped character is (['r'], ['n'], ['b'], or ['t']), it
    is translated into a whitespace char (["\r", "\n", "\b", or "\t"]).
    If it is not one of these characters, it is retained {e as is}.

    The special meanings of escaped characters are cancelled. For
    example, one can precede a double quote by an escape char, to
    cancel its usual meaning of starting a quote-wrapped part.

    Escape chars are stripped. They don't appear in hash table values.
    Escaped characters remain.

    {4 Hash chars in regular text start comments to end of line}

    An unescaped hash (['#']) char starts a comment to end of line.
    Any data following the hash char, up to the end of a line, is ignored.

    {4 Non-special chars in regular text}

    Characters not belonging to wrapped segments or name substitutions
    can be any character.
    For example, whitespace can occur in the middle of a value.
*)

module Buf : sig
  (** Sslconf buffer. *)

  (** This implements Openssl's buffer allocation policy in an OCaml runtime.

      {ul {- Per-buffer memory allocation is limited to 2**[nbits]-1.}
      {- A buffer has a desired length, which is null byte initialized.}
      {- Buffer memory allocation is 4/3 times desired length.}}

      from Openssl:
      {{: https://github.com/openssl/openssl/blob/master/include/openssl/buffer.h} buffer.h}
      {{: https://github.com/openssl/openssl/blob/master/crypto/buffer/buffer.c} buffer.c}
  *)

  (** An Sslconf buffer supports an underlying [Bytes.t] buffer.
      [empty nbits] is the constructor.
      Max allocated size is 2**[nbits]-1 bytes.
      A good default [nbits] value is 31.
      [length] bytes are expected, These bytes are initialized to a null value.
      Dynamic allocated size is (floor((length + 3) / 3) * 4) bytes. *)

  type t
  (** Buffer type. *)

  val empty : int -> t
  (** [empty nbits] creates an empty buffer. *)

  val data : t -> Bytes.t
  (** [data buf] returns the underlying [Bytes.t] buffer. *)

  val extend : t -> int -> (t, (string * int * int * int)) Rresult.result
  (** [extend buf length] extends [buf] to support [length] bytes.
      [Ok buf] or [Error (msg length nbits allocation_limit)] is returned.
      New bytes up to [Bytes.length (Buf.data buf)] are initialized
      to null. An error occurs, and no allocation is done, if [length]
      would cause an allocation greater than buffer max allocated size. *)
end

module Bio : sig
  (** Basic I/O. *)

  (** Openssl code:
      {{: https://github.com/openssl/openssl/blob/master/include/openssl/crypto.h} crypto.h}
      {{: https://github.com/openssl/openssl/blob/master/crypto/bio/bio_lib.c} bio_lib.c}
  *)

  val gets : in_channel -> Bytes.t -> int -> int -> int
  (** [gets ic buf pos max] gets a line from [ic] and puts it at [pos] in [buf].
      Number of bytes read is returned.
      If 0 bytes are read, then [ic] is at end of file.
      Line length is limited to [max] bytes if no newline is found.
      The newline is included in the line if a newline is found. *)
end

type t
(** Sslconf type *)

val create : unit -> t
(** Initialize an Sslconf instance *)

val nbits : int option ref
(** [FOR TESTING ONLY] Limits buffer allocated memory to a max bit length. *)

type error =
  | Open of string
  | Extend of string * int * int * int
  | Parse of string * int * int * string * string
(** Open (msg):
    {ul {- open failure message}}
    Extend (msg nbits max_length max_alloc):
    {ul {- failure message;}
        {- number of bits allowed in buffer size;}
        {- requested new length of buffer;}
        {- allocation limit of buffer size.}}
    Parse (file lineno col proc msg):
    {ul {- file name;}
        {- line number where error was seen;}
        {- column position where error was seen;}
        {- procedure name in which error was seen;}
        {- error message.}} *)

val string_of_error : error -> string
(** [string_of_error error] converts an error to a user-readable string. *)

val conf_load_file : t -> string -> (unit, error) Rresult.result
(** [conf_load_file conf filename] loads a file into a {!t} instance.
    The error instance describes only the first failure found. *)

val conf_get_value : ?conf:t -> ?section:string -> string -> string option
(** [conf_get_value ?conf ?section name] gets an optional value from a name.
    A string option is returned.

    See Openssl [NCONF_get_string()]
    in {{: https://github.com/openssl/openssl/blob/master/crypto/conf/conf_lib.c} conf_lib.c}.
    If a config instance is provided,
    its hash table with section+name keys is searched.
    If no config instance is provided, the process environment is searched.
    If a section is provided, then:
    {ul {- if the section is ["ENV"], the process environment is searched.}
        {- else the section and name are made into a pair,
           and the pair is searched for in the hash table.}}
    If no section is provided, or the search in a provided section fails,
    then section "default" is used with the name. *)

type stack = (string * string) Stack.t
(** A stack with (name, value) entries. *)

val conf_get_section : t -> string -> stack option
(** [conf_get_section conf section]
    gets an optional stack of name-value pairs.

    See Openssl function [NCONF_get_section()]
    in {{: https://github.com/openssl/openssl/blob/master/crypto/conf/conf_lib.c} conf_lib.c}.
    A hash table with section names in the config instance is searched
    for a section.
    If a stack is returned, and <name> is in a name-value pair on it, then
    [conf_get_value conf section name] will get its most recent value.  *)

val sexp_of_stack : stack -> Sexplib.Sexp.t
(** [sexp_of_stack conf section]
    converts a config instance section stack to an s-expression *)

val stack_of_sexp : Sexplib.Sexp.t -> stack
(** [stack_of_sexp sexp]
    converts an s-expression to a config instance section stack *)

val sexp_of_conf : t -> Sexplib.Sexp.t
(** [sexp_of_conf conf]
    converts a config instance to an s-expression *)

val conf_of_sexp : Sexplib.Sexp.t -> t
(** [conf_of_sexp sexp]
    converts an s-expression to a config instance *)

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Tony Wuersch

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
