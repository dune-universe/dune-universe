(** {1 Error report} *)

type 'a state
(** Type state *)

type error
(** Type error *)

val state : 'a option -> int -> int -> string -> 'a state
(** Create a new parser state *)

val result_of_state : 'a state -> ('a, error) result
(** Get the result *)

val state_value : 'a state -> 'a option
(** Get the parsed value *)

val state_offset : 'a state -> int
(** Get the current position of the parser inside the string *)

val state_line : 'a state -> int
(** Get the current line position of the parser inside the string *)

val state_rest : 'a state -> string
(** Get the remaining characters of the string beeing parsed *)

val report : error -> unit
(** Report an error *)

(** {1 Defining parsers} *)

type 'a parser
(** Type parser *)

val ( ~~ ) : (string -> 'a state) -> 'a parser
(** Parser constructor *)

val pure : 'a -> 'a parser
(** Pure parser consuming no character and returning a value of type ['a]. *)

val many : 'a parser -> 'a list parser
(** Parse zero or more times a given pattern *)

val some : 'a parser -> 'a list parser
(** Parse one or more times a given pattern *)

val check : (char -> bool) -> char parser
(** Test a predicate on the first character of the input. Resolve to this
    character if the predicate is verified *)

(** {2 Combinators and Infix operators} *)

val ( <*> ) : ('a -> 'b) parser -> 'a parser -> 'b parser
(** Apply *)

val ( <$> ) : ('a -> 'b) -> 'a parser -> 'b parser
(** Map *)

val ( *> ) : 'a parser -> 'b parser -> 'b parser
(** Apply to the right *)

val ( <* ) : 'a parser -> 'b parser -> 'a parser
(** Apply to the left *)

val ( <|> ) : 'a parser -> 'a parser -> 'a parser
(** Alternative *)

(** {2 Let notation} *)

val ( let* ) : 'a parser -> ('a -> 'b parser) -> 'b parser
(** Binding operator using the [let*] operator. This syntax is similar to
    Haskel's "do notation" :

    {[
      let tuple_parser =
        let* f1 = floatingpoint in
        let* _ = char ',' in
        let* f2 = floatingpoint in
        pure (f1, f2)
    ]}

    Even if this operator can be usefull in extreme cases, users are encouraged
    to use the {!(<*>)} operator instead :

    {[
      let tuple_parser =
        (fun x y -> (x, y)) <$> floatingpoint <*> char ',' *> floatingpoint
    ]} *)

(** {2 Powerfull Combinators} *)

(** The following operators are usefull to parse patterns which are left
    recursive by nature. The typical exemple is arithmetic expressions. *)

val chainl : ('a -> 'a -> 'a) parser -> 'a parser -> 'a parser
(** Eliminate left recursion in "left associative" chains. [chainl op term] is a
    parser recognizing chains of terms [term] bound by the operator [op]. The
    operator is considered left associative. *)

val chainr : 'a parser -> ('a -> 'a -> 'a) parser -> 'a parser
(** Eliminate left recursion in "right associative" chains. [chainr term op] is
    a parser recognizing chains of terms [term] bound by the operator [op]. The
    operator is considered right associative. *)

(** {2 Utils} *)

val do_parse : 'a parser -> string -> ('a, error) result
(** Run a parser against a string *)

val ( --> ) : string -> 'a parser -> 'a state
(** Feed a parser with a string (from left to right) This is verry different
    from [do_parse] ! No verifications are made on the remaining chars. *)

val ( <-- ) : 'a parser -> string -> 'a state
(** Feed a parser with a string (from right to left) [p <-- input] is
    [input --> p]. This function is just for convenience. *)

val do_parse_from_file : 'a parser -> string -> ('a, error) result
(** Run a parser against a file *)
