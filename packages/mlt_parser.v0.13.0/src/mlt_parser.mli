(** Code for parsing toplevel expect test files *)

open Ppxlib
open Expect_test_common.Std
open Expect_test_matcher.Std

type chunk =
  { part        : string option (** The part the chunk is in, None if it's not in any
                                    part. *)
  ; phrases     : toplevel_phrase list
  ; expectation : Fmt.t Cst.t Expectation.t
  ; phrases_loc : Location.t
  }

(** Recursively parses toplevel phrases (i.e., contiguous units of code separated by
    [;;]) into "chunks", one chunk per [%%expect] statement.

    For example if the mlt contents are:

    {[
      let x = 1 + 1;;

      printf "%d" x + 2;;

      [%%expect {|
     - : int: 4
     |}];;

      print_string "f" ^ "o" ^ "o";;

      [%%expect {|
     - : string: "foo"
     |}];;

      print_string 3 + 3 + 3;;
    ]}

    then you'd have two chunks, where the first has two phrases (["x = 1 + 1"] and
    ["printf "%d" x + 2"]) and an [expectation.body] of [": int 4"]. The second chunk
    would have just the one phrase.

    ["print_line 3 + 3 + 3"] is not part of a chunk because there is no expectation
    following it, so instead it is returned as [trailing_code], which is just a list of
    toplevel phrases with some position metadata.

    "part" refers to [@@@part "foo"] statements, which are arbitrary section breaks. Each
    chunk, and the trailing code, belongs to a part (which is just the empty string [""]
    if none has been specified).
*)
val split_chunks
  :  fname:string
  -> allow_output_patterns:bool
  -> toplevel_phrase list
  -> chunk list * (toplevel_phrase list * position * string option) option

type mlt_block =
  | Org of string
  | Expect of string
  | Code of string
[@@deriving sexp]
;;
(** Takes a list of toplevel phrases and the raw string they're embedded in and returns a
    list of labeled blocks, so that for instance the following raw toplevel code:

    {[
      [%%org {|
        Here comes a very /simple/ example.
      |}];;

      1 + 1;;
      [%%expect {|
      - : int: 2
      |}];;
    ]}

    is parsed into its constituent parts:

    {[
      [
        (Org "Here comes a very /simple/ example.");
        (Code "1 + 1");
        (Expect "- : int: 2")
      ]
    ]}

    Note that we only care about these three kinds of element (org blocks, expect blocks,
    and regular OCaml code blocks); everything else -- including toplevel comments -- is
    silently discarded.
*)
val parse
  :  toplevel_phrase list
  -> contents:string
  -> mlt_block list
