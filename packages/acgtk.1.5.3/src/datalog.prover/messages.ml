
(* This file was auto-generated based on "dl_parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 2 ->
        "An integer is expected.\n"
    | 11 ->
        "A \"(\" is expected.\n"
    | 1 ->
        "A \"/\" or a \"(\" is expected.\n"
    | 18 ->
        "A \",\" or a \")\" is expected.\n"
    | 16 ->
        "A \".\" or a \",\" is expected.\n"
    | 42 | 25 | 19 | 12 ->
        "A variable or a constant (natural number) is expected.\n"
    | 32 ->
        "A \".\" or a \",\" is expected.\n"
    | 41 | 24 ->
        "A \"(\" is expected.\n"
    | 44 ->
        "A \"?\" is expected.\n"
    | 48 | 28 ->
        "A \".\" or a \":-\" is expected.\n"
    | 49 | 46 | 39 | 22 | 33 | 29 | 21 | 0 | 9 ->
        "A predicate symbol is expected.\n"
    | _ ->
        raise Not_found
