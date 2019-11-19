
(* This file was auto-generated based on "command_parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "A command is expected.\n"
    | 13 ->
        "An identifier (the name of a signature or of a lexicon) or the command \"help\" are expected.\n"
    | 42 ->
        "A command is expected after an identifier.\n"
    | 45 ->
        "The commands \"save\", \"check\", \"realize\" (and the deprecated \"analyse\" and \"realize_show\") are expected.\n"
    | 53 ->
        "The command \"trace\" or \"wait\" is expected.\n"
    | 62 ->
        "An identifier (the name of a signature) or the command \"help\" are expected.\n"
    | 73 | 72 | 67 | 66 | 65 ->
        "An identifier (the name of a lexicon) or the command \"help\" are expected.\n"
    | 74 ->
        "The keyword \"as\" is expected.\n"
    | 75 ->
        "An identifier (the name of a new lexicon) is expected.\n"
    | 58 | 54 | 47 | 94 | 38 | 9 | 1 | 5 ->
        "A semicolon \":\" or the keyword \"help is expected.\n"
    | 86 | 83 | 80 | 70 | 76 | 68 | 63 | 51 | 90 | 35 | 108 | 30 | 27 | 24 | 21 | 18 | 14 | 3 | 7 | 11 | 16 | 40 | 49 | 56 | 60 | 78 | 92 | 96 ->
        "A semicolon \":\" is expected\n"
    | _ ->
        raise Not_found
