(** 
    Command line driver for encoding a file or stdin into Ascii85.

    @author Christian Lindig
*)

(* A pseudo random number generator to generate byte sequences to 
   test the encode *)

let step x          = x * 22695477 + 1 
let s               = ref 0     (* never used with this value *)
let int () =
  ( s := step !s
  ; ((!s lsr 16) land 16383)
  )

(** [test] emits 1024 peseudo-random chars to stdout that can be used for
    testing *)

let test () =
  let rec loop = function
    | 1024  -> ()
    | n     -> 
      ( int () mod 256 |> Char.chr |> output_char stdout
      ; loop (n+1)
      )
  in loop 0

let ps = "currentfile /ASCII85Decode filter cvx exec "
(** [ps] contains PostScript code that decodes and executes a
    PostScript program in Ascii85 encoding that immediately 
    follows. *)

let default = "<~"

let usage this =
  Printf.eprintf "usage: %s [-ps|-p prefix] [file]\n" this

(** Some code for testing and a [main] function that handles command line
    arguments *)

let main () =
  let argv        = Array.to_list Sys.argv in
  let this        = List.hd argv |> Filename.basename in
  let args        = List.tl argv in    
  match args with
  | ["-test"]     -> test ()

  | ["-ps";path]          -> Ascii85.encode_file ps path 
  | ["-ps"]               -> Ascii85.encode ps stdin stdout

  | ["-p";prefix;path]    -> Ascii85.encode_file  prefix path 
  | ["-p";prefix]         -> Ascii85.encode prefix stdin stdout

  | ["-h"]                -> usage this; exit 0

  | [path]                -> Ascii85.encode_file default path 
  | []                    -> Ascii85.encode default stdin stdout

  | _	                    -> usage this; exit 1 

let () = if not !Sys.interactive then begin main (); exit 0 end
