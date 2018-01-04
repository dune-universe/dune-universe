open Core

(*
   module RFC2822 = struct
   let unfold str =
   let lexbuf = Lexing.from_string str in
   let bigbuffer = Bigbuffer.create (String.length str) in
   Lexer.field_unstructured_unfold bigbuffer lexbuf;
   Bigbuffer.contents bigbuffer
   ;;

   let fold str =
   let lexbuf = Lexing.from_string str in
   let bigbuffer = Bigbuffer.create (String.length str) in
   Lexer.field_unstructured_fold bigbuffer lexbuf;
   Bigbuffer.contents bigbuffer
   ;;

   TEST_MODULE "Folding_and_unfolding" = struct
   TEST = (fold "a\n b\nc") = " a\n b\n c"
   TEST = (fold " a\n b\nc") = " a\n b\n c"
   TEST = (fold "\ta\n b\nc") = "\ta\n b\n c"
   TEST = (unfold " a\n b\nc d") = "a b c d"
   end
   end
*)

module RFC2045 = struct
  module Token = struct
    include (Mimestring.Case_insensitive : Mimestring.S)

    let is_valid str =
      not (String.is_empty str)
      && String.for_all str ~f:(function
        | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':'
        | '\\' | '"' | '/' | '[' | ']' | '?' | '=' -> false
        (* '\n', '\r', ' ' are excluded by the following: *)
        | '\033' .. '\126' -> true
        | _ -> false)

    let is_valid_or_quote str =
      if is_valid str then str
      else Mimestring.quote str
  end
end
