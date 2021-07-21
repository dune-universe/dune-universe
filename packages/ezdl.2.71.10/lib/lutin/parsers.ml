(*
Interface des parsers 
*)


open Lexeme;;
open LutErrors
open Parsing;;
open LutParser;;
open LutLexer;;
open Syntaxe;;
open Format;;

let lexemize_lut infile = (
  let inchannel = 
    Lexeme.set_current_file infile;
    open_in infile 
  in
  let lexbuf = Lexing.from_channel inchannel in
  let tk = ref (LutLexer.lexer lexbuf) in (
      while !tk <> TK_EOF do
	     match (token_code !tk) with
	         ( co , lxm) -> (
	           printf "file %s line %3d col %2d to %2d : %15s = \"%s\"\n"
		          lxm.file lxm.line lxm.cstart lxm.cend  co lxm.str ;
	         );
	           tk := (LutLexer.lexer lexbuf)
      done
    );
    close_in inchannel
)

let read_lut_res = Hashtbl.create 1 (* make sure this is done once *)

let reinit_parser () = Hashtbl.clear read_lut_res

let rec read_lut files =
  try Hashtbl.find read_lut_res files
  with Not_found -> 
    let res =  aux ([],Syntaxe.empty_package()) files in
      Hashtbl.add read_lut_res files res;
      res
and
    aux (handled,pack) files =
  match files with
    | [] -> pack
    | infile::rest -> 
        let infile = if Filename.is_relative infile then 
          Filename.concat (Sys.getcwd()) infile
        else
          infile
        in
          if List.mem infile handled then
            aux (handled,pack) rest
          else
            let cpack =
              if (Verbose.level ()) > 0 then (
                Printf.eprintf "Opening file %s \n" infile;
                flush stderr
              );
              let inchannel = 
                Lexeme.set_current_file infile;
                open_in infile 
              in
              let lexbuf = Lexing.from_channel inchannel in
                try LutParser.lutFileTop LutLexer.lexer lexbuf 
                with Parse_error -> (
                  print_compile_error (Lexeme.last_made ()) "syntax error";
                  exit 1
                )
            in
            let pack = union cpack pack in
              aux (infile::handled, pack) (cpack.pck_included_files_to_handle@rest)
