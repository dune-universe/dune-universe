(* lexer for rfc2252 format schemas

   Copyright (C) 2004 Eric Stokes, and The California State University
   at Northridge

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)


{
  type token =
      Lparen
    | Rparen
    | Numericoid of string
    | Name of string list
    | Desc of string
    | Obsolete
    | Equality of string
    | Ordering of string
    | Substr of string
    | Syntax of string * Int64.t
    | Single_value
    | Collective
    | No_user_modification
    | Usage of string
    | Sup of string list
    | Abstract
    | Structural
    | Auxiliary
    | Must of string list
    | May of string list
    | Xstring of string

  let quote = Str.regexp "'"
  let spacerex = Str.regexp "  *"
  let stripspace buf = Str.global_replace spacerex "" buf
  let extract buf i chop = String.sub buf i ((String.length buf) - i - chop);;
  let splitoidlst buf regex = Str.split regex buf;;
  let stripquote buf = Str.global_replace quote "" buf
  let stripquotes lst = List.map (fun item -> stripquote item) lst
}

(* conversion definitions, from rfc 2252. I've tried to keep the names
   the same, or close. I've changed some names to make them more
   descriptive *)
let alpha  = [ 'a' - 'z' 'A' - 'Z' ]
let digit  = [ '0' - '9' ]
let hdigit = [ 'a' - 'f' 'A' - 'F' '0' - '9' ]
let k = [ 'a' - 'z' 'A' - 'Z' '0' - '9' '-' ';' ]
let p = [ 'a' - 'z' 'A' - 'Z' '0' - '9' '"' '(' ')' '+' ',' '-' '.' '/' ':' '?' ' ' ]
let utf8 = [ '\t' ' ' '!' - '&' '(' - '~' ] (* for now, this works, need to read about this *)
let xstring = [ 'A' - 'Z' '-' ';' '_' ] +
let whsp = ' ' +
let dstring = utf8 *
let qdstring = (whsp)? '\'' (dstring as qdstringval) '\'' (whsp)?
let qdstringlist = qdstring +
let qdstrings = qdstring | ( (whsp)? '(' qdstringlist ')' (whsp)? )
let letterstring = alpha +
let numericstring = digit +
let anhstring = k +
let keystring = alpha anhstring *
let printablestring = p +
let space = ' ' +
let descr = keystring
let qdescr = whsp ''' (descr as qdescrval) ''' whsp
let qdescrlist = qdescr ( ''' descr ''' whsp ) *
let numericoid = numericstring ( '.' numericstring ) *
let oid = descr | numericoid
let woid = ( whsp )? oid ( whsp )?
let oidlist = ( woid ( '$' woid ) * ) as oidlst
let oids = woid as oidlst | whsp '(' ( oidlist as oidlst ) ')' whsp

(* violates rfc2252 to support Microsoft Active Directory, but at least is not ambigous *)
let noidlen = whsp ( ( numericoid ( '{' numericstring '}' ) ? ) as oid )
              | whsp ''' ( ( numericoid ( '{' numericstring '}' ) ? ) as oid ) '''
              | whsp ''' ( keystring as oid ) '''

let attributeUsage = "userApplication" | "directoryOperation" | "distributedOperation" | "dSAOperation"

rule lexattr = parse
    '(' whsp {Lparen}
  | "NAME" qdescr {Name [qdescrval]}
  | "NAME" whsp '(' (qdescrlist as namelst) ')' whsp {Name (stripquotes
                                                 (splitoidlst
                                                    namelst
                                                    (Str.regexp "  *")))}
  | "DESC" qdstring {Desc qdstringval}
  | "OBSOLETE" whsp {Obsolete}
  | "SUP" whsp (woid as sup) {Sup [(stripspace sup)]}
  | "EQUALITY" whsp (woid as equality) {Equality (stripspace equality)}
  | "ORDERING" whsp (woid as ord) {Ordering (stripspace ord)}
  | "SUBSTR" whsp (woid as substr) {Substr (stripspace substr)}
  | "SYNTAX" noidlen whsp {match (splitoidlst oid (Str.regexp "{")) with
                               [syntax]        -> Syntax (syntax, Int64.zero)
                             | [syntax;length] -> Syntax (syntax,
                                                          Int64.of_string
                                                            (extract length 0 1))
                             | _               -> failwith "syntax error"}
  | "SINGLE-VALUE" whsp {Single_value}
  | "COLLECTIVE" whsp {Collective}
  | "NO-USER-MODIFICATION" whsp {No_user_modification}
  | "USAGE" whsp attributeUsage whsp {Usage (extract (Lexing.lexeme lexbuf) 6 1)}
  | "X-" xstring qdstrings {Xstring (Lexing.lexeme lexbuf)}
  | oid whsp {Numericoid (extract (Lexing.lexeme lexbuf) 0 1)}
  | ')' {Rparen}

and lexoc = parse
    '(' whsp {Lparen}
  | "NAME" qdescr {Name [qdescrval]}
  | "NAME" whsp '(' (qdescrlist as namelst) ')' whsp {Name (stripquotes
                                                              (splitoidlst
                                                                 namelst
                                                                 (Str.regexp "  *")))}
  | "DESC" qdstring {Desc qdstringval}
  | "OBSOLETE" whsp {Obsolete}
  | "SUP" whsp (woid as sup) {Sup [(stripspace sup)]}
  | "SUP" whsp '(' oidlist ')' whsp {Sup (List.rev_map stripspace
                                            (splitoidlst oidlst
                                               (Str.regexp " *\\$ *")))}
  | "ABSTRACT" whsp {Abstract}
  | "STRUCTURAL" whsp {Structural}
  | "AUXILIARY" whsp {Auxiliary}
  | "MUST" whsp (woid as must) {Must [(stripspace must)]}
  | "MUST" whsp '(' oidlist ')' whsp {Must (List.rev_map stripspace
                                              (splitoidlst oidlst
                                                 (Str.regexp " *\\$ *")))}
  | "MAY" whsp (woid as may) {May [(stripspace may)]}
  | "MAY" whsp '(' oidlist ')' whsp {May (List.rev_map stripspace
                                            (splitoidlst oidlst
                                               (Str.regexp " *\\$ *")))}
  | "X-" xstring qdstrings {Xstring (Lexing.lexeme lexbuf)}
  | oid whsp {Numericoid (extract (Lexing.lexeme lexbuf) 0 1)}
  | ')' {Rparen}
