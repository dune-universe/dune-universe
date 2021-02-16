/**************************************************************************/
/*                                                                        */
/*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       */
/*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           */
/*                                                                        */
/*  This software is free software; you can redistribute it and/or        */
/*  modify it under the terms of the GNU Library General Public           */
/*  License version 2.1, with the special exception on linking            */
/*  described in file LICENSE.                                            */
/*                                                                        */
/*  This software is distributed in the hope that it will be useful,      */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  */
/*                                                                        */
/**************************************************************************/

/* File parser.mly */
%{
  open Fonts_type

let enc = ref None
let slant = ref None
let extend = ref None
let pfab = ref None

let compose tex human =
  match !pfab with
    |None -> (Parsing.parse_error "No pfab for this font ttf");None
    |Some v_pfab ->
       let font = {tex_name = tex;
                   human_name = human;
                   enc_name = !enc;
                   pfab_name = v_pfab;
                   slant = !slant;
                   extend = !extend
                  } in
  (slant := None;
  extend := None;
  enc := None;
  pfab := None;Some font)

let add_some l = function
  | None -> l
  | Some a -> a::l


%}
%token <float> FLOAT
%token <string> ID IDENC IDPFAB IDUnknown
%token EOL EOF
%token REMAP SLANT EXTEND
%token DQUOTE LESS
%token DEFAULT NONE
%token REENCODEFONT
%type <Fonts_type.font_map list> pdftex_main
%start pdftex_main
%%
/*dvipdfm_main pr:
    dvipdfm_line EOL dvipdfm_main        { pr $1 }
    dvipdfm_line EOF {[$1]}
;

dvipdfm_line:
  ID ID ID ID
;*/


pdftex_main :
  | pdftex_line EOL pdftex_main {add_some $3 $1}
  | pdftex_line EOF {add_some [] $1}
  | EOL pdftex_main {$2}
  | EOF              {[]}
;

pdftex_line:
  | ID ID pdftex_options {compose $1 $2}
  | ID pdftex_options {compose $1 $1}

pdftex_options:
  | {}
  | DQUOTE pdftex_options_aux DQUOTE pdftex_options {$2}
  | IDENC pdftex_options                {enc:=Some $1}
  | IDPFAB pdftex_options               {pfab:=Some $1}
  | IDUnknown pdftex_options            {pfab:=None}

pdftex_options_aux:
  |                    {}
  | FLOAT SLANT pdftex_options_aux {slant:=Some $1}
  | FLOAT EXTEND pdftex_options_aux {extend:=Some $1}
  | ID REENCODEFONT pdftex_options_aux {}
