(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2018 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

(** Contains all error messages and error handling stuff. *)

open Containers
open Fmtc

(** An abbreviation. *)
module Location = Location

(** {1 Setting up the logging machinery} *)

module LoggingSetup = struct

  (* Logging "source" for the electrod {b library}. *)
  let electrod_src = Logs.Src.create "electrod.onera.fr" ~doc:"logs electrod events"

  (** Creates the logging "source" for the electrod {b library} (not the 
      whole application). Defines functions: debug, info, err, warn 
      {[  let univ_duplicate_atoms args =
            info @@ fun m -> args @@ fun written actual ->
            m "univ contains duplicate atoms: %a\n\
               ignoring them and continuing with this: %a"
              (Atom.pp_list) written
              (Atom.pp_list) actual]}
  *)

  module M = (val Logs.src_log electrod_src : Logs.LOG)

  include M

end

let debug = LoggingSetup.debug
let info = LoggingSetup.info
let warn = LoggingSetup.warn
let err m = LoggingSetup.kmsg (fun () -> raise Exit) Logs.Error m


let style =
  let open! Logs in
  let open Logs_fmt in
  function
    | App -> app_style
    | Error -> err_style
    | Warning -> warn_style
    | Info -> info_style
    | Debug -> debug_style


(** {1 Module to get source code fragments and show them (in error messages for
    instance), possibly with colored output} *)
module Extract = struct

  let lines file loc = match file with
    | None -> []
    | Some f ->
        IO.(with_in f @@ fun ic ->
            IO.read_lines ic
            |> Gen.drop (Int.max 0 Location.(begl loc - 1))
            |> Gen.take (Int.max 1 Location.(1 + endl loc - begl loc))
            |> Gen.to_list)

  (* splits the string into 2 parts, the first containing [nb] characters *)
  let split_string s nb =
    let open String in
    let lg = length s in
    (* debug (fun m -> m "%s (%d) / %d" s lg nb); *)
    assert (nb >= 0 && nb <= lg);
    (* debug (fun m -> m "sub %d %d" 0 nb); *)
    let first = sub s 0 nb in
    (* debug (fun m -> m "sub %d %d" nb (lg-nb)); *)
    let last = sub s nb (lg - nb) in
    (* debug (fun m -> m "%s°%s" first last); *)
    (first, last)

  let pp =
    (pair ~sep:nop string
    @@ pair ~sep:nop
         (styled `Bold @@ styled `Magenta
          @@ list ~sep:Format.pp_force_newline string)
         string) 
  
  let extract file loc = 
    let lines = lines file loc in
    let innocent_first_last_idx = Int.max 0 (Location.begc loc) in
    let suspect_last_last_idx = Int.max 0 (Location.endc loc) in
    match lines with
      | [] -> ("", ([], ""))
      | [line] ->
          (* trick : cut first the last part *)
          let first_part, innocent_last =
            split_string line suspect_last_last_idx in
          let innocent_first, suspect =
            split_string first_part innocent_first_last_idx in
          (innocent_first, ([suspect], innocent_last))
          
      | first::others ->
          let innocent_first, suspect_first =
            split_string first innocent_first_last_idx in
          let suspect_middle, last =
            List.take_drop (Int.max 0 Location.(endl loc - begl loc - 1)) others in
          let last = List.hd last in
          let suspect_last, innocent_last =
            split_string last suspect_last_last_idx in
          let suspect = suspect_first :: suspect_middle @ [suspect_last] in
          (innocent_first, (suspect, innocent_last)) (* pair of pairs! *)

end


(** {1 Messages (errors, warnings...)}  *)

(** [code num] takes a message number and prints is as an error code (4
    (zero-padded) digits). DO NOT EVER --EVER!-- CHANGE THE NUMBER OF A MESSAGE
    (AT WORST, DELETE THE MESSAGE AND LET THE NUMBER BE LOST). ONLY ADD MESSAGES
    AT THE {b END} OF THE FOLLOWING MESSAGING MODULES. *)
let code num =
  Printf.sprintf "%03d" num




(** {2 Errors that stop the program} *)
    
module Fatal = struct

  let lexical args = err @@ fun m -> args @@
    fun infile lexbuf msg ->
    let loc = Location.from_positions
                (Lexing.lexeme_start_p lexbuf)
                (Lexing.lexeme_end_p lexbuf) in
    m ~header:(code 1)
      "%a%a: lexical error: %s"
      (option @@ colon **> string) infile
      Location.pp loc
      msg

  let syntax args = err @@ fun m -> args @@
    fun file lexbuf ->
    let loc = Location.from_positions
                (Lexing.lexeme_start_p lexbuf)
                (Lexing.lexeme_end_p lexbuf) in
    m ~header:(code 2)
      "%s:%a: syntax error %a%a"
      file
      Location.pp loc
      (* (print_extract ~color:error_color) (file, loc); *)
      string (Lexing.lexeme lexbuf)
      (hardline **< Extract.pp) (Extract.extract (Some file) loc)
      

  
  let wrong_suffix args = err @@ fun m -> args @@
    fun infile id ->
    let name, loc = Raw_ident.basename id, Raw_ident.location id in
    m ~header:(code 3)
      "%a%a: syntax error: %S is not a valid suffix \
       for indexed identifiers%a"
      (option @@ colon **> string) infile
      Location.pp loc
      name
      (hardline **< Extract.pp) (Extract.extract infile loc)


  let different_prefixes args = err @@ fun m -> args @@
    fun infile first last ->
    let first_loc = Raw_ident.location first in
    let last_loc = Raw_ident.location last in
    m ~header:(code 4)
      "%a%S (%a) and %S (%a) have different_prefixes%a"
      (option @@ sp **> colon **> string) infile
      (Raw_ident.basename first)
      Location.pp first_loc
      (Raw_ident.basename last)
      Location.pp last_loc
      (hardline **<
       Extract.pp) (Extract.extract infile (Location.span (first_loc, last_loc)))


  let not_an_interval args = err @@ fun m -> args @@
    fun infile first last ->
    let first_loc = Raw_ident.location first in
    let last_loc = Raw_ident.location last in
    m ~header:(code 5)
      "%a%S (%a) and %S (%a) do not define a valid (increasing) range of atoms%a"
      (option @@ sp **> colon **> string) infile
      (Raw_ident.basename first)
      Location.pp first_loc
      (Raw_ident.basename last)
      Location.pp last_loc
      (hardline **<
       Extract.pp) (Extract.extract infile (Location.span (first_loc, last_loc)))

  let rel_name_already_used args = err @@ fun m -> args @@
    fun infile id ->
    let loc = Raw_ident.location id in
    m ~header:(code 6)
      "%a%a: identifier %S already used%a"
      (option @@ colon **> string) infile
      Location.pp loc
      (Raw_ident.basename id)
      (hardline **< Extract.pp) (Extract.extract infile loc)
      
  let undeclared_id args = err @@ fun m -> args @@
    fun infile id ->
    let loc = Raw_ident.location id in
    m ~header:(code 7)
      "%a%a: identifier %S unknown%a"
      (option @@ colon **> string) infile
      Location.pp loc
      (Raw_ident.basename id)
      (hardline **< Extract.pp) (Extract.extract infile loc)

  let should_denote_a_constant_set args = err @@ fun m -> args @@
    fun infile id ->
    let loc = Raw_ident.location id in
    m ~header:(code 8)
      "%a%a: %S does not denote a constant set%a"
      (option @@ colon **> string) infile
      Location.pp loc
      (Raw_ident.basename id)
      (hardline **< Extract.pp) (Extract.extract infile loc)

  let incompatible_arities args = err @@ fun m -> args @@
    fun infile id ->
    let loc = Raw_ident.location id in
    m ~header:(code 9)
      "%a%a: inconsistent arities used in some tuples for %S%a"
      (option @@ colon **> string) infile
      Location.pp loc
      (Raw_ident.basename id)
      (hardline **< Extract.pp) (Extract.extract infile loc)

  let undeclared_atoms args = err @@ fun m -> args @@
    fun infile loc absent ->
    m ~header:(code 10)
      "%a%a: atom(s) not declared in 'univ': %a%a"
      (option @@ colon **> string) infile
      Location.pp loc 
      (vbox2 @@ list ~sep:sp Atom.pp) absent
      (hardline **< Extract.pp) (Extract.extract infile loc)

  let inf_not_in_sup args = err @@ fun m -> args @@
    fun infile id pp_bound inf sup ->
    let loc = Raw_ident.location id in
    m ~header:(code 11)
      "%a%a: lower bound of %S is not included in upper bound@\n%a@\n\
      lower bound = %a@\nupper bound = %a"
      (option @@ colon **> string) infile
      Location.pp loc
      (Raw_ident.basename id)
      (Extract.pp) (Extract.extract infile loc)
      (box2 @@ pp_bound) inf
      (box2 @@ pp_bound) sup

  let inexact_ref_used_in_exact_scope args = err @@ fun m -> args @@
    fun infile id ref_id ->
    let loc = Raw_ident.location id in
    m ~header:(code 12)
      "%a%a: %S is declared as exact but \
       refers to relation %S which has inexact bounds%a"
      (option @@ colon **> string) infile
      Location.pp loc
      (Raw_ident.basename id)
      (Raw_ident.basename ref_id)
      (hardline **< Extract.pp) (Extract.extract infile loc)
      
  let arity_error args = err @@ fun m -> args @@
    fun infile exp msg ->
    let loc = exp.GenGoal.exp_loc in
    m ~header:(code 13)
      "%a%a: %s%a"
      (option @@ colon **> string) infile
      Location.pp loc
      msg
      (hardline **< Extract.pp) (Extract.extract infile loc)
      
  let init_and_fby_incompatible_arities args = err @@ fun m -> args @@
    fun infile id init fby ->
    let loc = Raw_ident.location id in
    m ~header:(code 14)
      "%a%a: inconsistent arities used in the 'init' (%d) and \
       'then' (%d) parts for %S%a"
      (option @@ colon **> string) infile
      Location.pp loc
      init
      fby
      (Raw_ident.basename id)
      (hardline **< Extract.pp) (Extract.extract infile loc)
     
  let cannot_decide_arity args = err @@ fun m -> args @@
    fun infile id ->
    let loc = Raw_ident.location id in
    m ~header:(code 15)
      "%a%a: the arity of %S cannot be inferred, specify it explicitly%a"
      (option @@ colon **> string) infile
      Location.pp loc
      (Raw_ident.basename id)
      (hardline **< Extract.pp) (Extract.extract infile loc)
      
  let specified_computed_arities_discrepancy args = err @@ fun m -> args @@
    fun infile id specified_arity computed_arity ->
    let loc = Raw_ident.location id in
    m ~header:(code 16)
      "%a%a: discrepancy between the specified (%d) and \
       inferred (%d) arities for %S%a"
      (option @@ colon **> string) infile
      Location.pp loc
      specified_arity
      computed_arity
      (Raw_ident.basename id)
      (hardline **< Extract.pp) (Extract.extract infile loc)
      
  let instance_is_var args = err @@ fun m -> args @@
    fun infile id ->
    let loc = Raw_ident.location id in
    m ~header:(code 17)
      "%a%a: %S refers to a variable relation, its value cannot be fixed in \
       an instance%a"
      (option @@ colon **> string) infile
      Location.pp loc
      (Raw_ident.basename id)
      (hardline **< Extract.pp) (Extract.extract infile loc)
      
  let instance_not_in_scope args = err @@ fun m -> args @@
    fun infile id ->
    let loc = Raw_ident.location id in
    m ~header:(code 18)
      "%a%a: the value given for %S in the instance does not comply with \
       the declared scope%a"
      (option @@ colon **> string) infile
      Location.pp loc
      (Raw_ident.basename id)
      (hardline **< Extract.pp) (Extract.extract infile loc)
      
  let instance_already_declared args = err @@ fun m -> args @@
    fun infile id ->
    let loc = Raw_ident.location id in
    m ~header:(code 19)
      "%a%a: instance %S already declared%a"
      (option @@ colon **> string) infile
      Location.pp loc
      (Raw_ident.basename id)
      (hardline **< Extract.pp) (Extract.extract infile loc)

  let syntax_error_paragraphs args = err @@ fun m -> args @@
    fun infile msg ->
    m ~header:(code 20)
      "%a%s"
      (option @@ sp **> colon **> string) infile
      msg

  let symmetry_wrongly_defined args = err @@ fun m -> args @@
    fun infile id ->
    let loc = Raw_ident.location id in
    m ~header:(code 21)
      "%a%a: left-hand-side and right-hand-side of symmetry do
       not have the same length%a"
      (option @@ colon **> string) infile
      Location.pp loc
      (hardline **< Extract.pp) (Extract.extract infile loc)

  let solver_failed args = err @@ fun m -> args @@
    fun solver scr smv errcode erroutput ->
    m ~header:(code 22)
      "%s@ failed@ with@ error@ code@ %d@ and@ error@ output:@\n%s@\n\
       Script@ and@ model@ files@ kept@ at@ %S@ and@ %S,\
       @ remember@ to@ remove@ them."
      solver errcode erroutput
      scr smv

  let solver_bug args = err @@ fun m -> args @@
    fun solver msg ->
    m ~header:(code 23)
      "bug in %s: %s" solver msg

  let no_multiplicity_allowed_here args = err @@ fun m -> args @@
    fun infile id ->
    let loc = Raw_ident.location id in
    m ~header:(code 24)
      "%a%a: %S: only one toplevel, 'lone' or 'one', multiplicity is allowed\ 
       for relations"
      (option @@ colon **> string) infile
      Location.pp loc
      (Raw_ident.basename id)

  let multiplicity_only_in_a_sup args = err @@ fun m -> args @@
    fun infile id ->
    let loc = Raw_ident.location id in
    m ~header:(code 25)
      "%a%a: %S: a multiplicity is only allowed in the upper bound of an \
       inexact scope"
      (option @@ colon **> string) infile
      Location.pp loc
      (Raw_ident.basename id)

  let inf_must_be_empty args = err @@ fun m -> args @@
    fun infile id ->
    let loc = Raw_ident.location id in
    m ~header:(code 26)
      "%a%a: the lower bound of %S must be empty as it is enumerable"
      (option @@ colon **> string) infile
      Location.pp loc
      (Raw_ident.basename id)
end

  
(** {2 Warnings (the program does not fail)} *)

module Warn = struct

  let univ_duplicate_atoms args = warn @@ fun m -> args @@
    fun __infile written actual ->
    let fmt = Fmtc.box2 @@ Atom.pp_list in
    m ~header:(code 1)
      "univ contains duplicate atoms...:@ %a\
       @;...ignoring them and continuing with:@ %a"
      fmt written
      fmt actual

  let empty_scope_declared args = warn @@ fun m -> args @@
    fun infile id ->
    let loc = Raw_ident.location id in
    m ~header:(code 2)
      "%a%a: %s is always empty@\n%a" 
      (option @@ colon **> string) infile
      Location.pp loc
      (Raw_ident.basename id)
      Extract.pp (Extract.extract infile loc)

  let duplicate_elements args = warn @@ fun m -> args @@
    fun infile id bound_kind pp_bound bound ->
    let loc = Raw_ident.location id in
    let pp_inf_sup (which : [ `Inf | `Sup | `Exact]) = match which with
      | `Exact -> ""
      | `Inf -> " lower" 
      | `Sup -> " upper" 
    in
    m ~header:(code 3)
      "%a%a: the%s bound of %s contains duplicate elements...:@ %a\
       @\n...ignoring them and continuing with:@ %a"
      (option @@ colon **> string) infile
      Location.pp loc
      (pp_inf_sup bound_kind)
      (Raw_ident.basename id)
      Extract.pp (Extract.extract infile loc)
      (box2 @@ pp_bound) bound

  let disj_with_only_one_variable args = warn @@ fun m -> args @@
    fun infile id ->
    let loc = Raw_ident.location id in
    m ~header:(code 4)
      "%a%a: keyword \"disj\" ignored as %S is a single variable@\n%a" 
      (option @@ colon **> string) infile
      Location.pp loc
      (Raw_ident.basename id)
      Extract.pp (Extract.extract infile loc)

  let met_spurious_variable args = warn @@ fun m -> args @@
    fun var ->
    m ~header:(code 5)
      "Spurious variable(s) in the SMV trace: %S"
      var
end

