(* Main entry point *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

open Format
open Sym
open Formula
open Form_printer
open Quant_printer
open Fact_printer
open Sexpr_printer
open Solve
open Main

(* This function specifies what chase does after the command line
   arguments have been parsed and the input has been read. *)

(* This mysterious code ensures line breaks occur only at break hints.
   A bug report has been filed about the lack of documentation on the
   need for changing the maximum indent. *)
let carefully_set_margin f n =
  pp_set_margin f n;
  pp_set_max_indent f (n - 1)

let run opts o xs =
  let ctx = mk_ctx () in  (* Make a printing context for symbols *)
  let f = formatter_of_out_channel o in (* Grab formatter *)
  if opts.Main.margin > 0 then
    pp_set_margin f opts.Main.margin; (* Set margin *)
  let pp =
    if opts.explicit then       (* Select formula pretty printer *)
      print_quant ctx f
    else
      print_form ctx f in
  let comment_char =
    if opts.sexpr then          (* Select comment character *)
      ';'
    else
      '%' in

  (* Pretty print within comments *)
  let cmt_len = 2 in          (* Number of chars added with comment *)
  carefully_set_margin f (pp_get_margin f () - cmt_len);
  let out_str = Printf.sprintf "\n%c " comment_char in
  let funs = pp_get_formatter_out_functions f () in
  let new_funs = {
      funs with
      out_newline = fun () -> funs.out_string out_str 0 3} in
  pp_set_formatter_out_functions f new_funs;

  (* Print header *)
  pp_print_char f comment_char;
  pp_print_string f " chase version ";
  pp_print_string f Version.version;
  pp_print_newline f ();
  pp_print_string f "bound = ";
  pp_print_int f opts.bound;
  pp_print_string f ", limit = ";
  pp_print_int f opts.limit;
  pp_print_newline f ();

  (* Print formulas *)
  pp_print_string f "********";
  pp_print_newline f ();
  List.iter pp xs;
  if opts.flat then
    begin
      pp_print_string f "********";
      pp_print_newline f ()
    end;
  let xs = List.map Flatten.flatten xs in
  if opts.flat then
    List.iter pp xs;
  pp_print_string f "********";

  (* Restore original formatter and margin *)
  pp_set_formatter_out_functions f funs;
  pp_print_newline f ();
  carefully_set_margin f (pp_get_margin f () + cmt_len);

  let pr_fr =
    if opts.sexpr then          (* Select frame printer *)
      if opts.compact then
        print_sexpr_unflattened ctx f
      else
        print_sexpr_frame ctx f
    else if opts.compact then
      print_unflattened ctx f
    else
      print_frame ctx f in
  let vpr fr =                  (* Verbose printer *)
    pp_print_newline f ();
    pr_fr fr in
  let tpr fr =                  (* Terse printer *)
    if fr.status = Sat then     (* Print only models *)
      begin
        pp_print_newline f ();
        pr_fr fr
      end in
  let pr = if opts.terse then tpr else vpr in

  (* Run main loop *)
  solve opts.just_one opts.bound opts.limit pr (mk_axioms xs)

let () = main run
