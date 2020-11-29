(* Copyright (C) 2020 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

(* Create a lexbuf that reads code from Markdown fenced code blocks

   A fenced code block is surrounded by the string "```" at the
   beginning of a line of text.

   The implementation assumes that it is an error for code within a
   block to start with "`" or "``", so that returning "`" when the
   code input started with "``" is not distinguishable.

 *)

open Lexing

(* Input a character without using exceptions *)
let input_char_opt =
  let buf = Bytes.create 1 in
  fun ch ->
  let n = input ch buf 0 1 in
  if n = 1 then
    Some (Bytes.get buf 0)
  else
    None

(* States of the reader *)
type state =
  | Strip_bol            (* Stripping at the beginning of a line *)
  | Strip                (* Stripping after the beginning of a line *)
  | Strip_flush          (* Stripping after seeing a fence *)
  | Copy_bol             (* Copying at the beginning of a line *)
  | Copy                 (* Copying after the beginning of a line *)
  | Copy_flush           (* Copying after seeing a fence *)

(* This function produces a function for use in lexing.  The function
   receives a byte array of characters and the number of requested
   characters.  It fills the array up to the requested number of
   characters, and returns the number of characters added to the
   array. *)
let markdown ch =
  let state = ref Strip_bol in
  (* The array is buf; n is the requested size, i is the current fill. *)
  let rec loop buf n i =
    if i >= n then
      i                         (* Array is full *)
    else
      match input_char_opt ch with (* Read a character *)
      | None -> i
      | Some c ->
         match !state with      (* Dispatch on the state *)
         | Strip_bol ->     (* Look for fence while stripping input *)
            if c = '`' then
              match input_char_opt ch with
              | None -> i
              | Some c ->
                 if c = '`' then
                   match input_char_opt ch with
                   | None -> i
                   | Some c ->
                      if c = '`' then (* Found fence; change mode *)
                        begin
                          state := Strip_flush;
                          loop buf n i
                        end
                      else
                        strip buf n i c
                 else
                   strip buf n i c
            else
              strip buf n i c
         | Strip ->             (* Strip in strip mode *)
            strip buf n i c
         | Strip_flush ->       (* Strip while going to copy mode *)
            if c = '\n' then
              begin
                Bytes.set buf i c;
                state := Copy_bol;
                loop buf n (i + 1)
              end
            else
              loop buf n i
         | Copy_bol ->
            if c = '`' then (* Add backquote if input is not a fence *)
              match input_char_opt ch with
              | None ->
                 copy_eof buf n i
              | Some c ->
                 if c = '`' then
                   match input_char_opt ch with
                   | None ->
                      copy_eof buf n i
                   | Some c ->
                      if c = '`' then
                        begin   (* Found fence; change mode *)
                          state := Copy_flush;
                          loop buf n i
                        end
                      else
                        copy_quote buf n i
                 else
                   copy_quote buf n i
            else
              copy buf n i c
         | Copy ->              (* Copy in copy mode *)
            copy buf n i c
         | Copy_flush ->        (* Copy while going to Strip mode *)
            if c = '\n' then
              begin
                Bytes.set buf i c;
                state := Strip_bol;
                loop buf n (i + 1)
              end
            else
              loop buf n i
  and strip buf n i c =
    if c = '\n' then
      begin
        Bytes.set buf i c;
        state := Strip_bol;
        loop buf n (i + 1)
      end
    else
      begin
        state := Strip;
        loop buf n i
      end
  and copy_eof buf _ i =
    begin
      Bytes.set buf i '`';
      i + 1
    end
  and copy_quote buf n i =
    begin
      Bytes.set buf i '`';
      state := Copy;
      loop buf n (i + 1)
    end
  and copy buf n i c =
    begin
      Bytes.set buf i c;
      if c = '\n' then
        state := Copy_bol
      else
        state := Copy;
      loop buf n (i + 1)
    end in
  fun buf n ->
  loop buf n 0

(* A debugger for this code
let debug_markdown ch =
  let f = markdown ch in
  fun buf n ->
  prerr_string "n = ";
  prerr_int n;
  prerr_newline ();
  let ans = f buf n in
  let rec loop i =
    if i >= ans then
      begin
        prerr_endline "---";
        prerr_string "result = ";
        prerr_int ans;
        prerr_newline ();
        prerr_endline "---";
        prerr_newline ();
        ans
      end
    else
      begin
        prerr_char (Bytes.get buf i);
        loop (i + 1)
      end in
  loop 0
 *)

(* Create a lexbuf from the markdown function *)
let read_lexbuf fname ch =
  let lexbuf = from_function (markdown ch) in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  lexbuf
