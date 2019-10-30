(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

open Char

let charA = code 'A'
let charZ = code 'Z'
let chara = code 'a'

(* Does string start with an uppercase letter? *)
let is_upper s =
  String.length s > 0 &&
    let i = Char.code s.[0] in
    charA <= i && i <= charZ

(* Capitalize *)
let cap s =
  if is_upper s then
    s
  else
    let b = Bytes.of_string s in
    Bytes.set b 0 (chr (code (s.[0]) + charA - chara));
    Bytes.to_string b

(* Uncapitalize *)
let uncap s =
  if is_upper s then
    let b = Bytes.of_string s in
    Bytes.set b 0 (chr (code (s.[0]) + chara - charA));
    Bytes.to_string b
  else
    s

type 'a stream = Nil | Cons of 'a * (unit -> 'a stream)
