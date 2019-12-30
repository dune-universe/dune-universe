(*
 * wseg.ml
 * -----------
 * Copyright : (c) 2019, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of wseg
 *)

open Base
open Stdio

module Tuple2 = struct
  let get1 (v, _)= v
  let get2 (_, v)= v
end

module Dict = struct
  module Tree = Trie.Make(String)

  type entry= string * float
  type entries= entry list

  let split_utf8 ?(pos=0) s=
    let len= String.length s in
    let rec to_list pos=
      if pos >= len then
        []
      else
        let next= CamomileLibrary.UTF8.next s pos in
        if next > len then
          []
        else
          String.sub s ~pos ~len:(next - pos) :: (to_list next)
    in
    to_list pos

  let buildEntries rawEntries=
    let quantity= List.fold
      ~f:(fun acc (_char, count)-> acc +. count)
      ~init:0.
      rawEntries
    in
    rawEntries
      |> List.map ~f:(fun (char, count)-> (char, count /. quantity))

  let buildIndex entries=
    let tree= Tree.create None in
    List.iter ~f:(fun (token, freq)->
      Tree.set tree (split_utf8 token) freq)
      entries;
    tree

  type word= string list * float
  type chunk= word list

  let dispConds cd=
    List.iter ~f:(fun (cl, _freq)->
      List.iter ~f:(printf "%s ") cl;
      Out_channel.newline stdout)
      cd;
    Out_channel.newline stdout

  let dispCands cd=
    List.iter ~f:(fun (wl:word list)->
      List.iter ~f:(fun (cl, _freq)->
        List.iter ~f:(Out_channel.output_string stdout) cl;
        Out_channel.output_string stdout " ")
        wl;
        Out_channel.newline stdout)
      cd;
    Out_channel.newline stdout

  let result_of_cand wl=
    List.map ~f:(fun (cl, _freq)->
      String.concat ~sep:"" cl)
      wl
    |> String.concat ~sep:"|"

  let condWord node s=
    let rec condWord node cl pos= match cl with
    | []-> []
    | c::tl-> match Tree.sub node [c] with
      | None-> []
      | Some node-> match Tree.get node [] with
        | None-> condWord node tl (pos+1)
        | Some freq-> ((List.split_n s (pos+1) |> Tuple2.get1), freq)
          :: (condWord node tl (pos+1))
    in
    condWord node s 0

  let candidates wordDict s max=
    let rec candidates s max=
      if max > 0 && List.length s > 0 then
        let words= condWord wordDict s in
        List.map
          ~f:(fun word->
            let (cl, _freq)= word in
            let suffix= candidates
              (List.split_n s (List.length cl) |> Tuple2.get2)
              (max-1) in
            match suffix with
            | []-> [[word]]
            | _-> List.map ~f:(fun suffix-> word::suffix) suffix)
          words
        |> List.concat
      else []
    in
    candidates (split_utf8 s) max
end

let length_word (sl, _)= List.fold
  ~f:(fun acc s-> acc + String.length s)
  ~init:0
  sl

let length_chunk chunk= List.fold
  ~f:(fun acc word-> acc + length_word word)
  ~init:0
  chunk

let average chunk= (/.)
  (List.fold
    ~f:(fun acc word-> acc + length_word word)
    ~init:0
    chunk
    |> Float.of_int)
  (List.length chunk |> Float.of_int)

let possibility_mul chunk= List.fold
  ~f:(fun acc (_, freq)-> acc *. freq)
  ~init:1.
  chunk

let variance chunk=
  let avg= average chunk in
  (/.)
    (List.fold
      ~f:(fun acc word->
        acc +. (Caml.( ** ) (Float.of_int (length_word word) -. avg) 2.))
      ~init:0.
      chunk
    )
    (List.length chunk |> Float.of_int)

module MMSEG = struct
  let rule1 chunks=
    let (_len, res)=
      List.fold
        ~f:(fun (len, res) chunk->
          let len_curr = length_chunk chunk in
          if len_curr = len then
            (len, chunk::res)
          else
            if len_curr > len then
              (len_curr, [chunk])
            else
              (len, res)
          )
        ~init:(0, [])
        chunks
    in
    List.rev res

  let rule2 chunks=
    let (_avg, res)=
      List.fold
        ~f:(fun (avg, res) chunk->
          let open Float in
          let avg_curr = average chunk in
          if avg_curr = avg then
            (avg, chunk::res)
          else
            if avg_curr > avg then
              (avg_curr, [chunk])
            else
              (avg, res))
        ~init:(0., [])
        chunks
    in
    List.rev res

  let rule3 chunks=
    let (_vari, res)=
      List.fold
        ~f:(fun (vari, res) chunk->
          let open Float in
          let vari_curr = variance chunk in
          if vari_curr = vari then
            (vari, chunk::res)
          else
            if vari_curr < vari then
              (vari_curr, [chunk])
            else
              (vari, res))
        ~init:(Float.infinity, [])
        chunks
    in
    List.rev res

  let rule4 chunks=
    let (_possibility, res)=
      List.fold
        ~f:(fun (possibility, res) chunk->
          let open Float in
          let possibility_curr = possibility_mul chunk in
          if possibility_curr = possibility then
            (possibility, chunk::res)
          else
            if possibility_curr > possibility then
              (possibility_curr, [chunk])
            else
              (possibility, res))
        ~init:(0., [])
        chunks
    in
    List.rev res

  let rule_final= List.hd

  let seg chunks= chunks |> rule1 |> rule2 |> rule3 |> rule4 |> rule_final
end

module MPSEG = struct
end

