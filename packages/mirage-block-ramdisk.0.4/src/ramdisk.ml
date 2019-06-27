(*
 * Copyright (C) 2011-2013 Citrix Systems Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type 'a io = 'a Lwt.t

(* NB not actually page-aligned *)
type page_aligned_buffer = Cstruct.t

type error = Mirage_block.error
let pp_error = Mirage_block.pp_error
type write_error = Mirage_block.write_error
let pp_write_error = Mirage_block.pp_write_error

module Int64Map = Map.Make(Int64)

type t = {
  mutable map: page_aligned_buffer Int64Map.t;
  mutable info: Mirage_block.info;
  id: string;
}

let devices = Hashtbl.create 1

let get_info { info; _ } = Lwt.return info

let create ~name ~size_sectors ~sector_size =
  let map = Int64Map.empty in
  let info = {
    Mirage_block.read_write = true;
    size_sectors;
    sector_size;
  } in
  let device = { map; info; id = name } in
  Hashtbl.replace devices name device;
  Lwt.return (Ok device)

let destroy ~name = Hashtbl.remove devices name

let connect ~name =
  if Hashtbl.mem devices name
  then Lwt.return (Hashtbl.find devices name)
  else begin
    let _ = create ~name ~size_sectors:32768L ~sector_size:512 in
    Lwt.return (Hashtbl.find devices name)
  end

let disconnect _ = Lwt.return ()

let rec read x sector_start buffers = match buffers with
  | [] -> Lwt.return (Ok ())
  | b :: bs ->
    if Int64Map.mem sector_start x.map
    then Cstruct.blit (Int64Map.find sector_start x.map) 0 b 0 512
    else Cstruct.(memset (sub b 0 512) 0);
    read x (Int64.succ sector_start)
      (if Cstruct.len b > 512
       then (Cstruct.shift b 512) :: bs
       else bs)

let rec write x sector_start buffers = match buffers with
  | [] -> Lwt.return (Ok ())
  | b :: bs ->
    if Cstruct.len b = 512 then begin
      x.map <- Int64Map.add sector_start b x.map;
      write x (Int64.succ sector_start) bs
    end else begin
      x.map <- Int64Map.add sector_start (Cstruct.sub b 0 512) x.map;
      write x (Int64.succ sector_start) (Cstruct.shift b 512 :: bs)
    end

let seek_mapped t from =
  let rec loop from =
    if from >= t.info.Mirage_block.size_sectors || Int64Map.mem from t.map
    then Lwt.return (Ok from)
    else loop (Int64.succ from) in
  loop from

let seek_unmapped t from =
  let rec loop from =
    if from >= t.info.Mirage_block.size_sectors || not (Int64Map.mem from t.map)
    then Lwt.return (Ok from)
    else loop (Int64.succ from) in
  loop from

let resize x new_size_sectors =
  let to_keep, _ =
    Int64Map.partition (fun sector_start _ -> sector_start < new_size_sectors) x.map in
  x.map <- to_keep;
  x.info <- { x.info with Mirage_block.size_sectors = new_size_sectors };
  Lwt.return (Ok ())

let flush _ = Lwt.return (Ok ())
