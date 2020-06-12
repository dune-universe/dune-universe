(*
 * Copyright (c) 2015 David Scott <dave@recoil.org>
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
 *
 *)
open Lwt.Infix

module Compare = Mirage_block_combinators.Compare(Ramdisk)(Ramdisk)
module Fill = Mirage_block_combinators.Fill(Ramdisk)
module Safe = Mirage_block_combinators.Make_safe(Ramdisk)
module Copy = Mirage_block_combinators.Copy(Ramdisk)(Ramdisk)
module Sparse_copy = Mirage_block_combinators.Sparse_copy(Ramdisk)(Ramdisk)

let expect_ok msg = function
  | Error _ -> failwith msg
  | Ok x -> x

let expect_ok_msg pp_error = function
  | Error e -> Fmt.kstrf Alcotest.fail "%a" pp_error e
  | Ok x   -> x

let expect_unsafe pp_error = function
  | Error (`Unsafe _) -> ()
  | Ok _              -> Alcotest.fail "unexpected ok"
  | Error e           -> Fmt.kstrf Alcotest.fail "%a" pp_error e

let ramdisk_compare () =
  let t =
    Ramdisk.connect ~name:"from" >>= fun from ->
    Ramdisk.connect ~name:"dest" >>= fun dest ->
    Compare.v from dest >>= fun x ->
    let x = expect_ok_msg Compare.pp_error x in
    Alcotest.(check int) __LOC__ 0 x;
    Lwt.return ()
  in
  Lwt_main.run t

let different_compare () =
  let t =
    Ramdisk.connect ~name:"from" >>= fun from ->
    Fill.random from >>= fun x ->
    let () = expect_ok "patterns" x in
    Ramdisk.connect ~name:"dest" >>= fun dest ->
    Compare.v from dest >>= fun x ->
    let x = expect_ok_msg Compare.pp_error x in
    if x = 0 then Alcotest.fail "different disks compared the same";
    Lwt.return () in
  Lwt_main.run t

let basic_copy () =
  let t =
    Ramdisk.connect ~name:"from" >>= fun src ->
    Ramdisk.connect ~name:"dest" >>= fun dst ->
    Copy.v ~src ~dst >>= fun x ->
    let () = expect_ok_msg Copy.pp_error x in
    Compare.v src dst >>= fun x ->
    let x = expect_ok_msg Compare.pp_error x in
    Alcotest.(check int) __LOC__ 0 x;
    Lwt.return ()
  in
  Lwt_main.run t

let random_copy () =
  let t =
    Ramdisk.connect ~name:"from" >>= fun src ->
    Fill.random src >>= fun x ->
    let () = expect_ok "patterns" x in
    Ramdisk.connect ~name:"dest" >>= fun dst ->
    Copy.v ~src ~dst >>= fun x ->
    let () = expect_ok_msg Copy.pp_error x in
    Compare.v src dst >>= fun x ->
    let x = expect_ok_msg Compare.pp_error x in
    Alcotest.(check int) __LOC__ 0 x;
    Lwt.return ()
  in
  Lwt_main.run t

let sparse_copy () =
  let t =
    Ramdisk.connect ~name:"from" >>= fun src ->
    Ramdisk.connect ~name:"dest" >>= fun dst ->
    Sparse_copy.v ~src ~dst >>= fun x ->
    let () = expect_ok_msg Sparse_copy.pp_error x in
    Compare.v src dst >>= fun x ->
    let x = expect_ok_msg Compare.pp_error x in
    Alcotest.(check int) __LOC__ 0 x;
    Lwt.return ()
  in
  Lwt_main.run t

let safe_bad_buffer_length () =
  let t =
    Ramdisk.connect ~name:"ramdisk" >>= fun ramdisk ->
    Ramdisk.get_info ramdisk >>= fun info ->
    let bad_buffer = Cstruct.create (info.Mirage_block.sector_size + 1) in
    Safe.read ramdisk 0L [ bad_buffer ] >>= fun x ->
    expect_unsafe Safe.pp_error x;
    Safe.write ramdisk 0L [ bad_buffer ] >>= fun x ->
    expect_unsafe Safe.pp_write_error x;
    Lwt.return ()
  in
  Lwt_main.run t

let safe_good_buffer_length () =
  let t =
    Ramdisk.connect ~name:"ramdisk" >>= fun ramdisk ->
    Ramdisk.get_info ramdisk >>= fun info ->
    let good_buffer = Cstruct.create (info.Mirage_block.sector_size + 0) in
    Safe.read ramdisk 0L [ good_buffer ] >>= fun x ->
    expect_ok "Safe.read" x;
    Safe.write ramdisk 0L [ good_buffer ] >>= fun x ->
    expect_ok "Safe.write" x;
    Lwt.return ()
  in
  Lwt_main.run t

let safe_bad_sector_start () =
  let t =
    Ramdisk.connect ~name:"ramdisk" >>= fun ramdisk ->
    Ramdisk.get_info ramdisk >>= fun info ->
    let sector = Cstruct.create info.Mirage_block.sector_size in
    Safe.read ramdisk (-1L) [ sector ] >>= fun x ->
    expect_unsafe Safe.pp_error x;
    Safe.write ramdisk (-1L) [ sector ] >>= fun x ->
    expect_unsafe Safe.pp_write_error x;
    Lwt.return ()
  in
  Lwt_main.run t

let safe_good_sector_start () =
  let t =
    Ramdisk.connect ~name:"ramdisk" >>= fun ramdisk ->
    Ramdisk.get_info ramdisk >>= fun _info ->
    Safe.read ramdisk 0L [] >>= fun x ->
    expect_ok "Safe.read" x;
    Safe.write ramdisk 0L [] >>= fun x ->
    expect_ok "Safe.write" x;
    Lwt.return ()
  in
  Lwt_main.run t

let safe_bad_sector_end () =
  let t =
    Ramdisk.connect ~name:"ramdisk" >>= fun ramdisk ->
    Ramdisk.get_info ramdisk >>= fun info ->
    let sector = Cstruct.create info.Mirage_block.sector_size in
    Safe.read ramdisk info.Mirage_block.size_sectors [ sector ] >>= fun x ->
    expect_unsafe Safe.pp_error x;
    Safe.write ramdisk info.Mirage_block.size_sectors [ sector ] >>= fun x ->
    expect_unsafe Safe.pp_write_error x;
    Lwt.return ()
  in
  Lwt_main.run t

let safe_good_sector_end () =
  let t =
    Ramdisk.connect ~name:"ramdisk" >>= fun ramdisk ->
    Ramdisk.get_info ramdisk >>= fun info ->
    let sector = Cstruct.create info.Mirage_block.sector_size in
    Safe.read ramdisk (Int64.pred info.Mirage_block.size_sectors) [ sector ]
    >>= fun x ->
    expect_ok "Safe.read" x;
    Safe.write ramdisk (Int64.pred info.Mirage_block.size_sectors) [ sector ]
    >>= fun x ->
    expect_ok "Safe.write" x;
    Lwt.return ()
  in
  Lwt_main.run t

let tests = [
  "ramdisk compare"                         , ramdisk_compare;
  "different compare"                       , different_compare;
  "copy empty ramdisk"                      , basic_copy;
  "copy a random disk"                      , random_copy;
  "sparse copy an empty disk"               , sparse_copy;
  "safe wrapper catches bad buffer lengths" , safe_bad_buffer_length;
  "safe wrapper accepts good buffer lengths", safe_good_buffer_length;
  "safe wrapper catches bad sector start"   , safe_bad_sector_start;
  "safe wrapper accepts good sector start"  , safe_good_sector_start;
  "safe wrapper catches bad sector end"     , safe_bad_sector_end;
  "safe wrapper accepts good sector end"    , safe_good_sector_end;
]

let () =
  Alcotest.run "ramdisk" [
    "tests", List.map (fun (n, f) -> n, `Quick, f) tests
  ]
