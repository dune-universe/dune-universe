(*
 * Copyright (c) 2021 Tarides <contact@tarides.com>
 * Copyright (c) 2021 Gabriel Belouze <gabriel.belouze@ens.psl.eu>
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

type t = {
  mutable bytes_read : int;
  mutable nb_reads : int;
  mutable bytes_written : int;
  mutable nb_writes : int;
  mutable nb_replace : int;
}

let fresh_stats () =
  { bytes_read = 0; nb_reads = 0; bytes_written = 0; nb_writes = 0; nb_replace = 0 }

let stats = fresh_stats ()

let get () = stats

let reset_stats () =
  stats.bytes_read <- 0;
  stats.nb_reads <- 0;
  stats.bytes_written <- 0;
  stats.nb_writes <- 0;
  stats.nb_replace <- 0

let incr_bytes_read n = stats.bytes_read <- stats.bytes_read + n

let incr_bytes_written n = stats.bytes_written <- stats.bytes_written + n

let incr_nb_reads () = stats.nb_reads <- succ stats.nb_reads

let incr_nb_writes () = stats.nb_writes <- succ stats.nb_writes

let incr_nb_replace () = stats.nb_replace <- succ stats.nb_replace

let add_read n =
  incr_bytes_read n;
  incr_nb_reads ()

let add_write n =
  incr_bytes_written n;
  incr_nb_writes ()
