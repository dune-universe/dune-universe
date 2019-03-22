(* File: mindstorm_macros.ml

   Copyright (C) 2016-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(* Monadic bindings, Unix module,... and their imperative counterparts.
   These macros allow to write a single version of the code that works
   both for direct and Lwt modules. *)
#ifdef LWT
open Lwt
#define LWT_t Lwt.t
#define ONLY_LWT(e) e
#define LET(v, expr) (expr) >>= fun v ->
#define EXEC(expr) (expr) >>= fun () ->
#define RETURN(x) Lwt.return(x)
#define FAIL(exn) Lwt.fail(exn)
#define UNIX(fn) Lwt_unix.fn
#define TRY_BIND(expr0, v, expr_v, exn_patt) \
  Lwt.try_bind (fun () -> expr0) (fun v -> expr_v) (function exn_patt)
#define EXCEPTION(e) e
#define LOCK(conn) Conn.lock conn >>= fun () ->
#define UNLOCK(conn) Conn.unlock conn;

#else
#define LWT_t
#define ONLY_LWT(e)
#define LET(v, expr) let v = expr in
#define EXEC(expr) (expr);
#define RETURN(x) (x)
#define FAIL(exn) raise(exn)
#define UNIX(fn) Unix.fn
#define LOCK(conn)
#define UNLOCK(conn)

#if OCAML_MAJOR >= 4 && OCAML_MINOR >= 2
(* More efficient *)
#define TRY_BIND(expr0, v, expr_v, exn_patt) \
  (match expr0 with v -> (expr_v) | exn_patt)
#define EXCEPTION(e) exception e

#else
type 'a val_or_exn = Val of 'a | Exn of exn
#define TRY_BIND(expr0, v, expr_v, exn_patt) \
  (match (try Val(expr0) with exn_ -> Exn exn_) with \
   | Val v -> (expr_v) \
   | Exn exn_ -> (match exn_ with exn_patt))
#define EXCEPTION(e) e
#endif

#endif
