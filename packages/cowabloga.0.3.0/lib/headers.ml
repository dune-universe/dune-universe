(*
 * Copyright (c) 2013 Richard Mortier <mort@cantab.net>
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

(** Some default HTTP response types as HTTP headers; taken from
    http://www.iana.org/assignments/media-types/ *)

let html = ["content-type", "text/html; charset=UTF-8"]
let xhtml = ["content-type", "application/xhtml+xml; charset=UTF-8"]
let css = ["content-type", "text/css; charset=UTF-8"]

let atom = ["content-type", "application/atom+xml; charset=UTF-8"]
let javascript = ["content-type", "application/javascript; charset=UTF-8"]
let json = ["content-type", "application/json; charset=UTF-8"]
let pdf = ["content-type", "application/pdf"]

let png = ["content-type", "image/png"]
let jpeg = ["content-type", "image/jpeg"]
