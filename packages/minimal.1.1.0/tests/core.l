#
# Copyright (c) 2018 Xavier R. Guérin <copyright@applepine.org>
# 
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
# 
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
#

(load "@lib/list.l")
(load "@lib/unit.l")

(def core::add ()
  (test::eq (+ 1 1) 2))

(def core::sub ()
  (test::eq (- 1 1) 0))

(def core::mul ()
  (test::eq (* 2 5) 10))

(test::run
  "Core test suite"
  '(("core::add" `core::add)
    ("core::sub" `core::sub)
    ("core::mul" `core::mul)
    ))
