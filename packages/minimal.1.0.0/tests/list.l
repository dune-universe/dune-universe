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

(def list::flatten ()
  (prog
    (test::eq (flatten (1)) (1))
    (test::eq (flatten (1 (2))) (1 2))
    (test::eq (flatten (1 (2) 3)) (1 2 3))
    (test::eq (flatten (1 (2 (3 (4 5) 6 (7 8) 9) 10) 11)) (1 2 3 4 5 6 7 8 9 10 11))
    ))

(def list::len ()
  (prog
    (test::eq (len ()) 0)
    (test::eq (len (1)) 1)
    (test::eq (len (1 2)) 2)
    (test::eq (len (1 2 3)) 3)
    ))

(def list::rev ()
  (prog
    (test::eq (rev (1)) (1))
    (test::eq (rev (1 2 3)) (3 2 1))
    ))

(def list::zip ()
  (prog
    (test::eq (zip (1 2 3) ("a" "b" "c")) ((1 . "a") (2 . "b") (3 . "c")))
    ))

(def list::filter ()
  (prog
    (test::eq (filter '((e)(= e 1)) (1 2 1 3 1 4 1)) (2 3 4))
    ))

(test::run
  "Language library test suite"
  '(
    ("list::flatten" `list::flatten)
    ("list::len"     `list::len)
    ("list::rev"     `list::rev)
    ("list::zip"     `list::zip)
    ("list::filter"  `list::filter)
    ))
