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

(def
  pretty (e)
  "Pretty-print an element e"
  (?: (atm? e)
    (prin e)
    (prog
      (prin "(")
      (let ((hd . (car e))
            (tl . (cdr e)))
        (prog
          (pretty hd)
          (? (sym? hd) (prin "\n"))
          (pretty-list tl)
          ))))

  pretty-list (lst)
  "Pretty-print a list specifically"
  (prog
    (?: (nil? lst)
      (prin ")")
      (prog
        (prin " ")
        (pretty (car lst))
        (pretty-list (cdr lst))
        ))))
