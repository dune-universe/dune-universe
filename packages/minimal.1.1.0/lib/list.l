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

(def iter (fun lst)
  "Iterate over list lst and call fun on each item."
  (case lst
    ((_) . (fun (car lst)))
    ( _  . (prog
             (fun (car lst))
             (iter fun (cdr lst))))))

(def foldl (fun acc lst)
  "Fold lst into acc with fun from left to right."
  (?: (lst? lst)
      (?: (nil? lst)
          acc
          (foldl fun (fun acc (car lst)) (cdr lst)))
      (fun acc lst)))

(def foldr (fun lst acc)
  "Fold lst into acc with fun from right to left."
  (?: (lst? lst)
      (?: (nil? lst)
          acc
          (fun (car lst) (foldr fun (cdr lst) acc)))
      (fun lst acc)))

(def map (fun lst)
  "Map lst into lst' using fun"
  (? (and (lst? lst) (not (nil? lst)))
    (cons
      (fun (car lst))
      (map fun (cdr lst)))))

(def map2 (fun lst1 lst2)
  "Map lst1 and lst2 into lst' using fun."
  (? (and (lst? lst1) (lst? lst2))
    (?! (and (nil? lst1) (nil? lst2))
      (cons
        (fun (car lst1) (car lst2))
        (map2 fun (cdr lst1) (cdr lst2))))))

(def zip (lst1 lst2)
  "Bind entries from lst1 and lst2 into a list of pairs."
  (map2 cons lst1 lst2))

(def filter (fun lst)
  "Scan lst and remove any element for which fun returns T."
  (foldr
    (λ (e acc) (?: (fun e) acc (cons e acc)))
    lst NIL))

(def flatten (lst)
  "Flatten multi-layer list lst."
  (?! (nil? lst)
      (?: (lst? lst)
          (foldr (λ (lst acc) (conc (flatten lst) acc)) lst NIL)
          (list lst))))

(def rev (lst)
  "Return the reverse of lst."
  (foldl (λ (acc e) (cons e acc)) NIL lst))

(def len (lst)
  "Return the length of list lst"
  (foldl (λ (acc _) (+ acc 1)) 0 lst))

(def assoc (key lst)
  "Return the value for key in the associative list lst."
  (eval (cons 'case key lst)))
