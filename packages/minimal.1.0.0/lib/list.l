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

(def iter (iter:fun iter:lst)
  "Iterate over list lst and call fun on each item."
  (case iter:lst
    ((_) . (iter:fun (car iter:lst)))
    ( _  . (prog
             (iter:fun (car iter:lst))
             (iter iter:fun (cdr iter:lst))))))

(def foldl (foldl:fun foldl:acc foldl:lst)
  "Fold lst into acc with fun from left to right."
  (?: (lst? foldl:lst)
      (?: (nil? foldl:lst)
          foldl:acc
          (foldl foldl:fun (foldl:fun foldl:acc (car foldl:lst)) (cdr foldl:lst)))
      (foldl:fun foldl:acc foldl:lst)))

(def foldr (foldr:fun foldr:lst foldr:acc)
  "Fold lst into acc with fun from right to left."
  (?: (lst? foldr:lst)
      (?: (nil? foldr:lst)
          foldr:acc
          (foldr:fun (car foldr:lst) (foldr foldr:fun (cdr foldr:lst) foldr:acc)))
      (foldr:fun foldr:lst foldr:acc)))

(def map (map:fun map:lst)
  "Map lst into lst' using fun"
  (? (and (lst? map:lst) (not (nil? map:lst)))
    (cons
      (map:fun (car map:lst))
      (map map:fun (cdr map:lst)))))

(def map2 (map2:fun map2:lst1 map2:lst2)
  "Map lst1 and lst2 into lst' using fun."
  (? (and (lst? map2:lst1) (lst? map2:lst2))
    (?! (and (nil? map2:lst1) (nil? map2:lst2))
      (cons
        (map2:fun (car map2:lst1) (car map2:lst2))
        (map2 map2:fun (cdr map2:lst1) (cdr map2:lst2))))))

(def zip (zip:lst1 zip:lst2)
  "Bind entries from lst1 and lst2 into a list of pairs."
  (map2 cons zip:lst1 zip:lst2))

(def filter (filter:fun filter:lst)
  "Scan lst and remove any element for which fun returns T."
  (foldr
    '((e acc) (?: (filter:fun e) acc (cons e acc)))
    filter:lst NIL))

(def flatten (flatten:lst)
  "Flatten multi-layer list lst."
  (?! (nil? flatten:lst)
      (?: (lst? flatten:lst)
          (foldr '((lst acc)(conc (flatten lst) acc)) flatten:lst NIL)
          (list flatten:lst))))

(def rev (rev:lst)
  "Return the reverse of lst."
  (foldl '((acc e)(cons e acc)) NIL rev:lst))

(def len (len:lst)
  "Return the length of list lst"
  (foldl '((acc _)(+ acc 1)) 0 len:lst))

(def assoc (assoc:key assoc:lst)
  "Return the value for assoc:key in the associative list assoc:lst."
  (eval (cons 'case assoc:key assoc:lst)))
