(setq lambdas
  '(("inc" ((x)(+ x 2)))
    ("add" ((x y)(+ x y)))
    ("acc" ((l)(foldl + 10 l)))
    ))

(def filter (lamdas) 
  (foldr '(((_ fn) acc)
           (case fn 
             (((_  ) _) . (cons fn acc))
             (((_ _) _) . acc)
             (_         . acc)))
         lambdas
         ()))
