(setq lambdas
  '(("inc" `(λ (x)(+ x 2)))
    ("add" `(λ (x y)(+ x y)))
    ("acc" `(λ (l)(foldl + 10 l)))
    ))

(def filter (lamdas) 
  (foldr (λ ((_ fn) acc)
           (case fn 
             (((_  ) _) . (cons fn acc))
             (((_ _) _) . acc)
             (_         . acc)))
         lambdas
         ()))
