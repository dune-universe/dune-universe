(setq DATA '(("Name"   "Age" "City"  )
             ("Alex"   "32" "London" )
             ("John"   "17" "Chicago")
             ("Marc"   "25" "Lyon"   )
             ("Sophie" "29" "Nice"   )
             ))

(out "result.csv"
  (iter '((t)(prinl (join ", " t))) DATA))
