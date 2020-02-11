
  $ compile="ocamlfind ocamlc -c -package visitors.ppx -package visitors.runtime"

  $ $compile $TESTDIR/conflict.ml 2>&1 | sed -e "s|$TESTDIR/||"
  File "conflict.ml", line 7, characters 30-35:
  Warning 22: visitors: name clash: the types t and Elt.t
  both have visitor methods named visit_t.
  Please consider using [@@name] at type declaration sites
  or [@name] at type reference sites.
  File "conflict.ml", line 5, characters 0-111:
  Error: This expression has type Elt.t = int
         but an expression was expected of type t

  $ $compile $TESTDIR/conflict_at_name.ml 2>&1 | sed -e "s|$TESTDIR/||"
  File "conflict_at_name.ml", line 7, characters 31-38:
  Warning 22: visitors: name clash: the types t and Elt.elt
  both have visitor methods named visit_t.
  Please consider using [@@name] at type declaration sites
  or [@name] at type reference sites.
  File "conflict_at_name.ml", line 5, characters 0-126:
  Error: This expression has type Elt.elt = int
         but an expression was expected of type t

  $ $compile $TESTDIR/conflict_atat_name.ml 2>&1 | sed -e "s|$TESTDIR/||"
  File "conflict_atat_name.ml", line 6, characters 0-25:
  Warning 22: visitors: name clash: the types t and elt
  both have visitor methods named visit_t.
  Please consider using [@@name] at type declaration sites
  or [@name] at type reference sites.
  File "conflict_atat_name.ml", line 1, characters 0-136:
  Error: The method `visit_t' has multiple definitions in this object

  $ $compile $TESTDIR/datacon.ml 2>&1 | sed -e "s|$TESTDIR/||"
  File "datacon.ml", line 6, characters 2-10:
  Warning 22: visitors: name clash: the data constructors A and A
  both have visitor methods named visit_A.
  Please consider using [@name] at data constructor declaration sites.
  File "datacon.ml", line 6, characters 2-10:
  Warning 30: the constructor A is defined in both types t and u.
  File "datacon.ml", line 1, characters 0-90:
  Error: The method `visit_A' has multiple definitions in this object

  $ $compile $TESTDIR/datacon_at_name.ml 2>&1 | sed -e "s|$TESTDIR/||"
  File "datacon_at_name.ml", line 6, characters 2-22:
  Warning 22: visitors: name clash: the data constructors A and C
  both have visitor methods named visit_A.
  Please consider using [@name] at data constructor declaration sites.
  File "datacon_at_name.ml", line 1, characters 0-102:
  Error: The method `visit_A' has multiple definitions in this object
