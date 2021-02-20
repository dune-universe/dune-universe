  $ (cd ../../../../default && ./regression/test000.exe)
  Tree: 1242
  Number of nodes: 4
  Incremented: 2353
  $ (cd ../../../../default && ./regression/test001.exe)
  sum:
  5
  sumN:
  55
  sum with debug:
  R @ 0
  R @ 1
  B + @ 2
  W @ 3
  E @ 4
  5
  sumN with debug:
  R @ 0
  S n @ 1
  C 0 @ 2
  S s @ 3
  L n @ 4
  JF 15 @ 5
  L n @ 6
  C 1 @ 7
  B - @ 8
  S n @ 9
  R @ 10
  L s @ 11
  B + @ 12
  S s @ 13
  J 4 @ 14
  L n @ 4
  JF 15 @ 5
  L n @ 6
  C 1 @ 7
  B - @ 8
  S n @ 9
  R @ 10
  L s @ 11
  B + @ 12
  S s @ 13
  J 4 @ 14
  L n @ 4
  JF 15 @ 5
  L n @ 6
  C 1 @ 7
  B - @ 8
  S n @ 9
  R @ 10
  L s @ 11
  B + @ 12
  S s @ 13
  J 4 @ 14
  L n @ 4
  JF 15 @ 5
  L n @ 6
  C 1 @ 7
  B - @ 8
  S n @ 9
  R @ 10
  L s @ 11
  B + @ 12
  S s @ 13
  J 4 @ 14
  L n @ 4
  JF 15 @ 5
  L n @ 6
  C 1 @ 7
  B - @ 8
  S n @ 9
  R @ 10
  L s @ 11
  B + @ 12
  S s @ 13
  J 4 @ 14
  L n @ 4
  JF 15 @ 5
  L n @ 6
  C 1 @ 7
  B - @ 8
  S n @ 9
  R @ 10
  L s @ 11
  B + @ 12
  S s @ 13
  J 4 @ 14
  L n @ 4
  JF 15 @ 5
  L n @ 6
  C 1 @ 7
  B - @ 8
  S n @ 9
  R @ 10
  L s @ 11
  B + @ 12
  S s @ 13
  J 4 @ 14
  L n @ 4
  JF 15 @ 5
  L n @ 6
  C 1 @ 7
  B - @ 8
  S n @ 9
  R @ 10
  L s @ 11
  B + @ 12
  S s @ 13
  J 4 @ 14
  L n @ 4
  JF 15 @ 5
  L n @ 6
  C 1 @ 7
  B - @ 8
  S n @ 9
  R @ 10
  L s @ 11
  B + @ 12
  S s @ 13
  J 4 @ 14
  L n @ 4
  JF 15 @ 5
  L n @ 6
  C 1 @ 7
  B - @ 8
  S n @ 9
  R @ 10
  L s @ 11
  B + @ 12
  S s @ 13
  J 4 @ 14
  L n @ 4
  JF 15 @ 5
  L s @ 15
  W @ 16
  E @ 17
  55
  sum:
  5
  sumN:
  55
  $ (cd ../../../../default && ./regression/test002.exe)
  (1+a)
  3
  $ (cd ../../../../default && ./regression/test003.exe)
  A (B (C ([1; 2; 3; 4])))
  B (A (D ("3")))
  A (B (new C ([1; 2; 3; 4])))
  $ (cd ../../../../default && ./regression/test004.exe)
  `A (`B ([`C (3); `C (4)]))
  `B ([`A (`D ("3")); `C (5)])
  new A `B ([new `C (3); new `C (4)])
  $ (cd ../../../../default && ./regression/test005.exe)
  new!
  new!
  new!
  new!
  `A (`B (`A (new D 4)))
  new c0!
  new!
  new c0!
  new `B (`A (new `D 4))
  new c0!
  new `E 18
  $ (cd ../../../../default && ./regression/test006.exe)
  Node (1, [Node (2, [Leaf]); Node (3, [Leaf]); Node (4, [Node (5, []); Leaf])])
  Node (1, [Node (2, [Leaf]); Node (3, [Leaf]); Node (4, [Node (5, []); Leaf])])
  $ (cd ../../../../default && ./regression/test007.exe)
  A ([1; 2; 3], [4; 5; 6])
  A ([1; 2; 3], [4; 5; 6])
  $ (cd ../../../../default && ./regression/test008.exe)
  3
  $ (cd ../../../../default && ./regression/test009.exe)
  X (A (1), [Y (A (2), []); X (A (2), []); Y (A (3), [])])
  X (A (1), [Y (A (2), []); X (A (2), []); Y (A (3), [])])
  $ (cd ../../../../default && ./regression/test010.exe)
  17
  $ (cd ../../../../default && ./regression/test011.exe)
  $ (cd ../../../../default && ./regression/test012.exe)
  `A (3)
  `D ("2")
  `A (3)
  `D ("2")
  true
  true
  true
  true
  false
  $ (cd ../../../../default && ./regression/test013.exe)
  `D (`B (`C ("5")))
  `D (`B (new C 5))
  $ (cd ../../../../default && ./regression/test014.exe)
  A (B (E (C ([1; 2; 3; 4]))))
  B (E (A (D ("3"))))
  new A B (E (new C ([1; 2; 3; 4])))
  $ (cd ../../../../default && ./regression/test015.exe)
  Node (1, [Node (2, [Node (5, [])]); Node (3, []); Node (4, [Node (6, [])])])
  21
  1, 2, 5, 3, 4, 6
  6, 4, 3, 5, 2, 1
  $ (cd ../../../../default && ./regression/test016.exe)
  x == x: true
  x == y: false
  x == z: false
  $ (cd ../../../../default && ./regression/test017.exe)
  x == x: true
  x == y: false
  x == z: false
  $ (cd ../../../../default && ./regression/test018.exe)
  x == x: EQ
  x == y: LT
  x == z: LT
  $ (cd ../../../../default && ./regression/test019.exe)
  x   == x: EQ
  x   == y: LT
  y   == x: GT
  x   == z: LT
  z'  == z: EQ
  z'' == z: LT
  $ (cd ../../../../default && ./regression/test020.exe)
  x   == x: EQ
  x   == y: LT
  y   == x: GT
  x   == z: LT
  z'  == z: EQ
  z'' == z: LT
  $ (cd ../../../../default && ./regression/test021.exe)
  `A (`B (`C ([1; 2; 3; 4])))
  `B (`A (`D ("3")))
  `A (`B (new `C ([1; 2; 3; 4])))
  $ (cd ../../../../default && ./regression/test022.exe)
  x=A ([Some (1); None; Some (2); None])
  y=A ([Some (1); None; Some (2); Some (4)])
  z=A ([])
  x == x = true
  x == y = false
  x == z = false
  $ (cd ../../../../default && ./regression/test023.exe)
  `A (3)
  `D ("2")
  `A (3)
  `D ("2")
  true
  true
  true
  true
  false
  $ (cd ../../../../default && ./regression/test024.exe)
  `A (3)
  `C (2)
  `A (3)
  `C (2)
  true
  true
  true
  true
  false
  $ (cd ../../../../default && ./regression/test025.exe)
  `A (3)
  `A ("2")
  `A (3)
  `A ("2")
  true
  true
  $ (cd ../../../../default && ./regression/test026.exe)
  A (3)
  A (2)
  A (3)
  A (2)
  true
  true
  $ (cd ../../../../default && ./regression/test027.exe)
  x == x: true
  x == y: false
  compare (x, x) = EQ
  compare (x, y) = GT
  compare (y, x) = LT
  { x=1; y="2"; a=3; b=`B; }
  1
  2
  a
  `B
  $ (cd ../../../../default && ./regression/test028.exe)
  x == x: true
  x == y: false
  compare (x, x) = EQ
  compare (x, y) = GT
  compare (y, x) = LT
  (1, "2", 3, `B)
  1
  2
  a
  `B
  $ (cd ../../../../default && ./regression/test029.exe)
  x == x: true
  x == y: false
  compare (x, x) = EQ
  compare (x, y) = GT
  compare (y, x) = LT
  (1, ("2", (3, `B)))
  1
  2
  a
  `B
  $ (cd ../../../../default && ./regression/test030.exe)
  x == x: true
  x == y: false
  compare (x, x) = EQ
  compare (x, y) = LT
  compare (y, x) = GT
  A ((1, 2))
  A (1, 2)
  $ (cd ../../../../default && ./regression/test031.exe)
  { x=1; y="2"; a=3; b=`B; }
  1
  2
  a
  `B
  $ (cd ../../../../default && ./regression/test032.exe)
  "abc"
  $ (cd ../../../../default && ./regression/test036.exe)
  <html><ul><li>Add<ul><li>Ident<ul><li>&quot;b&quot;</li></ul></li><li>Add<ul><li>Sub<ul><li>tuple<ul><li>Ident<ul><li>&quot;a&quot;</li></ul></li><li>Ident<ul><li>&quot;b&quot;</li></ul></li></ul></li></ul></li><li>Const<ul><li>1</li></ul></li></ul></li></ul></li></ul></html>
  <html><ul><li><ul>str<li>aAdd<ul><li>Ident<ul><li>&quot;b&quot;</li></ul></li><li>Add<ul><li>Sub<ul><li>tuple<ul><li>Ident<ul><li>&quot;a&quot;</li></ul></li><li>Ident<ul><li>&quot;b&quot;</li></ul></li></ul></li></ul></li><li>Const<ul><li>1</li></ul></li></ul></li></ul></li><li>bIdent<ul><li>&quot;c&quot;</li></ul></li></ul></li></ul></html>
  $ (cd ../../../../default && ./regression/test037.exe)
  a=`B (`B (`A)), map a=`B (`B (`A))
  b=`D (`D (`C)), map b=`D (`D (`C))
  c=`D (`B (`D (`A))), map c=`D (`B (`D (`A)))
  $ (cd ../../../../default && ./regression/test040.exe)
  $ (cd ../../../../default && ./regression/test041.exe)
  $ (cd ../../../../default && ./regression/test042.exe)
  `Cons (2, `Nil)
  $ (cd ../../../../default && ./regression/test082mutal.exe)
  $ (cd ../../../../default && ./regression/test083polyvar.exe)
  Original PV: `A (1)
  Mapped PV: `A (1)
  ****************************
  Original pv: `A (1)
  Mapped pv and showed as a pv_ext: `A (1)
  Original pv_ext: `C (1)
  Mapped PV_ext and showed as a pv_ext: `C (1)
  ****************************
  Original pv_ext: `C (1)
  Mapped pv_ext and showed as a pv_ext2: `C (1)
  Original pv_ext2: `D (1)
  Mapped PV_ext2 and showed as a pv_ext2: `D (1)
  ****************************
  Original pv_ext2: `D (1)
  Mapped pv_ext2 and showed as a pv_ext3: `D (1)
  Original pv_ext3: `E (1)
  Mapped PV_ext3 and showed as a pv_ext3: `E (1.)
  $ (cd ../../../../default && ./regression/test086std.exe)
  $ (cd ../../../../default && ./regression/test087stateful.exe)
  $ (cd ../../../../default && ./regression/test089struct.exe)
  { info=asdf; node=EConst (19); }
  { info=__asdf; node=EConst (19); }
  { info=x; node=EAdd ({ info=y; node=EConst (20); }, { info=z; node=EConst (40); }); }
  { info=__x; node=EAdd ({ info=__y; node=EConst (20); }, { info=__z; node=EConst (40); }); }
  $ (cd ../../../../default && ./regression/test090eval.exe)
  $ (cd ../../../../default && ./regression/test091eval.exe)
  Original: `App (`Abs ("x", `Var ("x")), `Abs ("y", `Var ("y")))
  Converted: `App (`Abs (`Var (0)), `Abs (`Var (0)))
  $ (cd ../../../../default && ./regression/test705.exe)
  `A (`B (`C (3)))
  `B (`A (`D ("3")))
  `E ([1; 2; 3])
  `B (`A (`D ("3")))
  new `E ([1; 2; 3])
  new `B (`A (`D ("3")))
  $ (cd ../../../../default && ./regression/test081llist.exe)
  :: (aaa, bbb)
  :: (aaa, :: (bbb, []))
  Var (5)	Value (asdf)
  Value (:: (aaa, Value (:: (bbb, Var (15)))))
  Modified implementation:
  	Var (5)
  	asdf
  Printing of modified logic list
  :: (aaa, :: (bbb, Var (15)))
  $ (cd ../../../../default && ./regression/test800.exe)
  $ (cd ../../../../default && ./regression/test802mutal.exe)
  new!
  new!
  new!
  new!
  `A (`B (`A (new D 4)))
  new c0!
  new!
  new c0!
  new `B (`A (new `D 4))
  new c0!
  new `E 18
  $ (cd ../../../../default && ./regression/test803polyvar.exe)
  Original PV: `A (1)
  Mapped PV: `A (1)
  ****************************
  Original pv: `A (1)
  Mapped pv and showed as a pv_ext: `A (1)
  Original pv_ext: `C (1)
  Mapped PV_ext and showed as a pv_ext: `C (1)
  ****************************
  Original pv_ext: `C (1)
  Mapped pv_ext and showed as a pv_ext2: `C (1)
  Original pv_ext2: `D (1)
  Mapped PV_ext2 and showed as a pv_ext2: `D (1)
  ****************************
  Original pv_ext2: `D (1)
  Mapped pv_ext2 and showed as a pv_ext3: `D (1)
  Original pv_ext3: `E (1)
  Mapped PV_ext3 and showed as a pv_ext3: `E (1.)
  $ (cd ../../../../default && ./regression/test804polyvar.exe)
  `A (aaa)
  Just (`A (a))	Just (`C (ccc))
  $ (cd ../../../../default && ./regression/test805std.exe)
  $ (cd ../../../../default && ./regression/test806fmt.exe)
  { a=1; 
   b="x"; }
  
  QQQ (
       "azerty"
       )
  LLL 
  ([ 1; 2; 3]
  )
  
    [ 1.; 2.
    ; 3.; 4.]
  $ (cd ../../../../default && ./regression/test807showT.exe)
  $ (cd ../../../../default && ./regression/test811compare.exe)
  $ (cd ../../../../default && ./regression/test812html.exe)
  $ (cd ../../../../default && ./regression/test813htmlTy.exe)
  $ (cd ../../../../default && ./regression/test815abstr.exe)
  $ (cd ../../../../default && ./regression/test816hash.exe)
  use new value
  use old value
  use new value
  $ (cd ../../../../default && ./regression/test820spec.exe)
  { a=5; b="<opaque>"; }
  $ (cd ../../../../default && ./regression/test821clab.exe)
  C { xxx="asdf"; yyy=1; }
  <ul>C<li>xxx&quot;asdf&quot;</li><li>yyy1</li></ul>
  $ (cd ../../../../default && ./regression/test822.exe)
  C (1)
  $ (cd ../../../../default && ./regression/test823list.exe)
  $ (cd ../../../../default && ./regression/test824mut.exe)
  [ ("gl",
    Lambda { lam_argname=None; lam_api=[]; lam_eff=HEmpty ; lam_body=CInt (5); 
            lam_is_rec=true;
    }); ("gl",
        Lambda { lam_argname=None; lam_api=[]; lam_eff=HEmpty ; 
                lam_body=CInt (5); lam_is_rec=true;
        }); ("gl",
            Lambda { lam_argname=None; lam_api=[]; lam_eff=HEmpty ; 
                    lam_body=CInt (5); lam_is_rec=true;
            }); ("gl",
                Lambda { lam_argname=None; lam_api=[]; lam_eff=HEmpty ; 
                        lam_body=CInt (5); lam_is_rec=true;
                })]
  $ (cd ../../../../default && ./regression/test825tuples.exe)
  $ (cd ../../../../default && ./regression/test826antiph.exe)
  "asdf"
  $ (cd ../../../../default && ./regression/test828combi.exe)
  Should be an ADT:   `A (5)`
  Should be a number: `5`
  Should be a number: `5`
  $ (cd ../../../../default && ./regression/test840garrique.exe)
  17
  2
  17
  2
  $ (cd ../../../../default && ./regression/test798gen.exe)
  Nil
  Cons (2, Nil)
  Cons (2, Cons (2, Nil))
  Nil
  Cons (WTF, Nil)
  Cons (3, Cons (4, Nil))
  Nil
  Cons (6, Nil)
  Cons (7, Cons (8, Nil))
  $ (cd ../../../../default && ./regression/test817logic.exe)
  []
  :: (2, [])
  :: (2, :: (2, []))
  []
  :: (WTF, [])
  :: (3, :: (4, []))
  []
  :: (6, [])
  :: (7, :: (8, []))
  Default logic values
  	Var (5)
  	Value (6)
  Modified logic values
  	Var (5)
  	6
  Modified logic list values
  	[]
  	:: (6, [])
