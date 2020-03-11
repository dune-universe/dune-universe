use
    boolean
    predicate
    natural
end

A:ANY
G:ANY
H:ANY


all(x:G,a:[G])
        -- Inversion
    ensure
       []  = x^a  ==>  false
    end

all(x:G,a:[G])
        -- Inversion
    ensure
       x^a = []   ==>  false
    end


all(a:[G])
    require
        a as (_ ^ _ ^ _)
        a as (_ ^ [] )
    ensure
        false

        via some(x,y,b) a = x^y^b
        via some(z) a = [z]
    end


head (a:[G]): G
    require
        a as x ^ t
    ensure
        -> inspect a
           case h ^ _ then h
    end



tail (a:[G]): [G]
    require
        a as x ^ t
    ensure
        -> inspect a
           case _ ^ t then t
    end


all(x,y:G, a,b:[G])
        -- Injection
    require
        x^a = y^b
    ensure
        x = y
    assert
        y^b in {l: l as _^_ and l.head = x and l.tail = a}
    end


all(x,y:G, a,b:[G])
        -- Injection
    require
        x^a = y^b
    ensure
        x = y
    end



{: List Length
   =========== :}


size (a:[G]): NATURAL
    -> inspect a
       case []  then 0
       case h^t then t.size.successor


length (a:[A]): NATURAL
    -> inspect
           a
       case [] then
           0
       case x ^ xs then
           xs.length + 1


all(a:[A])
    require
        a as _ ^ _
    ensure
        0 < a.length
    via some(x,xs) a = x ^ xs
    end

all(a:[A])
    require
        a as _ ^ _
    ensure
        1 <= a.length
    assert
        0 + 1 <= a.length
    end

all(a:[A])
        {: This proof is needed as long as advanced as-expression handling is
           not yet implemented :}
    require
        a as [_]
    ensure
        a as _ ^ _
    via some(x) a = [x]
    end



{: Element Access
   ============== :}

[] (a:[A], i:NATURAL): A
        -- The ith element of the list 'a'.
    require
        i < a.length
    ensure
        -> inspect
               a
           case x ^ xs then
               inspect
                   i
               case 0 then
                   x
               case j.successor then
                   xs[j]
    end


{: List Content
   ============ :}


(in) (x:G, a:[G]): BOOLEAN
    -> inspect a
       case []  then false
       case h^t then x = h  or  x in t

elements (l:[G]): {G} -> {x: x in l}






{: Permutation
   =========== :}

permutation: ghost {[G],[G]}
   = {(r):
           r([],[])                                  -- empty list
           ,
           all(x,a,b) r(a,b) ==> r(x ^ a, x ^ b)     -- prefix element
           ,
           all(x,y,a) r(x ^ y ^ a, y ^ x ^ a)        -- swap adjacent
           ,
           all(a,b,c) r(a,b) ==> r(b,c) ==> r(a,c)   -- transitive
     }


all(a:[G])
    ensure
        permutation(a, a)
    inspect
        a
    end


all(a,b:[G])
    require
        permutation(a,b)
    ensure
        permutation(b,a)
    inspect
        permutation(a,b)
    end





{: Prefix
   ====== :}
is_prefix (a,b:[G]): BOOLEAN
    -> inspect a, b
       case [] , _   then true
       case _  , []  then false
       case x^a, y^b then x = y and a.is_prefix(b)





{: List concatenation
   ================== :}


(+) (a,b: [G]): [G]
    -> inspect a
       case []    then b
       case h ^ t then h ^ (t + b)

all(a:[G])
    ensure
        a + []  = a
    inspect a end


all(a,b,c:[G])
    ensure
        (a + b) + c = a + (b + c)
    inspect a end





{: List reversal
   ============= :}

(-) (a:[G]): [G]
    -> inspect
           a
       case [] then
           a
       case h ^ t then
           -t + [h]

all(a,b:[G])
    ensure
        - (a + b) = -b + -a
    inspect
        a
    case x ^ xs
        -- goal -(x ^ xs + b) = -b + - x ^ xs
        via [ -(x ^ xs + b)
            , - x ^ (xs + b)           -- def '+'
            , - (xs + b) + [x]         -- def '-'
            , -b + - xs + [x]          -- ind hypo
            , -b + (- xs + [x])        -- assoc
            , -b + - x ^ xs            -- def '-'
            ]
    end


all(a:[G])
       -- list reversal is an involution
    ensure
        - - a = a
    inspect
        a
    case x ^ xs
        -- goal: - - x ^ xs = x ^ xs
        via [ - - x ^ xs
            , - (-xs + [x])       -- def '-'
            , -[x] + - - xs       -- - (a + b) = - b + - a
            , -[x] + xs           -- ind hyp
            , x ^ xs              -- def '+'
            ]
    end


all(x:G, a:[G])
    ensure
        x ^ (-a) = - (a + [x])
    assert
        - - a = a

        x ^ (-a)            = - - x ^ (-a)
    end





{: List folding
   ============ :}

folded (f:(H,G)->H, b:H, l:[G]): H
    require
        f.is_total
    ensure
        -> inspect
               l
           case [] then
               b
           case h ^ t then
               f.folded(f(b,h),t)
    end

all(a,b:[G])
    ensure
        -a + b = ((a,x) -> x ^ a).folded(b,a)

    assert
        ensure
            all(b) -a + b = ((a,x) -> x ^ a).folded(b,a)
        inspect
            a
        case x ^ a
        assert
            all(b) -a + b = ((a,x) -> x ^ a).folded(b,a)  -- ind hypo

            all(b)
                ensure
                    - x ^ a + b = ((a,x) -> x ^ a).folded(b,x^a)
                via [ - x ^ a + b
                    , - a + [x] + b                       -- def '-'
                    , - a + ([x] + b)                     -- assoc of '+'
                    , - a + x ^ b                         -- def '+'
                    , ((a,x) -> x ^ a).folded(x ^ b, a)   -- use of ind hypo
                    , ((a,x) -> x ^ a).folded(b, x ^ a)   -- def folded
                    ]
                end
        end
    end




all(a,b:[G])
    ensure
        a + b = ((a,x) -> x ^ a).folded(b,-a)
    assert
        ensure a + b = -(-a) + b
        assert -(-a) = a
        end

        (-(-a)) + b = ((a,x) -> x ^ a).folded(b,-a)
    end




{:
# Mapping of Lists
:}

[] (f:G->H, a:[G]): [H]
        -- The list 'a' where all elements are mapped by 'f'.
    require
        a.elements <= f.domain
    ensure
        -> inspect
               a
           case [] then
               []
           case h ^ t then
               f(h) ^  f[t]
    end
