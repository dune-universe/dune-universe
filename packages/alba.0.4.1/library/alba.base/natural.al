use
    boolean
    predicate
end


class
    NATURAL
create
    0
    successor (predecessor:NATURAL)
end

1: NATURAL = 0.successor
2: NATURAL = 1.successor
3: NATURAL = 2.successor
4: NATURAL = 3.successor
5: NATURAL = 4.successor
6: NATURAL = 5.successor
7: NATURAL = 6.successor
8: NATURAL = 7.successor
9: NATURAL = 8.successor


{: Successor
   ========= :}

all(a:NATURAL)
    require
        some(x) a = successor(x)
    ensure
        a /= 0
    assert
        all(x)
            require
                a = successor(x)
            ensure
                a /= 0
            assert
                successor(x) = a
                a in {a: a /= 0}
            end
    end



all(a:NATURAL)
    require
        a /= 0
    ensure
        some(x) a = successor(x)

        inspect a
        case successor(a)
        assert
            successor(a) = successor(a)
    end



predecessor (n:NATURAL): NATURAL
    require
        n as _.successor
    ensure
        -> inspect n
           case    m.successor then m
    end





{: Addition
   ======== :}


(+) (a,b: NATURAL): NATURAL
    -> inspect b
       case 0           then a
       case n.successor then (a + n).successor


all(a:NATURAL)
    ensure
        a + 0 = a
    end



all(a,b:NATURAL)
    ensure
        a + b.successor = (a + b).successor
    end



all(a:NATURAL)
    ensure
        a + 1 = a.successor
    end



all(a,b,c:NATURAL)
    ensure
        a + b + c  =  a + (b + c)
    inspect c
    end


all(n:NATURAL)
    ensure
        0 + n = n
    inspect
        n
    end


all(a,b:NATURAL) -- commutativity of successor
    ensure
        a + b.successor = a.successor + b
    inspect
        b
    end




all(a,b:NATURAL)
        -- commutativity of addition
    ensure
        a + b = b + a
    inspect
        b
    case b.successor
        via [  a + b.successor
            , (a + b).successor  -- def '+'
            ,  (b + a).successor  -- ind hypo
            ,  b + a.successor    -- def '+'
            ,  b.successor + a    -- commutativity of successor
            ]
    end



all(a,b,x:NATURAL)
        -- right cancellation
    ensure
        a + x = b + x ==> a = b
    inspect
        x
    end





all(a,b,x:NATURAL)
        -- left cancellation
    require
        x + a = x + b
    ensure
        a = b
    assert
        ensure a + x = b + x
               via [x + a]
        end
    end




all(a:NATURAL)
    ensure
        a = a.successor ==> false
    inspect
        a
    end



all(a,x:NATURAL)
    require
        a + x = a
    ensure
        x = 0
    assert
        ensure
            x + a = 0 + a  -- by right cancellation proves the goal
        via [ a + x,
              a ]
        end
    end



all(a,b:NATURAL)
    require
        a + b = 0
    ensure
        a = 0
    inspect
        a
    case a.successor
        assert
            a.successor + b = 0

            ensure
                (a + b).successor = 0
            via [ a + b.successor,
                  a.successor + b ]
            end

            false
    end







all(a,b:NATURAL)
    require
        a + b = 0
    ensure
        b = 0
    assert
        b + a = a + b
        b + a = 0
    end






{: Order structure
   =============== :}

(<=) (a,b:NATURAL): BOOLEAN
    -> inspect
           a, b
       case 0,_ then
           true
       case successor(a), successor(b) then
           a <= b
       case _, _ then
           false

(<)  (a,b:NATURAL): BOOLEAN -> a <= b and a /= b


all(a:NATURAL)
    ensure
        a <= a
    inspect
        a
    end


all(a:NATURAL)
    require
        a <= 0
    ensure
        a = 0
    inspect
        a
    end

all(a:NATURAL)
    require
        a <= 0
        a as successor(_)
    ensure
        false
    assert
        a = 0
    end






all(a,b:NATURAL)
    require
        a <= b
    ensure
        a < b.successor

    inspect
        a
    case n.successor
        inspect
            b
        case m.successor
            assert
                n < m.successor
    end




all(a,b:NATURAL)
    require
        a <= b
    ensure
        a < b + 1
    assert
        a < b.successor
    end


all(a,b:NATURAL)
    require
        a.successor <= b
    ensure
        a < b
    inspect
        b
    end


all(a,b:NATURAL)
    require
        a + 1 <= b
    ensure
        a < b
    end



all(a,b:NATURAL)
    require
        a <= b
    ensure
        some(x) a + x = b
    inspect
        b
    case 0
        assert
            0 = a
            a + 0 = 0
    case successor(b)
        inspect
            a
        case 0
            assert
                0 + b.successor = b.successor
        case a.successor
            assert
                a <= b
            via some(x)
                a + x = b
            assert
                ensure
                    a.successor + x = b.successor
                via
                    [a + x.successor,
                     (a + x).successor]
                end
    end



all(a,b,x:NATURAL)
    require
        a + x = b
    ensure
        a <= b
    inspect
        b
    case b.successor
        assert
            all(a,x) a + x = b ==> a <= b  -- ind hypo
            -- goal: a + x = b.successor ==> a <= b.successor
        inspect
            a
        case a.successor
            assert
                all(x) a + x = b.successor ==> a <= b.successor  -- ind hypo
                a.successor + x = b.successor

                ensure
                    (a + x).successor = b.successor
                via [a + x.successor,
                     a.successor + x]
                end

                a + x = b
                a + x = b ==> a <= b    -- from outer ind hypo
                a <= b

                a.successor <= b.successor  -- goal
    end





all(a,b:NATURAL)
    require
        a <= b
        b <= a
    ensure
        a = b
    via some(x)
        a + x = b
    via some(y)
        b + y = a
    assert
        a + (x + y) = (a + x) + y
        a + x + y = b + y

        a + (x + y) = a

        x + y = 0
        x = 0
        0 in {x: a + x = b}
    end




all(a,b,c:NATURAL)
    require
        a <= b
        b <= c
    ensure
        a <= c
    via some(x)
        a + x = b
    via some(y)
        b + y = c
    assert
        a + (x + y) = c
        some(z) a + z = c
    end





all(a:NATURAL)
    ensure
        a <= a.successor
    inspect
        a
    end



all(a:NATURAL)
    ensure
        a < a.successor
    end


all(a:NATURAL)
    ensure
        a < a + 1
    end



all(a,b:NATURAL)
    require
        a < b
    ensure
        a.successor <= b
    via some(x)
        a + x = b
    assert
        ensure
            x /= 0
        via require
            x = 0
        assert
            0 = x
            x in {x: a = a + x}
            a = b
        end
    via some(y)
        x = y.successor
    assert
        y.successor in {x: a + x = b}
        a + y.successor = b
        ensure
            a.successor + y = b
        via [ a + y.successor ]
        end
    end




all(a,b:NATURAL)
    require
        a < b
    ensure
        a + 1 <= b
    end




all(a,b:NATURAL)
    require
        a < b + 1
    ensure
        a <= b
    assert
        a + 1 <= b + 1
    end


all(a,b:NATURAL)
    ensure
        a <= b or b <= a
    inspect
        a
    case successor(a)
        if a <= b
            if a = b
                assert
                    a <= a.successor
                    b in {x: x <= a.successor}
                    b <= a.successor
            orif a /= b
                assert
                    a < b
                    a.successor <= b
        orif b <= a
    end




all(a,b:NATURAL)
    require
        not (a <= b)
    ensure
        b <= a
    assert
        a <= b or b <= a
    end



all(a,b:NATURAL)
    ensure
        a <= b or b < a
    if a = b
        assert
            b in {x: a <= x}
    orif a /= b
        if a <= b
        orif b <= a
            assert
                b /= a
    end




all(a,b,n:NATURAL)
    require
        a <= b
    ensure
        a + n <= b + n
    inspect
        n
    end



all(a,b,n:NATURAL)
    require
        a + n <= b + n
    ensure
        a <= b
    inspect
        n
    end



all(a,b:NATURAL)
    ensure
        a <= a + b
    inspect
        b
    end


all(a,b:NATURAL)
    ensure
        b <= a + b
    inspect
        b
    end


all(a,b:NATURAL)
    require
        not (a < b)
        not (a = b)
    ensure
        b < a
    assert
        b <= a
    end

all(a,b:NATURAL)
    require
        not (a < b)
        not (a = b)
    ensure
        0 < a
    end


all(a,b:NATURAL)
    require
        a < b
    ensure
        1 <= b
    inspect
        b
    end




{: Wellfounded Relation
   ==================== :}

all(p:{NATURAL},y:NATURAL)
        -- '<'  is a wellfounded relation
    require
        all(y) (all(x) x < y ==> x in p) ==> y in p
    ensure
        y in p
    assert
        all(p:{NATURAL},x,y:NATURAL)
            require
                all(y) (all(x) x < y ==> x in p) ==> y in p
                x <= y
            ensure
                x in p
            inspect
                y
            case 0
                assert
                    x = 0
            end
    end



{: Wellorder
   ========= :}

is_least(a:NATURAL, p:{NATURAL}): ghost BOOLEAN
    -> a in p and (all(x) x in p ==> a <= x)

all(a:NATURAL, p:{NATURAL})
    require
        a in p
        not a.is_least(p)
    ensure
        some(x) x in p and x < a
    assert
        not (all(x) x in p ==> a <= x)  -- because 'a in p'
        ensure
           not (all(x) x in {x: x in p ==> a <= x})
           {: This complicated detour is necessary because De Morgan on
              quantifiers does currently work only with expressions of
              the form 'x in p' :}
        via require all(x) x in {x: x in p ==> a <= x}
        assert
            all(x)
                require
                    x in p
                ensure
                    a <= x
                assert
                    x in {x: x in p ==> a <= x}
                end
        end
    via some(x) x /in {x: x in p ==> a <= x}
    assert
        not (x in p ==> a <= x)
        x in p and x < a
    end


all(a,b:NATURAL, p:{NATURAL})
    require
        a.is_least(p)
        b.is_least(p)
    ensure
        a = b
    end


all(p:{NATURAL})
    require
        p.has_some
    ensure
        some(x) x.is_least(p)

    assert
        all(a:NATURAL, p:{NATURAL})
            require
                some(x) x <= a and x in p {: Weaker assumption gives a stronger
                                             induction hypothesis :}
            ensure
                some(x) x.is_least(p)
            inspect
                a
            case 0
                via some(x) x <= 0 and x in p
                assert
                    x.is_least(p)
            case n.successor
            {: ind hypo: all(p) (some(x) x <= n and x in p) ==>
                                (some(x) x.is_least(p))
               ass:  some(x) x <= n.successor and x in p
            :}
                if n.successor.is_least(p)
                else
                    via some(x) x <= n.successor and x in p
                    if n.successor in p
                        via some(k) k in p and k < n.successor
                        assert
                            k <= n and k in p
                    else
                        assert
                            x < n.successor
                            x <= n and x in p
            end

    via some(a) a in p
    assert
        a <= a and a in p
        some(x) x <= a and x in p
    end


least(p:{NATURAL}): ghost NATURAL
    require
        p.has_some
    ensure
        Result.is_least(p)
    end




{: Difference
   ========== :}




(-)  (a,b:NATURAL): NATURAL
    require
        b <= a
    ensure
        -> inspect
               a,b
           case _, 0 then
               a
           case successor(a), successor(b) then
               a - b
    end



all(n,m:NATURAL)
    require
        m <= n
    ensure
        n - m + m = n
    inspect
        m
    case l.successor
        inspect
            n
        case k.successor
        assert
            k - l + l = k
        via [ (k.successor - l.successor) + l.successor
            , (k - l + l).successor
            , k.successor
            ]
    end


all(a,b,n:NATURAL)
    require
        n <= a
        a - n = b
    ensure
        a = b + n
    end

all(a,b,n:NATURAL)
    require
        n <= a
        a = b + n
    ensure
        a - n = b
    inspect
        n
    end

all(n:NATURAL)
    ensure
        n - n = 0
    inspect
        n
    end


all(a,b,c:NATURAL)
    require
        c <= a
        a - c <= b
    ensure
        a <= b + c
    inspect
        c
    case m.successor
        inspect
            a
    end



all(a,b:NATURAL)
    require
        b <= a
    ensure
        (a - b).successor = a.successor - b
    inspect
        a
    case 0
        assert
            b = 0
    case k.successor
        inspect
            b
        case m.successor
    end

all(a,b,c:NATURAL)
    require
        c <= a
    ensure
        a - c + b = a + b - c
    inspect
        b
    case b.successor
        assert
            a - c + b = a + b - c
        via [ (a - c + b).successor
            , (a + b - c).successor
            , (a + b).successor - c
            , a + b.successor - c
            ]
    end

all(a,b:NATURAL)
    require
        a /= 0
        a <= b
    ensure
        a - 1 < b
    assert
        1 <= a
        a = a - 1 + 1
        a < b + 1;  {: therefore :} a <= b + 1
        a - 1 + 1 in {a: a <= b + 1}
        a - 1 + 1 <= b + 1
    end




{: Multiplication
   ============== :}

(*) (a,b:NATURAL): NATURAL
    -> inspect a
       case 0           then 0
       case n.successor then n*b + b


all(a:NATURAL)
    ensure
        0 * a = 0
    end

all(a,b:NATURAL)
    ensure
        a.successor * b = a*b + b
    end



all(a:NATURAL)
    ensure
       a * 0 = 0
    inspect a end

all(a:NATURAL)
    ensure
        1 * a = a
    assert
        0 + a = a
    end


all(a,b,c:NATURAL) -- distributivity
    ensure
        a * (b + c) = a*b + a*c
    assert
        all(a,b,c,d:NATURAL)  -- lemma
            {: Note: This lemma is needed as long as the special treatment of
                     commutative and associative operators is not yet implemented :}
            ensure
                a + b + (c + d) = a + c + (b + d)
            assert
                b + (c + d) = b + c + d
                b + c = c + b
                c + b + d = c + (b + d)
            via [ a + b + (c + d)
                , a + (b + (c + d))
                , a + (b + c + d)
                , a + (c + b + d)
                , a + (c + (b + d))
                , a + c + (b + d)
                ]
            end
    inspect
        a
    case successor(a)
        via [ a.successor * (b + c)
            , a*(b + c) + (b + c)
            , a*b + a*c + (b + c)
            , a*b + b + (a*c + c)
            , a.successor*b + a.successor*c
            ]
    end


{: Exponentiation
   ============== :}

(^) (a,b:NATURAL): NATURAL
    -> inspect b
       case 0           then 1
       case n.successor then a^n * a

all(a:NATURAL)
    ensure
        a^0 = 1
    end

all(a,b:NATURAL)
    ensure
        a ^ b.successor = a^b * a
    end



all ensure
    1:NATURAL + 1 = 2
end

all ensure
    1:NATURAL + 2 = 3
end

all ensure
    (1:NATURAL) * 2 = 2
end

all ensure
    2:NATURAL * 2 = 4
end

all ensure
    2:NATURAL ^ 2 = 4
end




{: Division
   ======== :}


(/) (n,d:NATURAL): NATURAL
    require
        d /= 0
    ensure
        -> inspect
               n
           case 0 then
               0
           case successor(m) then
               if n = (m / d + 1) * d then
                   m / d + 1
               else
                   m / d
    end



all(n,d:NATURAL)
    require
        d /= 0
    ensure
        n / d * d <= n
    inspect
        n
    case successor(m)
        -- goal: successor(m)/d*d <= successor(m)
        assert
            all(d) d /= 0 ==> m/d*d <= m -- ind. hypo
        if successor(m) = (m / d + 1) * d
            assert
                successor(m)/d = m/d + 1
                ensure
                    successor(m)/d*d = successor(m)
                via [ successor(m)/d*d
                    , (m/d + 1)*d        -- def '/'
                    , successor(m)       -- 'if'
                    ]
                end
        else
            assert
                m < successor(m)
            via [ successor(m)/d*d
                , m/d*d              -- def '/'
                , m                  -- ind hypo
                , successor(m)
                ]
    end


(mod) (n,d:NATURAL): NATURAL
    require
        d /= 0
    ensure
        -> n - n / d * d
    end
