use
    boolean
    predicate
end

A: ANY
B: ANY


{: General properties
   ================== :}


all(f,g:A->B)
        -- functions with disjoint domains are consistent
    require
        disjoint(f.domain, g.domain)
    ensure
        consistent(f,g)
    assert
        all(x)
            require
                x in f.domain
                x in g.domain
            ensure
                f(x) = g(x)
            assert
                x /in g.domain -- disjoint domains, contradicts assumption
            end
    end


all(p,q:{A}, f:A->B, y:B)
    require
        y in f[p+q]

    ensure
        y in f[p] + f[q]

    via some(x)
        x in p+q and x in f.domain and f(x) = y

    if x in p
        assert
            x in p and x in f.domain and f(x) = y

    orif x in q
        assert
            x in q and x in f.domain and f(x) = y
    end



all(p,q:{A}, f:A->B, y:B)
    require
        y in f[p] + f[q]

    ensure
        y in f[p+q]

    if y in f[p]
        via some(x)
            x in p and x in f.domain and f(x) = y
        assert
             x in p + q and x in f.domain and f(x) = y

    orif y in f[q]
        via some(x)
            x in q and x in f.domain and f(x) = y
        assert
             x in p + q and x in f.domain and f(x) = y
    end




all(p,q:{A}, f:A->B)
    ensure
        f[p+q] = f[p] + f[q]
    end



all(a:A, f:A->B)
    require
        a in f.domain

    ensure
        f[{a}] = {f(a)}

    assert
        all(b)
            require
                b in f[{a}]
            ensure
                b in {f(a)}
            via some(x)
                x in {a} and x in f.domain and f(x) = b
            end

        all(b)
            require
                b in {f(a)}
            ensure
                b in f[{a}]
            assert
                a in {a} and a in f.domain and f(a) = b
            end
    end




{: Override
   ======== :}


(+) (f:A->B,e:(A,B)): (A->B)
    -> agent (a:A): B
           require
               a = e.first or a in f.domain
           ensure
               -> if a = e.first then
                      e.second
                  else
                      f(a)
           end


(+) (f,g:A->B): ghost (A->B)
    -> agent (a:A): B
           require
               a in (g.domain + f.domain)
           ensure
               -> if a in g.domain then
                      g(a)
                  else
                      f(a)
           end


all(f:A->B, a:A, b:B)
    ensure
        (f + (a,b))(a) = b
    assert
        a = (a,b).first
    end




all(f:A->B, a,x:A, b:B)
    require
        x in f.domain
        x /= a
    ensure
        (f + (a,b))(x) = f(x)
    assert
        x /= (a,b).first
    end



all(x:A,f,g:A->B)
    require
        x in g.domain
    ensure
        (f + g)(x) = g(x)
    end



all(x:A,f,g:A->B)
    require
        consistent(f,g)
        x in f.domain
    ensure
        (f + g)(x) = f(x)
    if x in g.domain
    else
    end


all(f,g:A->B)
    ensure
        (f + g).domain <= (g + f).domain
    end


all(f,g:A->B)
    require
        consistent(f,g)

    ensure
        consistent(f + g, g + f)

    assert
        all(x)
            require
                x in (f + g).domain
                x in (g + f).domain
            ensure
                (f + g)(x) = (g + f)(x)
            if x in f.domain
                assert
                    (f + g)(x) = f(x)
            orif x in g.domain
                assert
                    (g + f)(x) = g(x)
            end
    end


all(f,g:A->B)
    require
        consistent(f,g)
    ensure
        f + g = g + f
    end






{: Order Structure
   =============== :}


(<=) (f,g:A->B): ghost BOOLEAN
    -> f.domain <= g.domain
       and
       consistent(f,g)

(<) (f,g:A->B): ghost BOOLEAN
    -> f <= g
       and
       some(a)
           a in g.domain and
           a /in f.domain



all(f:A->B)
        -- reflexivity of '<='
    ensure
        f <= f
    end

all(f,g:A->B)
        -- antisymmetry of '<='
    ensure
        f <= g  ==>  g <= f  ==> f = g
    end


all(f,g,h:A->B)
        -- transitivity of '<='
    require
        f <= g
        g <= h
    ensure
        f <= h
    assert
        all(x)
            require
                x in f.domain
            ensure
                f(x) = h(x)
            assert
                f(x) = g(x)
                g(x) = h(x)
            end
    end





all(f,g:A->B)
    require
        f < g
    ensure
        f /= g
    via require
        g <= f
    via some(a)
        a in g.domain and a /in f.domain
    end


all(f,g:A->B)
    require
        f <= g
        f /= g
    ensure
        f < g
    via require
        not some(x) x in g.domain and x /in f.domain
    assert
        all(x)
            require
                x in g.domain
            ensure
                x in f.domain
            assert
                not (x in g.domain and x/in f.domain)
                x /in g.domain or x in f.domain
            end
    end



all(f,g:A->B)
    ensure
        g <= f + g
    end

all(f,g:A->B)
    require
        consistent(f,g)
    ensure
        f <= f + g
    assert
        f.domain <= (f+g).domain
        all(x)
            require
                x in f.domain
            ensure
                f(x) = (f + g)(x)
            if x in g.domain
                assert
                    g(x) = (f + g)(x)
            orif x /in g.domain
            end
    end



all(f,g:A->B)
    require
        consistent(f,g)
    ensure
        some(h) f <= h  and g <= h
    assert
        f <= f + g and g <= f + g
    end



all(f,g,h:A->B)
        -- Two subfunctions of the same function are consistent.
    require
        f <= h
        g <= h
    ensure
        consistent(f,g)
    assert
        all(x)
            require
                x in f.domain
                x in g.domain
            ensure
                f(x) = g(x)
            assert
                f(x) = h(x)
                h(x) = g(x)
            end
    end



all(f,g:A->B)
    require
        f <= g
    ensure
        f.range <= g.range
    assert
        all(y)
            require
                y in f.range
            ensure
                y in g.range
            via some(x)
                x in f.domain and f(x) = y
            assert
                f(x) = g(x)   -- consistent functions
                x in g.domain and g(x) = y
            end
    end



{: Domain restriction
   ================== :}


(|) (f:A->B, p:{A}): (A->B)
    -> agent (a:A):B
           require
               a in f.domain
               a in p
           ensure
               -> f(a)
           end

(-) (f:A->B, a:A): (A->B)
        -- The function 'f' with the element 'a' removed from its domain.
    -> agent (x)
           require
               x in f.domain
               x /= a
           ensure
               -> f(x)
           end


all(f:A->B, p:{A})
    ensure
        (f|p).range <= f.range
    assert
        all(y)
            require
                y in (f|p).range
            ensure
                y in f.range
            via some(x)
                x in (f|p).domain and (f|p)(x) = y
            assert
                x in f.domain and f(x) = y
            end
    end


all(f:A->B, a,b:A)
    ensure
        f - a - b = f - b - a
    end



all(f:A->B, a:A)
    require
        a in f.domain
    ensure
        f = f - a + (a,f(a))
    assert
        -- Proof needs to many explicit case splits. Case splits should be
        -- triggered automatically by a conditional expression of a nonrecursive
        -- function !!!
        all(x)
            require
                x in f.domain
            ensure
                x in (f - a + (a,f(a))).domain
            if x = a
            else
            end
        all(x)
            require
                x in (f - a + (a,f(a))).domain
            ensure
                x in f.domain
            if x = a
            else
                assert
                    x in (f - a).domain
            end
        all(x)
            require
                x in f.domain
                x in (f - a + (a,f(a))).domain
            ensure
                f(x) = (f - a + (a,f(a)))(x)
            if x = a
            else
                via [(f - a)(x)]
            end
    end






{: Injectivity
   ===========

   A function is injective if it is one to one. I.e. each value in the range of
   the function has a unique origin.

:}

is_injective (f:A->B): ghost BOOLEAN
    -> all(x,y) x in f.domain
                ==> y in f.domain
                ==> f(x) = f(y)
                ==> x = y


(>->) (p:{A}, q:{B}): ghost BOOLEAN
    -> some(f:A->B)
           f.is_injective and
           f.domain = p and
           f.range <= q

(<->) (p:{A}, q:{B}): ghost BOOLEAN
    -> some(f:A->B)
           f.is_injective and
           f.domain = p and
           f.range  = q


all(f,g:A->B)
    require
        g.is_injective
        f <= g
    ensure
        f.is_injective
    assert
        all(x,y)
            require
                x in f.domain
                y in f.domain
                f(x) = f(y)
            ensure
                x = y
            assert
                g(x) = f(x)
                g(x) = g(y)
            end
    end



all(f:A->B, p:{A})
    require
        f.is_injective
    ensure
        (f|p).is_injective
    end


all(f,g:A->B, x,y:A)
    require
        f.is_injective
        g.is_injective
        disjoint(f.domain,g.domain)
        disjoint(f.range, g.range)
        x in (f + g).domain
        y in (f + g).domain
        (f + g)(x) = (f + g)(y)
        x in f.domain

    ensure
        x = y

    assert
        f <= f + g              -- disjoint domains
        f(x) = (f + g)(x)
        (f + g)(x) in f.range   -- because 'f(x) in f.range'

    if y in f.domain
        assert
            f(y) = (f + g)(y)       -- because 'f <= f + g'
            f(x) = f(y)
            x = y                   -- because 'f.is_injective'

    orif y in g.domain
        assert
            g <= f + g              -- always
            g(y) = (f + g)(y)
            (f + g)(y) in g.range   -- 'g(y)' in 'g.range'
            (f + g)(y) in f.range   -- because '(f + g)(x) = (f + g)(y)'
                                    -- and '(f + g)(x) in f.range'
            (f + g)(y) /in g.range  -- ranges of 'f' and 'g' are disjoint
    end




all(f,g:A->B, x,y:A)
    require
        f.is_injective
        g.is_injective
        disjoint(f.domain,g.domain)
        disjoint(f.range, g.range)
        x in (f + g).domain
        y in (f + g).domain
        (f + g)(x) = (f + g)(y)
        x in g.domain
    ensure
        x = y
    assert
        -- same situation as the previous theorem with 'f' and 'g' flipped
        disjoint(g.domain,f.domain)
        disjoint(g.range, f.range)
        x in (g + f).domain
        y in (g + f).domain
        f + g = g + f
        {h: x in h.domain ==> y in h.domain ==> h(x) = h(y)}(g + f)
    end


all(f,g:A->B)
    require
        f.is_injective
        g.is_injective
        disjoint(f.domain,g.domain)
        disjoint(f.range, g.range)
    ensure
        (f + g).is_injective
    assert
        all(x,y)
            require
                x in (f + g).domain
                y in (f + g).domain
                (f + g)(x) = (f + g)(y)
            ensure
                x = y
                -- use the last two previous theorems
            if x in f.domain
            orif x in g.domain
            end
    end


{: The inverse of a function
   ========================= :}


origin(b:B, f:A->B): ghost A
    require
        f.is_injective
        b in f.range
    ensure
        Result in f.domain
        f(Result) = b
    end



inverse0 (f:A->B): ghost (B -> A)
        -- Helper function
    require
        f.is_injective
    ensure
        -> agent(b:B): A
               require
                   b in f.range
               ensure
                   -> b.origin(f)
               end
    end


all(f:A->B)
    require
        f.is_injective
    ensure
        (f.inverse0).domain = f.range
    end





all(f:A->B)
        -- Existence of an inverse of an injective function.
    require
        f.is_injective
    ensure
        some(g) g.domain = f.range
                and
                all(x) x in f.domain ==> g(f(x)) = x
    assert
        f.inverse0.domain = f.range
        and
        all(x) x in f.domain ==> (f.inverse0)(f(x)) = x
    end



all(f:A->B, g,h:B->A)
        -- Uniqueness of the inverse of an injective function.
    require
        f.is_injective
        g.domain = f.range
        h.domain = f.range
        all(x) x in f.domain ==> g(f(x)) = x
        all(x) x in f.domain ==> h(f(x)) = x
    ensure
        g = h
    assert
        g.domain = h.domain
        all(y)
            require
                y in f.range
            ensure
                g(y) = h(y)
            via some(x)
                x in f.domain and f(x) = y
            via [ g(y)
                , g(f(x))
                , x
                , h(f(x))
                , h(y)
                ]
            end
    end




inverse (f:A->B): ghost (B -> A)
    require
        f.is_injective
    ensure
        Result.domain = f.range
        all(x) x in f.domain ==> Result(f(x)) = x
    end


all(f,g:A->B, y:B)
    require
        f <= g
        y in f.range
    ensure
        y in g.range
        assert
            f.range <= g.range
    end


all(f,g:A->B, y:B)
    require
        g.is_injective
        f <= g
        y in f.range
    ensure
        y.origin(f) = y.origin(g)

    via some(x)
        x in f.domain and f(x) = y
    assert
        f(x) = g(x)           -- consistent functions
        g(x).origin(g) = x    -- def 'origin'
        f.is_injective        -- g.is_injective and f <= g
    via [ f(x).origin(f)
        , x
        , g(x).origin(g)
        ]
    end




{:# Fixpoints
:}


fixpoints(f:A->A): ghost {A}
        -- The fixpoints of the function 'f'.
    -> {x: x in f.domain and f(x) = x}


is_idempotent(f:A->A): ghost BOOLEAN
        -- Is the function 'f' idempotent i.e. is f(x) = f(f(x)) valid?
    -> all(x) x in f.domain ==> f(x) in f.fixpoints


{:# Choice Function
:}


is_choice(f:{A}->A, p:{A}): ghost BOOLEAN
        -- Is 'f' a choice function for the set 'p'?
    -> (all(q) q.has_some ==> q <= p ==> q in f.domain)
       and
       (all(q) q.has_some ==> q <= p ==> f(q) in q)
