use
    boolean
end

G: ANY


{: Set order
   ========= :}

all(p:{G}) ensure p <= p end

all(p:{G}) ensure p = p end

all(p,q,r:{G}) require p <= q
                       q <= r
               ensure  p <= r end

all(p,q:{G})   require p <= q
                       q <= p
               ensure  p = q end





{: Proper subset
   ============= :}

(<)  (p,q:{G}): ghost BOOLEAN
    -> p <= q and some(x) x /in p and x in q


all(p,q:{G})
    require
        p < q
    ensure
        q.has_some
    via some(x) x /in p and x in q
        assert
            x in q
    end


all(p,q:{G})
    require
        p < q
    ensure
        p /= q
        via require p = q
        via some(x) x /in p and x in q
    end


all(p,q:{G})
    require
        p <= q
        p /= q
    ensure
        p < q
        via require not some(x) x /in p and x in q  -- a
        assert
            all(x)
                    -- construct contradiction by proving 'q <= p' which
                    -- contradicts 'p /= q'
                require
                    x in q
                ensure
                    x in p
                    assert
                        not (x /in p and x in q)  -- consequence of 'a' by
                                                  -- contrapositive
                        x in p or x /in q
                        ensure
                            x in p
                            if x in p orif x /in q
                        end
                end
    end


all(p,q,r:{G})
    require
        p < q
        q <= r
    ensure
        p < r
    via
        some(x) x /in p and x in q
    end


all(p,q,r:{G})
    require
        p <= q
        q < r
    ensure
        p < r
    via
        some(x) x /in q and x in r
    end


{: Singleton set
   ============= :}


singleton (a:G): {G} -> {x: x = a}

all(x:G, p:{G})
    require
        x in p
    ensure
        {x} <= p
    assert
        all(y) require y in {x}
               ensure  y in p
               assert   x = y end
    end


all(x:G)
    ensure
        {x}.has_some
    assert
        x in {x}
    end


all(p:{G}, x:G)
    require
        p < {x}
    ensure
        p.is_empty
        via require p.has_some
        via some(y) y in p
            assert
                p /= {x}
                y in {x}
                {x} <= p
    end


{: De Morgan's Laws
   ================ :}

all(p:{G})
    require
        all(x) x /in p   -- a
    ensure
        not some(x) x in p

        via require
            some(x) x in p
        via some(x) x in p
                assert   x /in p  -- from 'a'
    end





all(p:{G})
    require
        not some(x) x in p
    ensure
        all(x) x /in p
    end





all(p:{G})
    require
        some(x) x /in p
    ensure
        not all(x) x in p

        via require
            all(x) x in p
        via some(x) x /in p
                assert   x in p
    end








all(p:{G})
    require
        not all(x) x in p   -- a1
    ensure
        some(x) x /in p

        via require
            not some(x) x /in p  -- a2
        assert
            all(x)
                ensure
                    x in p  -- contradicts 'a1'

                    via require
                        not (x in p)   -- a3
                    assert
                        x /in p
                        some(x) x /in p -- witness 'a3', contradicts 'a2'
                end
    end






{: Set algebra
   =========== :}


(+) (p,q:{G}): {G}   -> {x: p(x) or q(x)}

(*) (p,q:{G}): {G}   -> {x: p(x) and q(x)}

(-) (p,q:{G}): {G}   -> {x: x in p and x /in q}

(-) (p:{G}): {G}     -> {x: not p(x)}

disjoint(p,q:{G}): ghost BOOLEAN -> (p*q).is_empty


all(p,q:{G})
    require
        p.has_some
    ensure
        (p + q).has_some
    via some(x)
        x in p
    assert
        x in p + q
    end

all(p,q,r:{G})
    require
        p + q <= r
    ensure
        p <= r
    end

all(p,q,r:{G})
    require
        p + q <= r
    ensure
        q <= r
    end

all(p,q,r:{G})
    require
        p <= r
        q <= r
    ensure
        p + q <= r
    assert
        all(x)
            require
                x in p + q
            ensure
                x in r
            if   x in p
            orif x in q
            end
    end


all(p,q,r:{G})
        -- Associativity of set union
    ensure
        p + q + r <= p + (q + r)
    assert
        all(x)
            require
                x in (p + q + r)
            ensure
                x in (p + (q + r))
            if x in (p + q)
            orif x in r
            end
    end

all(p,q,r:{G})
        -- Associativity of set union
    ensure
        p + (q + r) <= p + q + r
    assert
        all(x)
            require
                x in (p + (q + r))
            ensure
                x in (p + q + r)
            if x in p
            orif x in (q + r)
            end
    end




all(p,q:{G})
    require
        disjoint(p,q)
    ensure
        disjoint(q,p)
        assert
            p*q = q*p
            q*p in {x: x.is_empty}
    end

all(a:G, p,q:{G})
    require
        disjoint(p,q)
        a in p
    ensure
        a /in q
        via require a in q
            assert
               a in (p*q)
    end


all(p,q:{G})
    require
        not disjoint(p,q)
    ensure
        (p*q).has_some
    via
        require not (p*q).has_some
    end


all(p,q:{G})
    require
        q < p
    ensure
        (p - q).has_some
    via
        some(x) x /in q and x in p
        assert
            x in p - q
    end


all(p,q:{G}, x:G)
    require
        x /in p - q
        x in p
    ensure
        x in q
    via require
        not (x in q)
    end


all(p,q:{G})
    require
        not (p <= q)
    ensure
        some(x) x in p - q

    via require
        not some(x) x in p - q          -- a1
    assert
        ensure
            p <= q
        assert
            all(x)
                require
                    x in p
                ensure
                    x in q
                assert
                    all(x) x /in p - q  -- De Morgan of a1
                    x /in p - q
                via require
                    not (x in q)
                end
        end
    end



all(p,q:{G})
        -- Double complement with respect to a base set.
    require
        q <= p
    ensure
        p - (p - q) = q
    end








{: Union and intersection of collections of sets
   ============================================= :}

(+) (ps:{{G}}): ghost {G} -> {x: some(p) p in ps and x in p}

(*) (ps:{{G}}): ghost {G} -> {x: all(p) p in ps ==> x in p}



all(p:{G}, ps:{{G}})
        -- Every set of a collection of sets is a subset of the union
        -- of the collection.
    require
        p in ps
    ensure
        p <= + ps
    assert
        all(x)
            require
                x in p
            ensure
                x in + ps
            assert
                p in ps and x in p
            end
    end


{: Quantifier Transformations
   ========================== :}

A: ANY

all(p:{A}, e:BOOLEAN)
    ensure
        ((all(x) p(x)) or e) = (all(x) p(x) or e)
    assert
        require
             (all(x) p(x)) or e
        ensure
             all(x) p(x) or e
        end

        require
             all(x) p(x) or e
        ensure
             (all(x) p(x)) or e
        if e
        else
            assert
                all(x)
                    ensure
                        p(x)
                    assert
                        e or p(x)
                    end
        end
    end



all(p:{A})
    ensure
        not (some(x) x in p) = (all(x) x /in p)
    end

all(p:{A})
    ensure
        not (all(x) x in p)  = (some(x) x /in p)
    end


all(p:{A}, e:BOOLEAN)
    ensure
        ((some(x) x in p) ==> e)
        =
        (all(x) x in p ==> e)
    end


all(p:{A}, e:BOOLEAN)
    ensure
        (some(x) x in p and e) = ((some(x) x in p) and e)
    assert
        require
            some(x) x in p and e
        ensure
            (some(x) x in p) and e
        via some(x)
            x in p and e
        end

        require
            (some(x) x in p) and e
        ensure
            some(x) x in p and e
        via some(x)
            x in p
        assert
            x in p and e
        end
    end

all(p:{A}, e:BOOLEAN)
    require
         (all(x) p(x)) and e
    ensure
         all(x) p(x) and e
    end


all(p,q:{A})
    ensure
        (some(x) p(x)) or (some(x) q(x)) = (some(x) p(x) or q(x))
    assert
        require
            (some(x) p(x)) or (some(x) q(x))
        ensure
            some(x) p(x) or q(x)
        if some(x) p(x)
            via some(x) p(x)
            assert
                p(x) or q(x)
        orif some(x) q(x)
            via some(x) q(x)
            assert
                p(x) or q(x)
        end
        require
            some(x) p(x) or q(x)
        ensure
            (some(x) p(x)) or (some(x) q(x))
        via some(x)
            p(x) or q(x)
        if p(x)
        orif q(x)
        end
    end
