use
    predicate
    relation
end

A: ANY
B: ANY

{: Basics
   ====== :}


carrier (r:{A,A}): ghost {A} -> domain(r) + range(r)

identity: {A,A}
        -- The identity relation.
    = {x,y: x = y}

diagonal(p:{A}): {A,A}
        -- The identity relation restricted to the set 'p'
    -> {x,y: x = y and x in p}

is_total(f:A->B, r:{A,A}): ghost BOOLEAN
        -- Is 'f' total on the carrier of 'r'?
    -> r.carrier <= f.domain

is_antisymmetric(r:{A,A}): ghost BOOLEAN
        -- Is the relation 'r' antisymmetric?
    -> all(a,b) r(a,b) ==> r(b,a) ==> a = b

is_dichotomic(r:{A,A}): ghost BOOLEAN
        -- Is the relation 'r' dichotomic i.e. for all pairs of the carrier either
        -- the first one relates to the second or vice versa?
    -> all(a,b) {a,b} <= r.carrier ==> r(a,b) or r(b,a)

all(r:{A,A})
    ensure
        r.carrier <= r.inverse.carrier
    assert
        all(x)
            require
                x in r.carrier
            ensure
                x in r.inverse.carrier
            if x in r.domain
                assert
                   r.domain = r.inverse.range
                   x in r.inverse.range
            orif x in r.range
                assert
                   r.range = r.inverse.domain
                   x in r.inverse.domain
            end
    end

all(r:{A,A})
    ensure
        r.inverse.carrier <= r.carrier
    assert
        all(x)
            require
                x in r.inverse.carrier
            ensure
                x in r.carrier
            if x in r.inverse.domain
                assert
                   r.inverse.domain = r.range
                   x in r.range
            orif x in r.inverse.range
                assert
                   r.inverse.range = r.domain
                   x in r.domain
            end
    end

all(r,s:{A,A})
        -- The carrier of an intersection of two relations is a subset of the
        -- carrier of the first relation
    ensure
        (r*s).carrier <= r.carrier
    assert
        all(a)
            require
                a in (r*s).carrier
            ensure
                a in r.carrier
            assert
                (r*s).domain <= r.domain
                (r*s).range  <= r.range
                a in (r*s).domain or a in (r*s).range
                if a in (r*s).domain
                orif a in (r*s).range
            end
    end



all(rs:{{A,A}}, a:A)
    require
         a in (+ rs).carrier

    ensure
         some(r) r in rs and a in r.carrier

    if a in (+ rs).domain
        via some(b) (a,b) in + rs
        via some(r) r in rs and r(a,b)
        assert
            r in rs and a in r.carrier
    orif a in (+ rs).range
        via some(b) (b,a) in + rs
        via some(r) r in rs and r(b,a)
        assert
            r in rs and a in r.carrier
    end


all(r,s:{A,A})
    require
        r <= s

    ensure
        r.carrier <= s.carrier

    assert
        all(x)
            require
                x in r.carrier
            ensure
                x in s.carrier
            if x in r.domain
                via some(y)
                    r(x,y)
                assert
                    s(x,y)
            orif x in r.range
                via some(y)
                    r(y,x)
                assert
                    s(y,x)
            end
    end



all(r,s:{A,A})
        -- The carrier of an intersection of two relations is a subset of the
        -- carrier of the second relation
    ensure
        (r*s).carrier <= s.carrier
    assert
        (s * r).carrier <= s.carrier    -- previous theorem
        s * r = r * s
        r * s in {t: t.carrier <= s.carrier}
    end





all(r,s:{A,A})
         -- The intersection of two antisymmetric relations is antisymmetric
    require
        r.is_antisymmetric
        s.is_antisymmetric
    ensure
        (r * s).is_antisymmetric
    end



all(p:{A})
    ensure
        p in (<=).carrier
    assert
        {p,q: p <= q}(p,p)
    end


{: Closure
   ======= :}


closed(a:A, r:{A,A}): ghost {A}
    -> {(p): a in p, all(x,y) x in p ==> r(x,y) ==> y in p}


closed(p:{A}, r:{A,A}): ghost {A}
    -> {(q): all(x) x in p ==> x in q,
             all(x,y) x in q ==> r(x,y) ==> y in q}


all(a:A, r:{A,A})
    ensure
        a.closed(r) <= {a}.closed(r)
        assert
            all(x)
                require
                    x in a.closed(r)
                ensure
                    x in {a}.closed(r)
                    inspect x in a.closed(r)
                end
    end


all(a:A, r:{A,A})
    ensure
        {a}.closed(r) <= a.closed(r)
        assert
            all(x)
                require
                    x in {a}.closed(r)
                ensure
                    x in a.closed(r)
                    inspect x in {a}.closed(r)
                    case all(x) x in {a} ==> x in {a}.closed(r)
                        assert
                            a = x
                            a in a.closed(r)
                end
    end





{: Reflexivity
   =========== :}


is_reflexive (r:{A,A}): ghost BOOLEAN
    -> (all(x,y) r(x,y) ==> r(x,x)) and
       (all(x,y) r(x,y) ==> r(y,y))


all(a:A, r:{A,A})
    require
        r.is_reflexive
        a in r.carrier
    ensure
        r(a,a)

    if a in r.domain
        via some(b) r(a,b)
    orif a in r.range
        via some(b) r(b,a)
    end


all(r:{A,A})
    require
        r.is_reflexive
    ensure
        r.domain  = r.range
    assert
        all(x) require x in r.domain
               ensure  x in r.range
                       via some(y) r(x,y)
                           assert
                               r(x,x)
               end

        all(y) require y in r.range
               ensure  y in r.domain
                       via some(x) r(x,y)
                           assert
                               r(y,y)
               end
        r.domain = r.range
    end


all(r:{A,A})
    require
        r.is_reflexive
    ensure
        r.domain  = r.range
    assert
        all(x) require x in r.domain
               ensure  x in r.range
                       via some(y) r(x,y)
                           assert
                               r(x,x)
               end

        all(y) require y in r.range
               ensure  y in r.domain
                       via some(x) r(x,y)
                           assert
                               r(y,y)
               end
        r.domain = r.range
    end


all(r:{A,A})
    require
        r.is_reflexive
    ensure
        r.domain <= r.range
    assert
        all(x) require x in r.domain
               ensure  x in r.range
                       via some(y) r(x,y)
                           assert
                               r(x,x)
               end
    end

all(r:{A,A})
    require
        r.is_reflexive
    ensure
        r.range <= r.domain
    assert
        all(y) require y in r.range
               ensure  y in r.domain
                       via some(x) r(x,y)
                           assert
                               r(y,y)
               end
    end


all(r:{A,A})
    require
        r.is_reflexive
    ensure
        r.carrier <= r.domain
    assert
        all(x)
        require
            x in r.carrier
        ensure
            x in r.domain
        assert
            r(x,x)
        if x in r.domain
        orif x in r.range
        end
    end

all(r:{A,A})
    require
        r.is_reflexive
    ensure
        r.carrier <= r.range
    assert
        all(a)
            require
                a in r.carrier
            ensure
                a in r.range
            assert
                r(a,a)
            if a in r.domain
            orif a in r.range
            end
    end



all(r:{A,A})
        -- Every dichotomic relation is reflexive.
    require
        r.is_dichotomic
    ensure
        r.is_reflexive
    assert
        all(x,y)
             require
                 r(x,y)
             ensure
                 r(x,x)
             assert
                 r(x,x) or r(x,x)
             end
        all(x,y)
             require
                 r(x,y)
             ensure
                 r(y,y)
             assert
                 r(y,y) or r(y,y)
             end
    end




to_reflexive (p:{A}): {A,A}
    -> {x,y: x=y and p(x)}

all(p:{A})
    ensure
        inverse(p.to_reflexive) = p.to_reflexive
    end

all(p:{A})
    ensure
        domain(p.to_reflexive) = p
    assert
        all(x) require x in p
               ensure  x in domain(p.to_reflexive)
               assert   (p.to_reflexive)(x,x) end

        all(x) require x in domain(p.to_reflexive)
               ensure  x in p
               assert   some(y) (p.to_reflexive)(x,y)
                       all(y)  require (p.to_reflexive)(x,y)
                               ensure  x in p end
               end
    end


all(p:{A})
    ensure
        range(p.to_reflexive) = p
    assert
        p.to_reflexive.inverse = p.to_reflexive

        range(p.to_reflexive) = domain(p.to_reflexive.inverse)
    end

all(p:{A})
    ensure
        carrier(p.to_reflexive) = p
    assert
        domain(p.to_reflexive) = p
        range (p.to_reflexive) = p
    end



reflexive (r:{A,A}): ghost {A,A}
    -> {(s): all(a,b) r(a,b) ==> s(a,b),
             all(a,b) r(a,b) ==> s(a,a),
             all(a,b) r(a,b) ==> s(b,b)}


all(a,b:A, r:{A,A})
    require
        (r.reflexive)(a,b)
    ensure
        (r.reflexive)(a,a)

        inspect (r.reflexive)(a,b)
    end

all(a,b:A, r:{A,A})
    require
        (r.reflexive)(a,b)
    ensure
        (r.reflexive)(b,b)

        inspect (r.reflexive)(a,b)
    end






all(r:{A,A})
    ensure
        r.reflexive.is_reflexive
    end



{: Symmetry
   ======== :}

symmetric (r:{A,A}): {A,A}
    -> r + r.inverse





{: Transitivity
   ============ :}

is_transitive(r:{A,A}): ghost BOOLEAN
        -- Is the relation 'r' transitive?
    -> all(a,b,c) r(a,b) ==> r(b,c) ==> r(a,c)

(+) (r:{A,A}): ghost {A,A}
        -- The least transitive relation which contains 'r'.
    -> {(s): all(x,y)   r(x,y) ==> s(x,y),
             all(x,y,z) s(x,y) ==> r(y,z) ==> s(x,z)}


all(a,b,c:A, r:{A,A})
    require
        (+r)(a,b)
        (+r)(b,c)

    ensure
        (+r)(a,c)

    inspect
        (+r)(b,c)
        -- The induction goal generated by the compiler:
        --      all(a) (+r)(a,b) ==> (+r)(b,c) ==> (+r)(a,c)
        -- 'b' and 'c' are not quantified, because (b,c) is the inspect variable!

    case all(b,y,c) (+r)(b,y) ==> r(y,c) ==> (+r)(b,c)
        -- (+r)(b,c) because there is a 'y' between 'b' and 'c' with 'r(y,c)'
        assert
            all(x) (+r)(x,b) ==> (+r)(b,y) ==> (+r)(x,y)
                  -- ind hypo:
                  -- (b,y) already satisfies the goal
            (+r)(a,y)  -- ind hypo with x:=a
            r(y,c)     -- because of the case
            (+r)(a,c)  -- def '+r'
    end






all(r:{A,A})
    ensure
        (+r).is_transitive
    end



{: Reflexive transitive closure
   ============================ :}


(*) (r:{A,A}): ghost {A,A}
        -- The least reflexive transitive relation which contains 'r'.
    -> {(s):
           all(a,b) r(a,b) ==> s(a,a)
           ,
           all(a,b) r(a,b) ==> s(b,b)
           ,
           all(a,b,c) s(a,b) ==> r(b,c) ==> s(a,c)
       }



all(r:{A,A}, a:A)
        -- '*r' is reflexive
    require
        a in r.carrier
    ensure
        (*r)(a,a)
    if a in r.domain
        via some(b) r(a,b)
    orif a in r.range
        via some(b) r(b,a)
    end



all(a,b,c:A, r:{A,A})
        -- '*r' is transitive
    require
        (*r)(a,b)
        (*r)(b,c)
    ensure
        (*r)(a,c)
    inspect
        (*r)(b,c)
    case all(b,c1,c) (*r)(b,c1) ==> r(c1,c) ==> (*r)(b,c)
        assert
            (*r)(a,c1)
    end


all(r:{A,A})
        -- Reflexive transitive closure is increasing
    ensure
        r <= *r
    assert
        all(a)
            require
                a in r.domain
            ensure
                (*r)(a,a)
            via some(b) r(a,b)
            end

        all(a,b)
            require
                r(a,b)
            ensure
                (*r)(a,b)
            assert
                (*r)(a,a)
            end
    end


all(r:{A,A})
        -- Transitive closure is less equal reflexive transitive closure
    ensure
        +r <= *r
    assert
        all(a,b)
            require
                (+r)(a,b)
            ensure
                (*r)(a,b)
            inspect
                (+r)(a,b)
            case all(a,b) r(a,b) ==> (+ r)(a,b)
                assert
                    r <= *r
            end
    end


all(r,s:{A,A})
        -- Reflexive transitive closure in monotonic.
    require
        r <= s
    ensure
        *r <= *s
    assert
        all(a,b)
            require
                (*r)(a,b)
            ensure
                (*s)(a,b)
            inspect
                (*r)(a,b)
            case all(a,b) r(a,b) ==> (*r)(a,a)
                assert
                    s(a,b)
            case all(a,b) r(a,b) ==> (*r)(b,b)
                assert
                    s(a,b)
            end
    end





all(r:{A,A})
        -- Reflexive transitive closure does not increase the carrier.
    ensure
        (*r).carrier <= r.carrier
    assert
        all(a)
             require
                a in (*r).carrier
             ensure
                a in r.carrier
             if a in (*r).domain
                 via some(b) (*r)(a,b)
                 inspect
                     (*r)(a,b)
             orif a in (*r).range
                 via some(b) (*r)(b,a)
                 inspect
                     (*r)(b,a)
             end
    end


all(r,s:{A,A})
    require
        s <= *r
    ensure
        *s <= *r
    assert
        s.carrier <= (*r).carrier   -- precondition and carrier is monotonic
        s.carrier <= r.carrier      -- (*r).carrier <= r.carrier

        all(a,b)
            require
                (*s)(a,b)
            ensure
                (*r)(a,b)
            inspect
                (*s)(a,b)
            case all(a,b) s(a,b) ==> (* s)(a,a)
                assert
                    a in r.carrier
                if a in r.domain
                    via some(b) r(a,b)
                orif a in r.range
                    via some(b) r(b,a)
            case all(a,b) s(a,b) ==> (* s)(b,b)
                assert
                    b in r.carrier
                if b in r.domain
                    via some(a) r(b,a)
                orif b in r.range
                    via some(a) r(a,b)
            end
    end


all(r:{A,A})
        -- Idempotence
    ensure
        * * r = * r
    end



{: Equivalence
   =========== :}

equivalence (r:{A,A}): ghost {A,A}
    -> + r.reflexive.symmetric
