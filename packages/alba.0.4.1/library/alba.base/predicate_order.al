use
    predicate
    partial_order
end




A: ANY

{:
# The type PREDICATE[A] is a partial order

In order to prove this claim we have to prove that the union and intersection
operator on collection of sets is a supremum and an infimum respectively.

:}



all(p:{A}, ps:{{A}})
    ensure
        p in ps ==> *ps <= p
    end

all(p:{A}, ps:{{A}})
    require
        p in ps
    ensure
        p <= +ps

    assert
        all(x)
            require
                x in p
            ensure
                x in +ps
                assert
                    p in ps and x in p
                    some(q) q in ps and x in q
            end
    end

all(p:{A}, ps:{{A}})
    require
        all(q) q in ps ==> p <= q
    ensure
        p <= *ps

    assert
        all(x,q)
            require
                x in p
                q in ps
            ensure
                x in q
                assert p <= q
            end
    end

all(p:{A}, ps:{{A}})
    require
        all(q) q in ps ==> q <= p
    ensure
        +ps <= p   -- +ps = {x: some(q) q in ps and x in q}

    assert
        all(x)
            require
                x in +ps
            ensure
                x in p
                via some(q) q in ps and x in q
                assert
                    q <= p
            end
    end



all(p,q,r:{A})
        -- 'p * q' is the infimum of 'p' and 'q'
    require
       r <= p
       r <= q
    ensure
       r <= p * q
    end


all(p,q,r:{A})
        -- 'p + q' is the supremum of 'p' and 'q'
    require
       p <= r
       q <= r
    ensure
       p + q <= r
    end


class
    PREDICATE[A]
inherit
    ghost PARTIAL_ORDER
end


all(ps:{{A}})
    ensure
        (+ ps).is_supremum(ps)
    end


all(ps:{{A}})
    ensure
        (* ps).is_infimum(ps)
    end
