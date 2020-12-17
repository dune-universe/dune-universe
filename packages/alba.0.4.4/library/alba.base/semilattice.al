use
    partial_order
end

deferred class
    SL:SEMILATTICE

(*)  (a,b:SL): SL      deferred end
(<=) (a,b:SL): BOOLEAN -> a = a * b

all(a,b,c:SL)
    ensure
        a * a = a

        a * b = b * a

        a * b * c = a * (b * c)
    deferred end


all(a:SL)
    ensure
        a <= a
    end

all(a,b:SL)
    require
        a <= b
        b <= a
    ensure
        a = b
    end




all(a,b,c:SL)
    require
        a <= b
        b <= c
    ensure
        a <= c
    assert
        ensure
            a = a * c
        via [ a
            , a * b
            , a * (b * c)
            , a * b * c
            , a * c
            ]
        end
    end


all(a,b,c:SL)
    require
        a <= b
        b <= c
    ensure
        a <= c
    assert
        ensure
            a = a * c
        via [ a
            , a * b
            , a * (b * c)
            , a * b * c
            , a * c
            ]
        end
    end


all(a,b:SL)
    ensure
        a * b <= a
    assert
        ensure
            a * b = a * b * a
        assert
            a = a * a
            a * b = b * a
        via [ a * b
            , a * a * b
            , a * (a * b)
            , a * (b * a)
            , a * b * a
            ]
        end
    end


all(a,b:SL)
    ensure
        a * b <= b
    assert
        ensure
            a * b = a * b * b
        assert
            b = b * b
        via [ a * b
            , a * (b * b)
            , a * b * b
            ]
        end
    end

all(a,b,c:SL)
    require
        c <= a
        c <= b
    ensure
        c <= a * b
    assert
        ensure
            c = c * a * b
        via [ c
            , c * b
            , c * a * b
            ]
        end
    end






deferred class
    SEMILATTICE
inherit
    PARTIAL_ORDER
end



all(a,b:SL)
    ensure
        (a * b).is_infimum({a,b})
    end


G:ANY


all(p:{G})
    ensure
        p * p = p
    end

all(p,q:{G})
    ensure
        p * q = q * p
    end

all(p,q,r:{G})
    ensure
        (p * q) * r = p * (q * r)
    end

class
    core.PREDICATE[G]
inherit
    ghost SEMILATTICE
end
