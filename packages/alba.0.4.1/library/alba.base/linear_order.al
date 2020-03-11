use
    boolean
    partial_order
end


deferred class LO:LINEAR_ORDER

(<=) (a,b:LO): BOOLEAN    deferred end

all(a,b,c:LO)
    ensure
        a <= b or b <= a
        a <= b  ==>  b <= a  ==>  a = b
        a <= b  ==>  b <= c  ==>  a <= c
    deferred
    end


all(a:LO)
    ensure
        a <= a
    assert
        a <= a or a <= a
    end


deferred class LINEAR_ORDER
inherit        PARTIAL_ORDER end


all(a,b:LO)
    require
        not (a <= b)
    ensure
        b < a
    assert
        a <= b  or  b <= a

        require  b = a
        ensure   false
            assert a in {x: x <= b}
        end
    end

all(a,b:LO)
    require
        not (a < b)
    ensure
        b <= a
        assert
            require  not (b <= a)
            ensure   false
                assert   a < b
            end
    end



all(a,b:LO)
    require
        a /= b
    ensure
        a < b  or  b < a
        assert
            require
                not (a < b)
            ensure
                b < a
            end
    end



{: Maximum and Minimum
   =================== :}

min (a,b:LO): LO  -> if a <= b then a else b
max (a,b:LO): LO  -> if a <= b then b else a


all(a,b:LO)
    ensure
        min(a,b).is_least({a,b})
    assert
        ensure
            min(a,b) in {a,b}
        if a <= b
        else
        end

        all(x)
            require
                x in {a,b}
            ensure
                min(a,b) <= x
            if x in {a}
                if a <= b
                else
            orif x in {b}
                if a <= b
                else
            end
    end




all(a,b:LO)
    ensure
        max(a,b).is_greatest({a,b})
    assert
        ensure
            max(a,b) in {a,b}
        if a <= b else
        end

        all(x)
            require
                x in {a,b}
            ensure
                x <= max(a,b)
            if x in {a}
                if a <= b else
            orif x in {b}
                if a <= b else
            end
    end
