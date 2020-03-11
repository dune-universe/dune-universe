use
    predicate
end

A: ANY
B: ANY


all(r,s:{A,B})
    require
        all(a,b) r(a,b) ==> s(a,b)
    ensure
        r <= s
    assert
        all(t)
            require
                r(t)
            ensure
                s(t)
            inspect
                t
            end
    end


{: Domain and range
   ================ :}

domain (r:{A,B}): ghost {A}         -> {a: some(b) r(a,b)}
range  (r:{A,B}): ghost {B}         -> {b: some(a) r(a,b)}


domains (rs:{{A,B}}): ghost {{A}}
        -- The collection of all domains of the relations in 'rs'.
    -> {p: some(r) r in rs and p = r.domain}

ranges  (rs:{{A,B}}): ghost {{B}}
        -- The collection of all ranges of the relations in 'rs'.
    -> {p: some(r) r in rs and p = r.range}



all(r,s:{A,B})
    ensure
        (r*s).domain <= r.domain
    assert
        all(a)
            require
                a in (r*s).domain
            ensure
                a in r.domain
            via
                some(b) (r*s)(a,b)
            end
    end

all(r,s:{A,B})
    ensure
        (r*s).domain <= s.domain
    assert
        (s * r).domain <= s.domain  -- previous theorem
        s*r = r*s
        r*s in {t: t.domain <= s.domain}
    end


all(r,s:{A,B})
    ensure
        (r*s).range <= r.range
    assert
        all(b)
            require
                b in (r*s).range
            ensure
                b in r.range
            via
                some(a) (r*s)(a,b)
            end
    end

all(r,s:{A,B})
    ensure
        (r*s).range <= s.range
    assert
        (s * r).range <= s.range  -- previous theorem
        s*r = r*s
        r*s in {t: t.range <= s.range}
    end


all(r:{A,B}, rs:{{A,B}})
    require
        r in rs
    ensure
        r.domain in rs.domains
    assert
        r in rs and r.domain = r.domain
    end


all(rs:{{A,B}})
        -- The domain of a union of relations is the union of the domains
    ensure
        (+ rs).domain = + rs.domains
    assert
        all(x)
            require
                x in (+ rs).domain
            ensure
                x in + rs.domains

            via some(y) (+ rs)(x,y)
            via some(r) r in rs and r(x,y)
            assert
                r.domain in rs.domains and x in r.domain
            end

        all(x)
            require
                x in + rs.domains
            ensure
                x in (+ rs).domain

            via some(d) d in rs.domains and x in d
            via some(r) r in rs and d = r.domain
            assert
                x in r.domain
            via some(y) r(x,y)
            assert
                r in rs and r(x,y)
                (+ rs)(x,y)
                some(y) (+ rs)(x,y)
            end
    end




{: Domain and range restriction
   ============================ :}

(|) (p:{A}, r:{A,B}): {A,B}
    -> {x,y: x in p and r(x,y)}

(|) (r:{A,B}, q:{B}): {A,B}
    -> {x,y: r(x,y) and y in q}





{: Image and preimage
   ================== :}

[] (r:{A,B}, p:{A}): ghost {B}
    -> {b: some(a) a in p and r(a,b)}

image    (p:{A}, r:{A,B}): ghost {B} -> {b: some(a) a in p and r(a,b)}
preimage (p:{B}, r:{A,B}): ghost {A} -> {a: some(b) b in p and r(a,b)}





{: Inverse of a relation
   ===================== :}


inverse (r:{A,B}): {B,A}          -> {b,a: r(a,b)}


all(r:{A,B}, y:B)
    require
        y in r.range
    ensure
        y in r.inverse.domain
    via some(x)
        r(x,y)
    assert
        (r.inverse)(y,x)
    end


all(r:{A,B}, y:B)
    require
        y in r.inverse.domain
    ensure
        y in r.range
    via some(x)
        (r.inverse)(y,x)
    end


all(r:{A,B})
    ensure
        range(r)  = domain(inverse(r))
    end



all(r:{A,B}, x:A)
    require
        x in r.domain
    ensure
        x in r.inverse.range
    via some(y)
        r(x,y)
    assert
        (r.inverse)(y,x)
    end


all(r:{A,B}, x:A)
    require
        x in r.inverse.range
    ensure
        x in r.domain
    via some(y)
        (r.inverse)(y,x)
    assert
        (r.inverse)(y,x)
    end


all(r:{A,B})
    ensure
        domain(r) = range (inverse(r))
    end

all(r:{A,B})
    ensure
        range (inverse(r))  = domain(r)
    end

all(r:{A,B})
    ensure
        domain(inverse(r))  = range (r)
    end


all(r:{A,B})
    ensure
        r.inverse.inverse = r
    end


all(rs:{{A,B}})
        -- The inverse of a union of relations is the union of the inverse
        -- relations.
    ensure
        (+ rs).inverse = + {r: r.inverse in rs}

    assert
        all(x,y)
            require
                ((+ rs).inverse)(x,y)
            ensure
                (+ {r: r.inverse in rs})(x,y)

            via some(r) r in rs and r(y,x)
            assert
                r.inverse.inverse = r
                r.inverse in {s: s.inverse in rs} and (r.inverse)(x,y)

                some(s) s in {s: s.inverse in rs} and s(x,y)
            end

        all(x,y)
            require
                (+ {r: r.inverse in rs})(x,y)
            ensure
                ((+ rs).inverse)(x,y)

            via some(r) r in {r: r.inverse in rs} and r(x,y)
            assert
                r.inverse in rs and (r.inverse)(y,x)

                some(s) s in rs and s(y,x)
                (+ rs)(y,x)
            end
    end



all(rs:{{A,B}})
        -- The range of a union of relations is the union of the ranges
    ensure
        (+ rs).range = + rs.ranges
    assert
        all(y)
            require
                y in (+ rs).range
            ensure
                y in + rs.ranges
            via some(x)
                (+ rs)(x,y)
            via some(r)
                r in rs and r(x,y)
            assert
                r in rs and r.range = r.range
                r.range in rs.ranges and y in r.range
                some(p) p in rs.ranges and y in p
            end

        all(y)
            require
                y in + rs.ranges
            ensure
                y in (+ rs).range
            via some(p)
                p in rs.ranges and y in p
            via some(r)
                r in rs and p = r.range    -- def 'ranges'
            assert
                y in r.range
            via some(x)
                r(x,y)
            assert
                r in rs and r(x,y)
                (+ rs)(x,y)
                some(x) (+ rs)(x,y)
            end
    end



{: Relations which are functions
   ============================= :}

is_function(r:{A,B}): ghost BOOLEAN
    -> all(x,y1,y2) r(x,y1) ==> r(x,y2) ==> y1 = y2



[] (r:{A,B}, x:A): ghost B
    require
        r.is_function
        x in r.domain
    ensure
        r(x,Result)
    end
