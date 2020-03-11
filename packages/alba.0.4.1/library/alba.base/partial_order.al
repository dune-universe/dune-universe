use
    predicate
    order_relation
end

deferred class PO:PARTIAL_ORDER

PO2: PARTIAL_ORDER

(<=) (a,b:PO): BOOLEAN   deferred end

all(a,b,c:PO)
    ensure
        a <= a                               -- reflexive
        (a <= b) ==> (b <= a) ==> (a = b)    -- antisymmetric
        (a <= b) ==> (b <= c) ==> (a <= c)   -- transitive
    deferred
    end


(<)  (a,b:PO): BOOLEAN  -> a /= b and a <= b

(>=) (a,b:PO): BOOLEAN  -> b <= a

(>)  (a,b:PO): BOOLEAN  -> b < a


all(a:PO)
    ensure
        a in (<=).carrier
    assert
        {x,y: x <= y}(a,a)
    end



{:
# Upper and lower bounds
:}

is_lower_bound (a:PO, p:{PO}): ghost BOOLEAN -> all(x) p(x) ==> a <= x

is_upper_bound (a:PO, p:{PO}): ghost BOOLEAN -> all(x) p(x) ==> x <= a

has_lower_bound (p:{PO}): ghost BOOLEAN -> some(x) x.is_lower_bound(p)

has_upper_bound (p:{PO}): ghost BOOLEAN -> some(x) x.is_upper_bound(p)

upper_bounds (p:{PO}): ghost {PO} -> {x: x.is_upper_bound(p)}

lower_bounds (p:{PO}): ghost {PO} -> {x: x.is_lower_bound(p)}


all(a,b:PO)
        -- Lemma to connect '<=' with a general relation.
    require
        {a,b: a <= b}(a,b)
    ensure
        a <= b
    end

all(a:PO, p:{PO})
    require
        a.is_lower_bound(p)
    ensure
        a.is_lower_bound(p,(<=))
    end


all(a:PO, p:{PO})
    require
        a.is_lower_bound(p,(<=))
    ensure
        a.is_lower_bound(p)
    end


all(a:PO, p:{PO})
    require
        a.is_upper_bound(p)
    ensure
        a.is_upper_bound(p,(<=))
    end


all(a:PO, p:{PO})
    require
        a.is_upper_bound(p,(<=))
    ensure
        a.is_upper_bound(p)
    end





{:
# Greatest and least elements
:}

is_least (a:PO, p:{PO}): ghost BOOLEAN    -> p(a) and a.is_lower_bound(p)

is_greatest (a:PO, p:{PO}): ghost BOOLEAN -> p(a) and a.is_upper_bound(p)

is_minimal (a:PO, p:{PO}): ghost BOOLEAN  -> p(a) and all(x) x < a ==> not p(x)

is_maximal (a:PO, p:{PO}): ghost BOOLEAN  -> p(a) and all(x) a < x ==> not p(x)

has_least (p:{PO}): ghost BOOLEAN -> some(x) x.is_least(p)

has_greatest (p:{PO}): ghost BOOLEAN -> some(x) x.is_greatest(p)


all(a,b:PO, p:{PO})
    require
        a.is_least(p)
        b.is_least(p)
    ensure
        a = b
    end

all(a,b:PO, p:{PO})
    require
        a.is_greatest(p)
        b.is_greatest(p)
    ensure
        a = b
    end

least(p:{PO}): ghost PO
    require
        some(x) x.is_least(p)
    ensure
        Result.is_least(p)
    end



greatest(p:{PO}): ghost PO
    require
        some(x) x.is_greatest(p)
    ensure
        Result.is_greatest(p)
    end


all(a:PO, p:{PO})
    require
        a.is_least(p)
    ensure
        a.is_least(p,(<=))
    end


all(p,q:{PO})
        -- Bigger set has lower least element (antitonic)
    require
        p.has_least
        q.has_least
        p <= q
    ensure
        least(q) <= least(p)
    assert
        least(q).is_least(q)
        least(p).is_least(p)
    end









{:
# Infimum and supremum
:}

is_infimum (a:PO, p:{PO}): ghost BOOLEAN  -> a.is_greatest(p.lower_bounds)

is_supremum (a:PO, p:{PO}): ghost BOOLEAN -> a.is_least(p.upper_bounds)

has_infimum (p:{PO}): ghost BOOLEAN -> some(x) x.is_infimum(p)

has_supremum (p:{PO}): ghost BOOLEAN -> some(x) x.is_supremum(p)


all(a,b:PO, p:{PO})
        -- An infimum is unique
    ensure
        a.is_infimum(p)  ==> b.is_infimum(p)  ==> a = b
    end


all(a,b:PO, p:{PO})
        -- A supremum is unique
    ensure
        a.is_supremum(p) ==> b.is_supremum(p) ==> a = b
    end


all(a:PO, p:{PO})
    require
        a.is_infimum(p)
    ensure
        a.is_infimum(p,(<=))
    end





{:
# Upper and lower set
:}

upper_set (a:PO): {PO}
    -> {x: a <= x}

lower_set (a:PO): {PO}
    -> {x: x <= a}


{:
# Functions on partial orders
:}

is_monotonic(f:PO->PO2): ghost BOOLEAN ->
    all(a,b:PO) {a,b} <= f.domain ==> a <= b ==> f(a) <= f(b)

is_antitonic(f:PO->PO2): ghost BOOLEAN ->
    all(a,b:PO) {a,b} <= f.domain ==> a <= b ==> f(b) <= f(a)

is_increasing(f:PO->PO): ghost BOOLEAN ->
    all(a) (f.domain)(a) ==> a <= f(a)

is_decreasing(f:PO->PO): ghost BOOLEAN ->
    all(a) (f.domain)(a) ==> f(a) <= a



(*) (a,b:PO): ghost PO
    require
        some(x) x.is_infimum({a,b})
    ensure
        Result.is_infimum({a,b})
    end


(+) (a,b:PO): ghost PO
        -- The least upper bound of 'a' and 'b'.
    require
        some(x) x.is_supremum({a,b})
    ensure
        Result.is_supremum({a,b})
    end



(*) (p:{PO}): ghost PO
    require
        some(x) x.is_infimum(p)
    ensure
        Result.is_infimum(p)
    end

(+) (p:{PO}):  ghost PO
    require
        some(x) x.is_supremum(p)
    ensure
        Result.is_supremum(p)
    end


all(p:{PO})
   require
       p.has_infimum
   ensure
       (*p).is_infimum(p)
   end

all(p:{PO})
   require
       p.has_supremum
   ensure
       (+p).is_supremum(p)
   end

all(a,b,c:PO)
    require
        a < b
        b <= c
    ensure
        a /= c
    assert
        require a = c
        ensure  false
        assert   c = a
                a in {x: b <= x}
        end
    end


all(a,b,c:PO)
    require
        a <= b
        b < c
    ensure
        a /= c
    assert
        require a = c
        ensure  false
        assert   c in {x: x <= b}
        end
    end

all(a,b,c:PO)
    require
        a < b
        b < c
    ensure
        a < c
    end

all(a,b,c:PO)
    require
        a = b
        b <= c
    ensure
        a <= c
    assert
        b = a
        a in {x: x <= c}
    end


all(a,b,c:PO)
    require
        a <= b
        b = c
    ensure
        a <= c
    end


all(a,b:PO, p:{PO})
    require
       a <= b
       b.is_lower_bound(p)
    ensure
       a.is_lower_bound(p)
    end


all(a:PO)
    ensure
        a.is_lower_bound({a})
    assert
        all(x) require  {a}(x)
               ensure   a <= x
               assert    x = a
                        {y: y <= x}(a)
               end
    end

all(a,b:PO)
    require
        a <= b
    ensure
        a.is_lower_bound({b})
    end


all(x:PO, p,q:{PO})
    require
        x.is_lower_bound(p)
        x.is_lower_bound(q)
    ensure
        ((p + q).lower_bounds)(x)
    assert
        all(y) require (p + q)(y)
               ensure  x <= y
               assert   p(y) ==> x <= y
               end
    end




all(x:PO, p,q:{PO})
    require
        x in (p + q).lower_bounds
    ensure
        x.is_lower_bound(p)
    end


all(x:PO, p,q:{PO})
    require
        x in (p + q).lower_bounds
    ensure
        x.is_lower_bound(q)
    end


all(a,b:PO, p,q:{PO})
    require
        a.is_infimum(p)
        b.is_infimum(q)
        p <= q
    ensure
        b <= a
    assert
        b.is_lower_bound(p)
    end


all(a:PO, p:{PO})
    require
        a.is_least(p)
    ensure
        a.is_infimum(p)
    assert
        all(x) require x.is_lower_bound(p)
               ensure  x <= a
               assert   all(y) p(y) ==> x <= y
                       p(a)
               end
    end


all(a:PO, p:{PO})
    require
        a.is_infimum(p)
        a in p
    ensure
        a.is_least(p)
    end


all(a:PO)
    ensure
        a.is_infimum({x: a <= x})
    assert
        a.is_least({x: a <= x})
    end



all(p:{PO})
    require
        some(x) x.is_infimum(p)
    ensure
        (*p).is_infimum(p)
    end



all(x:PO, p:{PO})
    require
        some(x) x.is_infimum(p)
        x.is_infimum(p)
    ensure
        x = *p
    end

all(p,q:{PO})
    require
        p.has_infimum
        q.has_infimum
        p <= q
    ensure
        (*q) <= *p
    assert
        (*q).is_infimum(q)
        (*p).is_infimum(p)
    end







{: Directed Sets and Continuous Functions
   ======================================
:}

is_updirected (d:{PO}): ghost BOOLEAN
    -> d.has_some
       and
       all(a,b)  {a,b} <= d
                 ==>
                 some(x) x in d
                         and
                         x.is_upper_bound({a,b})


is_upcontinuous (f:PO->PO2): ghost BOOLEAN
    -> all(set,sup)
           set <= f.domain
           ==>
           sup in f.domain
           ==>
           sup.is_supremum(set)
           ==>
           f(sup).is_supremum(f[set])





{: Chains
   ======
:}


is_chain(p:{PO}): ghost BOOLEAN
        -- Is the set 'p' a chain?
    -> all(a,b) {a,b} <= p ==> a <= b or b <= a


all(p:{PO}, a,b:PO)
        -- All pairs in a chain have an upper bound
    require
        p.is_chain
        {a,b} <= p
    ensure
        some(x) x in p and a <= x and b <= x
    if a <= b
        assert
            b in p and a <= b and b <= b
    orif b <= a
        assert
            a in p and a <= a and b <= a
    end


{:
# Closure system
:}

is_closure_system(p:{PO}): ghost BOOLEAN
    -> all(q)
           q <= p
           ==>
           some(x) x.is_infimum(q) and x in p


is_weak_closure_system(p:{PO}): ghost BOOLEAN
    -> (all(x) (p * x.upper_set).has_some) -- 'p' is sufficiently large
       and
       all(q) q <= p ==>                   -- 'p' is closed to infimum
              q.has_some ==>
              some(x) x.is_infimum(q) and x in p

is_closure_map (f:PO->PO): ghost BOOLEAN
    -> f.is_total and
       f.is_increasing and
       f.is_monotonic and
       f.is_idempotent



all(p:{PO})
        -- Consistency with closure system of module 'order_relation'.
    require
        p.is_closure_system
    ensure
        p.is_closure_system((<=))
    assert
        p <= (<=).carrier
        all(q)
            require
                q <= p
            ensure
                some(x) x.is_infimum(q,(<=)) and x in p
            via
                some(x) x.is_infimum(q) and x in p
                assert
                    x.is_infimum(q,(<=)) and x in p
            end
    end
