use
    predicate
    partial_order
    -- endofunction
end


PO: PARTIAL_ORDER


above (p:{PO}, a:PO): {PO}
    -> {x: p(x) and a <= x}


is_closure_system (p:{PO}):  ghost BOOLEAN
    -> (all(a) p.above(a) /= empty) and
       all(q) q <= p  ==> q /= empty ==> (some(x) x.is_infimum(q)) and *q in p

{:
is_closure_map(f:PO->PO): ghost BOOLEAN ->
    f.is_total and
    f.is_ascending and
    f.is_monotonic and
    f.is_idempotent
:}


all(a:PO, p:{PO})
    require
        p.is_closure_system
    ensure
        some(x) x.is_infimum(p.above(a))
    assert
        (some(x) x.is_infimum(p.above(a))) and * p.above(a) in p
    end



all(a:PO, p:{PO})
    require
        p.is_closure_system
    ensure
        (* p.above(a)) in p
    assert
        (some(x) x.is_infimum(p.above(a))) and * p.above(a) in p
    end



all(a:PO, p:{PO})
    require
        p.is_closure_system
    ensure
        some(x) x.is_least(p.above(a))
    assert
        (* p.above(a)).is_infimum(p.above(a))
        (* p.above(a)) in p
        (* p.above(a)).is_least(p.above(a))
    end




closed (a:PO, p:{PO}): ghost PO
    require
        p.is_closure_system
    ensure
        -> least(p.above(a))
    end



all(a:PO, p:{PO})
    require
        p.is_closure_system
    ensure
        a <= a.closed(p)
    assert
        a in lower_bounds(p.above(a))
        least(p.above(a)).is_least(p.above(a))
    end




all(a,b:PO, p:{PO})
    require
        p.is_closure_system
        a <= b
    ensure
        a.closed(p) <= b.closed(p)
    assert
        least(p.above(a)) <= least(p.above(b))
    end



all(a:PO, p:{PO})
    require
        p.is_closure_system
    ensure
        a.closed(p) <= a.closed(p).closed(p)
    end
