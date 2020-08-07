use
    endorelation
    function
end

A: ANY
B: ANY


{:
# Preorder, partial order and linear order
:}

is_preorder(r:{A,A}): ghost BOOLEAN
    -> r.is_reflexive and
       r.is_transitive


is_partial_order(r:{A,A}): ghost BOOLEAN
    -> r.is_reflexive and
       r.is_transitive and
       r.is_antisymmetric


is_linear_order(r:{A,A}): ghost BOOLEAN
    -> r.is_partial_order and
       r.is_dichotomic


is_linear_preorder(r:{A,A}): ghost BOOLEAN
    -> r.is_preorder and
       r.is_dichotomic


all(r,s:{A,A})
    require
        r.is_partial_order
        s.is_partial_order
    ensure
        (r * s).is_partial_order
    end





{:
# Upper and lower bounds
:}


is_lower_bound(a:A, p:{A}, r:{A,A}): ghost BOOLEAN
        -- Is 'a' a lower bound for the set 'p' with respect to the relation 'r'?
    -> a in r.carrier and all(x) x in p ==> r(a,x)

is_upper_bound(a:A, p:{A}, r:{A,A}): ghost BOOLEAN
        -- Is 'a' an upper bound for the set 'p' with respect to the relation 'r'?
    -> a in r.carrier and all(x) x in p ==> r(x,a)

has_lower_bound(p:{A}, r:{A,A}): ghost BOOLEAN
        -- Does the set 'p' have a lower bound in 'r'?
    -> some(a) a.is_lower_bound(p,r)

has_upper_bound(p:{A}, r:{A,A}): ghost BOOLEAN
        -- Does the set 'p' have an upper bound in 'r'?
    -> some(a) a.is_upper_bound(p,r)

lower_bounds(p:{A}, r:{A,A}): ghost {A}
        -- The set of all lower bounds of the set 'p' with respect to the
        -- relation 'r'.
    -> {a: a.is_lower_bound(p,r)}

upper_bounds(p:{A}, r:{A,A}): ghost {A}
        -- The set of all upper bounds of the set 'p' with respect to the
        -- relation 'r'.
    -> {a: a.is_upper_bound(p,r)}


all(a:A, p:{A}, r:{A,A})
        -- Duality
    require
        a.is_lower_bound(p,r.inverse)
    ensure
        a.is_upper_bound(p,r)
    assert
        r.inverse.carrier <= r.carrier
        all(x)
            require
                x in p
            ensure
                r(x,a)
            assert
                (r.inverse)(a,x)
            end
    end


all(a:A, p:{A}, r:{A,A})
        -- Duality
    require
        a.is_upper_bound(p,r.inverse)
    ensure
        a.is_lower_bound(p,r)
    assert
        r.inverse.carrier <= r.carrier
        all(x)
            require
                x in p
            ensure
                r(a,x)
            assert
                (r.inverse)(x,a)
            end
    end



all(a,b:A, p:{A}, r:{A,A})
        -- Transitivity of a lower bound
    require
        r.is_partial_order
        r(a,b)
        b.is_lower_bound(p,r)
    ensure
        a.is_lower_bound(p,r)
    end


all(a,b:A, p:{A}, r:{A,A})
        -- Transitivity of an upper bound
    require
        r.is_partial_order
        r(a,b)
        a.is_upper_bound(p,r)
    ensure
        b.is_upper_bound(p,r)
    assert
        all(x)
            require
                x in p
            ensure
                r(x,b)
            assert
                r(x,a)
            end
    end


all(p:{A}, r:{A,A})
        -- Duality
    ensure
        p.lower_bounds(r) <= p.upper_bounds(r.inverse)
    assert
        all(x)
            require
                x in p.lower_bounds(r)
            ensure
                x in p.upper_bounds(r.inverse)
            assert
                r.inverse.inverse = r
                x.is_lower_bound(p,r.inverse.inverse)
                x.is_upper_bound(p,r.inverse)
            end
    end


all(p:{A}, r:{A,A})
        -- Duality
    ensure
        p.upper_bounds(r.inverse) <= p.lower_bounds(r)
    assert
        all(x)
            require
                x in p.upper_bounds(r.inverse)
            ensure
                x in p.lower_bounds(r)
            assert
                x.is_upper_bound(p,r.inverse)
                x.is_lower_bound(p,r)
            end
    end



all(p:{A}, r:{A,A})
        -- Duality
    ensure
        p.upper_bounds(r) <= p.lower_bounds(r.inverse)
    assert
        all(x)
            require
                 x in p.upper_bounds(r)
            ensure
                 x in p.lower_bounds(r.inverse)
            assert
                 r.inverse.inverse = r
                 x.is_upper_bound(p,r.inverse.inverse)
                 x.is_lower_bound(p,r.inverse)
            end
    end


all(p:{A}, r:{A,A})
        -- Duality
    ensure
        p.lower_bounds(r.inverse) <= p.upper_bounds(r)
    assert
        all(x)
            require
                x in p.lower_bounds(r.inverse)
            ensure
                x in p.upper_bounds(r)
            assert
                x.is_lower_bound(p,r.inverse)
                x.is_upper_bound(p,r)
            end
    end



{:
# Maximal elements
:}

is_maximal(a:A, p:{A}, r:{A,A}): ghost BOOLEAN
    -> a in p and all(b) b in p ==> r(a,b) ==> a = b


has_maximal(p:{A}, r:{A,A}): ghost BOOLEAN
    -> some(a) a.is_maximal(p,r)


all(a:A, p:{A}, r:{A,A})
        -- Every subset of a partial order which does not have maximal elements
        -- has elements strictly above each of its members.
    require
        r.is_partial_order
        p <= r.carrier
        not p.has_maximal(r)
        a in p
    ensure
        some(b) b in p and r(a,b) and a /= b
    via require
        not some(b) b in p and r(a,b) and a /= b
    assert
        all(b)
            require
                b in p
                r(a,b)
            ensure
                a = b
            assert
                not (b in p and r(a,b) and a /= b)   -- contrapositive of the
                                                     -- assumption
                if not (b in p and r(a,b))
                    if not (b in p)
                    orif not r(a,b)
                orif not (a /= b)
                    via require not (a = b)
            end
        a.is_maximal(p,r)
        p.has_maximal(r)
    end




{:
# Least elements
:}

is_least(a:A, p:{A}, r:{A,A}): ghost BOOLEAN
        -- Is 'a' the least element of set 'p' with respect to the relation 'r'?
    -> a in p and a.is_lower_bound(p,r)

has_least(p:{A}, r:{A,A}): ghost BOOLEAN
        -- Does the set 'p' have a least element?
    -> some(x) x.is_least(p,r)


least(p:{A}, r:{A,A}): ghost A
        -- The least element of the set 'p' in the partial order 'r'.
    require
        r.is_partial_order
        p.has_least(r)
    ensure
        Result.is_least(p,r)
    end


is_least(a:A,r:{A,A}): ghost BOOLEAN
        -- Is 'a' the least element of the carrier of 'r'?
    -> all(x) r(a,x)


has_least(r:{A,A}): ghost BOOLEAN
        -- Does the carrier of 'r' have a least element?
    -> some(a) a.is_least(r)


least(r:{A,A}): ghost A
        -- The least element of the carrier of 'r'.
    require
        r.is_partial_order
        r.has_least
    ensure
        Result.is_least(r)
    end





{:
# Greatest elements
:}

is_greatest(a:A, p:{A}, r:{A,A}): ghost BOOLEAN
        -- Is 'a' the greatest element of set 'p' with respect to the relation 'r'?
    -> a in p and a.is_upper_bound(p,r)

has_greatest(p:{A}, r:{A,A}): ghost BOOLEAN
        -- Does the set 'p' have a greatest element?
    -> some(x) x.is_greatest(p,r)



greatest(p:{A}, r:{A,A}): ghost A
        -- The greatest element of the set 'p' in the partial order 'r'.
    require
        r.is_partial_order
        p.has_greatest(r)
    ensure
        Result.is_greatest(p,r)
    end


all(a:A, p:{A}, r:{A,A})
        -- Duality
    require
        a.is_greatest(p,r)
    ensure
        a.is_least(p,r.inverse)
    assert
        r.carrier <= r.inverse.carrier
    end



all(a:A, p:{A}, r:{A,A})
        -- Duality
    require
        a.is_least(p,r)
    ensure
        a.is_greatest(p,r.inverse)
    assert
        r.carrier <= r.inverse.carrier
    end


all(a:A, p:{A}, r:{A,A})
        -- In a partial order every greatest element is maximal
    require
        r.is_partial_order
        a.is_greatest(p,r)
    ensure
        a.is_maximal(p,r)
    assert
        all(b)
            require
                r(a,b)
                a /= b
            ensure
                b /in p
            via require
                b in p
            assert
                r(b,a)    -- a.is_upper_bound(p,r)
                a = b     -- antisymmetry
            end
    end



all(a:A, p:{A}, r:{A,A})
        -- In a linear order every maximal element of a set is the greatest element
        -- of the set.
    require
        r.is_linear_order
        p <= r.carrier
        a.is_maximal(p,r)
    ensure
        a.is_greatest(p,r)
    assert
        all(b) r(a,b) ==> a /= b ==> b /in p
        all(x)
            require
                x in p
            ensure
                r(x,a)
            assert
                r(a,x) or r(x,a)
            if r(x,a)
            orif r(a,x)
                assert
                    -- a /= x ==> x /in p
                    not (x /in p) ==> not (a /= x)
                    not (a /= x)
                    not not (a = x)
            end
    end


all(a:A, p:{A}, r:{A,A})
        -- Every subset of a linear order which does not have a greatest element
        -- has elements strictly above each of its members.
    require
        r.is_linear_order
        p <= r.carrier
        not p.has_greatest(r)
        a in p

    ensure
        some(b) b in p and r(a,b) and a /= b

    assert
        require
            p.has_maximal(r)
        ensure
            p.has_greatest(r)
        via some(a)
            a.is_maximal(p,r)
        assert
            a.is_greatest(p,r)
        end

        not p.has_maximal(r)
    end

{:
# Infimum and supremum
:}

is_infimum(a:A, p:{A}, r:{A,A}): ghost BOOLEAN
        -- Is 'a' an infimum i.e. a greatest lower bound of the set 'p' with
        -- respect to the relation 'r'
    -> a.is_greatest(p.lower_bounds(r),r)

is_supremum(a:A, p:{A}, r:{A,A}): ghost BOOLEAN
        -- Is 'a' a supremum i.e. a least upper bound of the set 'p' with
        -- respect to the relation 'r'
    -> a.is_least(p.upper_bounds(r),r)

has_infimum(p:{A}, r:{A,A}): ghost BOOLEAN
        -- Does the set 'p' have a greatest lower bound?
    -> some(a) a.is_infimum(p,r)

has_supremum(p:{A}, r:{A,A}): ghost BOOLEAN
        -- Does the set 'p' have a least upper bound?
    -> some(a) a.is_supremum(p,r)

infimum(p:{A}, r:{A,A}): ghost A
        -- The greatest lower bound of the set 'p' in the partial order 'r'.
    require
        r.is_partial_order
        p.has_infimum(r)
    ensure
        Result.is_infimum(p,r)
    end

supremum(p:{A}, r:{A,A}): ghost A
        -- The least upper bound of the set 'p' in the partial order 'r'.
    require
        r.is_partial_order
        p.has_supremum(r)
    ensure
        Result.is_supremum(p,r)
    end



all(a:A, p:{A}, r:{A,A})
        -- Duality
    require
        a.is_infimum(p,r)
    ensure
        a.is_supremum(p,r.inverse)
    assert
        a.is_greatest(p.lower_bounds(r), r)

        p.lower_bounds(r) = p.upper_bounds(r.inverse)

        a.is_least(p.upper_bounds(r.inverse), r.inverse)
    end



all(a:A, p:{A}, r:{A,A})
        -- Duality
    require
        a.is_supremum(p,r)
    ensure
        a.is_infimum(p,r.inverse)
    assert
        a.is_least(p.upper_bounds(r),r)

        p.upper_bounds(r) = p.lower_bounds(r.inverse)

        a.is_greatest(p.lower_bounds(r.inverse),r.inverse)
    end


{:
# Up- and downclosed sets
:}

is_downclosed(p:{A}, r:{A,A}): ghost BOOLEAN
    -> p <= r.carrier
       and
       all(x,y) r(x,y) ==> y in p ==> x in p

is_upclosed(p:{A}, r:{A,A}): ghost BOOLEAN
    -> p <= r.carrier
       and
       all(x,y) r(x,y) ==> x in p ==> y in p


downclosed(p:{A}, r:{A,A}): ghost {A}
        -- The downward closure of the set 'p' with respect to the relation 'r'.
        -- 'p' has to be a subset of the carrier of 'r'.
    require
        p <= r.carrier
    ensure
        -> {(q): all(x) x in p ==> x in q,
                 all(x,y) r(x,y) ==> y in q ==> x in q}
    end


all(ps:{{A}}, r:{A,A})
        -- An arbitrary union of downclosed sets is downclosed
    require
        all(p) p in ps ==> p.is_downclosed(r)
    ensure
        (+ ps).is_downclosed(r)

    assert
        all(x)
                -- + ps <= r.carrier
            require
                x in + ps
            ensure
                x in r.carrier
            via
                some(p) p in ps and x in p
                assert
                    p.is_downclosed(r)
            end

        all(x,y)
                -- (+ ps).is_downclosed(r)
            require
                r(x,y)
                y in + ps
            ensure
                x in + ps
            via
                some(p) p in ps and y in p
                assert
                    p.is_downclosed(r)
                    p in ps and x in p
            end
    end



all(r:{A,A}, p:{A}, a:A)
        -- Adding the least element of the complement of a downclosed set to
        -- the set in a partial order gives a downclosed set.
    require
        r.is_partial_order
        p.is_downclosed(r)
        a.is_least(r.carrier - p, r)

    ensure
        (p + {a}).is_downclosed(r)

    assert
        all(x,y)
            require
                y in p + {a}
                r(x,y)
            ensure
                x in p + {a}
            if y in p
            orif y in {a}
                assert
                    require
                        not (x in p)
                    ensure
                        x in {a}
                    assert
                        y.is_least(r.carrier - p, r)
                        y.is_lower_bound(r.carrier - p , r)
                        all(x) x in r.carrier - p ==> r(y,x)
                        r(y,x)
                        y = x
                        x in {a}
                    end
            end
    end




all(r:{A,A}, p:{A})
        -- In a linear order the complement of a downclosed set is upclosed.
    require
        r.is_linear_order
        p.is_downclosed(r)

    ensure
        (r.carrier - p).is_upclosed(r)
    end



{:
# Upper and lower set

The lower sets play an important role in the study of order relations. The set
'a.lower_set(r)' for an element 'a' in the carrier of 'r' is inductively
defined. It is the set of all elements starting from 'a' and following 'r' in
downward direction.

This definition is valid for any relation. However it very useful for
preorders or more specific orders. In a preorder 'a.lower_set(r)' coincides
with the set of elements which are less or equal 'a'.

The collection of lower sets of a preorder characterizes completely the
preorder relation. I.e. having the lower sets it is possible to reconstruct
the relation.



:}


upper_set(a:A, r:{A,A}): ghost {A}
        -- The set of all elements starting from 'a' and following the
        -- relation 'r' in upward direction.
    require
        a in r.carrier
    ensure
        -> {(p): a in p,
                 all(x,y) r(x,y) ==> x in p ==> y in p}
    end


lower_set(a:A, r:{A,A}): ghost {A}
        -- The set of all elements starting from 'a' and following the
        -- relation 'r' in downward direction.
    require
        a in r.carrier
    ensure
        -> {(p): a in p,
                 all(x,y) r(x,y) ==> y in p ==> x in p}
    end


lower_sets(r:{A,A}): ghost {{A}}
        -- The collection of all lower sets of the relation 'r'.
    -> {p: some(a) a in r.carrier and p = a.lower_set(r)}



strict_lower_set(a:A, r:{A,A}): ghost {A}
        -- 'a.lower_set(r)' without 'a'.
    require
        a in r.carrier
    ensure
        -> {x: x in a.lower_set(r) and x /= a}
    end


strict_lower_sets(p:{A}, r:{A,A}): ghost {{A}}
        -- The collection of all strict lower sets of the elements in 'p'.
    require
        p <= r.carrier
    ensure
        -> {q: some(x) x in p and q = x.strict_lower_set(r)}
    end





all(a,b:A, r:{A,A})
        -- All elements of a lower set are in the carrier.
    require
        a in r.carrier
        b in a.lower_set(r)
    ensure
        b in r.carrier

    inspect
        b in a.lower_set(r)
    end




all(r:{A,A}, p:{A}, b:A)
        -- All lower sets of elements in a downclosed set are subsets
        -- of the downclosed set.
    require
        p.is_downclosed(r)
        b in p

    ensure
        b.lower_set(r) <= p

    assert
        all(a)
            require
                 a in b.lower_set(r)
            ensure
                 a in p
            inspect
                 a in b.lower_set(r)
            end
    end







all(ps:{{A}}, r:{A,A})
        -- The union of all lower sets is the carrier.
    require
        ps = {p: some(a) a in r.carrier and p = a.lower_set(r)}
    ensure
        + ps = r.carrier
    assert
        all(x)
            require
                x in + ps
            ensure
                x in r.carrier
            via some(p) p in ps and x in p
            via some(a) a in r.carrier and p = a.lower_set(r)
            end

        all(x)
            require
                x in r.carrier
            ensure
                x in + ps
            assert
                x in r.carrier and x.lower_set(r) = x.lower_set(r)

                x.lower_set(r) in ps and x in x.lower_set(r)

                some(p) p in ps and x in p
            end
    end


all(a,b:A, r:{A,A})
        -- The lower sets are sufficient to reconstruct the relation.
    require
        r.is_preorder
        b in r.carrier
        a in b.lower_set(r)
    ensure
        r(a,b)

    inspect
        a in b.lower_set(r)
    end



all(a,b:A, r:{A,A})
        -- All elements of an upper set are in the carrier.
    require
        a in r.carrier
        b in a.upper_set(r)

    ensure
        b in r.carrier

    inspect
        b in a.upper_set(r)
    end



all(a,b:A, r:{A,A})
        -- The upper sets are sufficient to reconstruct the relation.
    require
        r.is_preorder
        a in r.carrier
        b in a.upper_set(r)

    ensure
        r(a,b)

    inspect
        b in a.upper_set(r)
    end





all(a,b:A, r:{A,A})
       -- The map 'x -> x.lower_set(r)' is monotonic.
    require
        r.is_preorder
        r(a,b)
    ensure
        a.lower_set(r) <= b.lower_set(r)
    assert
        all(x)
            require
                x in a.lower_set(r)
            ensure
                x in b.lower_set(r)
            inspect
                x in a.lower_set(r)
            end
    end


all(a:A, r:{A,A})
        -- A strict lower set is a proper subset of the carrier
    require
        a in r.carrier
    ensure
        a.strict_lower_set(r) < r.carrier
    assert
        a /in a.strict_lower_set(r) and a in r.carrier
    end


all(a:A, r:{A,A})
    require
        a in r.carrier
    ensure
        a.strict_lower_set(r) + {a} = a.lower_set(r)
    assert
        all(x)
            require
                x in a.strict_lower_set(r) + {a}
            ensure
                x in a.lower_set(r)
            if x in a.strict_lower_set(r)
            orif x in {a}
            end
        all(x)
            require
                x in a.lower_set(r)
            ensure
                x in a.strict_lower_set(r) + {a}
            if x /= a
            orif x = a
            end
    end


all(a:A, r:{A,A})
        -- In a partial order every strict lower set is downclosed.
    require
        r.is_partial_order
        a in r.carrier
    ensure
        a.strict_lower_set(r).is_downclosed(r)
    assert
        all(x,y)
            require
                r(x,y)
                y in a.strict_lower_set(r)

            ensure
                x in a.strict_lower_set(r)

            assert
                y /= a                -- definition of 'strict_lower_set'
                y in a.lower_set(r)   --      "               "

                x in a.lower_set(r)   -- 'r(x,y)' and definition of 'lower_set'
                ensure
                    x /= a
                via require
                    x = a
                assert
                    ensure
                       y = a    -- contradiction
                    assert
                       ensure
                           r(y,a)  -- y in a.lower_set(r)
                       assert
                           r.is_preorder
                       end
                       r(a,y)
                    end
                end
            end
    end


all(r:{A,A}, p:{A}, a,b:A)
        -- In a partial order all elements strictly below the least element of
        -- a set are not in the set.
    require
        r.is_partial_order
        b.is_least(p,r)
        a in b.strict_lower_set(r)

    ensure
        a /in p

    via require
        a in p
    assert
        r.is_preorder
        r(b,a)
        r(a,b)
        a = b
    end



all(r:{A,A}, p:{A}, b:A)
        -- If 'b' is the least element of elements which are not in 'p' then
        -- all elements of the strict lower set of 'b' are in 'p'.
    require
        r.is_partial_order
        b.is_least(r.carrier - p, r)

    ensure
        b.strict_lower_set(r) <= p

    assert
        all(a)
            require
                a in b.strict_lower_set(r)
            ensure
                a in p
            assert
                r.is_preorder
                r(a,b)
            via require
                not (a in p)
            assert
                a in r.carrier - p
                r(b,a)          -- because 'b.is_least(r.carrier - p, r)'
                a = b           -- antisymmetry, contradicts assumption
            end
    end


all(r:{A,A}, a:A)
        -- The complement of a strict lower set is the corresponding upper set
        -- in a linear order.
    require
        r.is_linear_order
        a in r.carrier

    ensure
        r.carrier - a.strict_lower_set(r) = a.upper_set(r)

    assert
        all(b)
            require
                b in r.carrier - a.strict_lower_set(r)
            ensure
                b in a.upper_set(r)
            assert
                not (b /= a and b in a.lower_set(r))
                if not (b /= a)
                    assert
                        not not (b = a)
                orif not (b in a.lower_set(r))
                    assert
                        ensure
                            r(a,b)
                        assert
                            not r(b,a)
                            r(b,a) or r(a,b)
                        end
            end

        all(b)
            require
                b in a.upper_set(r)
            ensure
                b in r.carrier - a.strict_lower_set(r)
            assert
                ensure
                    b /in a.strict_lower_set(r)
                via require
                    b in a.strict_lower_set(r)
                assert
                    r.is_preorder
                    r(a,b)
                    r(b,a)
                    b = a    -- antisymmetry
                end
            end
    end



all(r:{A,A}, a:A)
        -- The complement of an upper set is the corresponding strict lower set
        -- in a linear order.
    require
        r.is_linear_order
        a in r.carrier

    ensure
        r.carrier - a.upper_set(r) = a.strict_lower_set(r)

    assert
        a.upper_set(r) = r.carrier - a.strict_lower_set(r)
    via
        [ r.carrier - (r.carrier - a.strict_lower_set(r)) ]
    end


all(r:{A,A}, a:A)
        -- In a linear order 'a' is the least element of the complement of
        -- 'a.strict_lower_set(r)'.
    require
        r.is_linear_order
        a in r.carrier

    ensure
        a.is_least(r.carrier - a.strict_lower_set(r),r)

    assert
        a in r.carrier - a.strict_lower_set(r)

        all(b)
            require
                b in r.carrier - a.strict_lower_set(r)
            ensure
                r(a,b)
            if a = b
            orif a /= b
                assert
                    b /in a.strict_lower_set(r)
                    not r(b,a)
                if r(a,b) orif r(b,a)
            end
    end





all(r:{A,A}, a,b:A)
        -- Transitivity of strict lower sets.
    require
        r.is_partial_order
        r(a,b)

    ensure
        a.strict_lower_set(r) <= b.strict_lower_set(r)

    assert
        r.is_preorder
        all(x)
            require
                x in a.strict_lower_set(r)
            ensure
                x in b.strict_lower_set(r)
            assert
                r(x,a)
                r(x,b)
                x in b.lower_set(r)
                -- x /= b
            via require
                x = b
            assert
                x = a   -- from r(a,b) and r(x,a) by antisymmetry, contradicts
                        -- x in a.strict_lower_set(r)
            end
    end


all(r:{A,A}, a,b,c:A)
        -- Transitivity of strict lower sets.
    require
        r.is_partial_order
        c in r.carrier
        b in c.strict_lower_set(r)
        r(a,b)

    ensure
        a in c.strict_lower_set(r)

    assert
        r.is_preorder
        b /= c
        b in c.lower_set(r)
        a in c.lower_set(r)
        ensure
            a /= c
        via require
            a = c
        assert
            r(b,c)    -- b in c.lower_set(r)
            r(c,b)    -- a = c and r(a,b)
            b = c     -- antisymmetry
        end
    end



all(r:{A,A}, a,b:A)
        -- If a strict lower set has a greatest element, then the lower set of
        -- this greatest element is a superset of the strict lower set.
    require
        r.is_partial_order
        b in r.carrier
        a.is_greatest(b.strict_lower_set(r), r)

    ensure
        b.strict_lower_set(r) <= a.lower_set(r)

    assert
        all(x)
            require
                x in b.strict_lower_set(r)
            ensure
                x in a.lower_set(r)
            assert
                r(x,a)
            end
    end



{: Consider the set `b.strict_lower_set(r)` in some partial order `r`. Then
every element of this set has a strict lower set as well which is fully
contained in the outer set. Therefore the union of all these strict lower sets
is fully contained in the out set as well.
:}

all(b:A, r:{A,A})
        -- The union of all strict lower sets of a strict lower set is fully
        -- contained in the outer strict lower set.
    require
        r.is_partial_order
        b in r.carrier

    ensure
        + b.strict_lower_set(r).strict_lower_sets(r) <= b.strict_lower_set(r)

    assert
        all(x)
            require
                x in + b.strict_lower_set(r).strict_lower_sets(r)

            ensure
                x in b.strict_lower_set(r)

            via some(p)
                p in b.strict_lower_set(r).strict_lower_sets(r)
                and
                x in p
            via some(y)
                y in b.strict_lower_set(r) and p = y.strict_lower_set(r)
            assert
                r.is_preorder
                x in y.strict_lower_set(r)

                r(x,y)
                r(y,b)
                x = b ==> x = y  -- x = b leads to contradiction

                x in b.lower_set(r)
                x /= b
            end
    end



all(r:{A,A}, p:{A}, a:A)
        -- If 'a' is the least element of the set 'p' then 'p' is a subset of
        -- the upper set of 'a'.
    require
        a.is_least(p,r)

    ensure
        p <= a.upper_set(r)

    assert
        all(b)
            require
                b in p
            ensure
                b in a.upper_set(r)
            assert
                r(a,b)
            end
    end



all(r:{A,A}, p:{A}, a:A)
    require
        r.is_preorder
        p.is_upclosed(r)
        a.is_least(p,r)

    ensure
        a.upper_set(r) <= p

    assert
        all(b)
            require
                b in a.upper_set(r)
            ensure
                b in p
            inspect
                b in a.upper_set(r)
            end
    end




all(r:{A,A}, p:{A}, a:A)
        -- In a linear order if the complement of a downclosed set has a least
        -- element then the downclosed set can be represented as the strict lower
        -- set of this least element.
    require
        r.is_linear_order
        p.is_downclosed(r)
        a.is_least(r.carrier - p, r)

    ensure
        p = a.strict_lower_set(r)

    assert
        r.is_preorder
        (r.carrier - p).is_upclosed(r)
        a.upper_set(r) = r.carrier - p
    via [ p
        , r.carrier - (r.carrier - p)
        , r.carrier - a.upper_set(r)
        , a.strict_lower_set(r)
        ]
    end


{:
# Directed sets
:}

is_updirected(d:{A}, r:{A,A}): ghost BOOLEAN
    -> d.has_some
       and
       all(x,y)
           {x,y} <= d ==>
           some(z) z.is_upper_bound({x,y},r)





{:
# Monotonic functions

A monotonic function is between two order relations is a function which
preserves the order.


:}


is_monotonic(f:A->B, r1:{A,A}, r2:{B,B}): ghost BOOLEAN
    -> f.is_total(r1)
       and
       all(x,y)
           {x,y} <= r1.carrier
           ==>
           r1(x,y)
           ==>
           r2(f(x), f(y))


is_monotonic(f:A->A, r:{A,A}): ghost BOOLEAN
    -> f.is_monotonic(r,r)



is_reflecting(f:A->B, r:{A,A}, s:{B,B}): ghost BOOLEAN
    -> f.is_total(r)
       and
       all(x,y)
           {x,y} <= r.carrier ==>
           s(f(x),f(y)) ==>
           r(x,y)


is_embedding(f:A->B, r:{A,A}, s:{B,B}): ghost BOOLEAN
    -> f.is_total(r)
       and
       f.is_injective
       and
       f.is_monotonic(r,s)
       and
       f.inverse.is_monotonic(s,r)




all(f:A->B, r:{A,A}, s:{B,B})
    require
        r.is_partial_order
        s.is_partial_order
        f.is_monotonic(r,s)
        f.is_reflecting(r,s)
        f.domain = r.carrier
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
                ensure
                    r(x,y)                  -- f.is_reflecting(r,s)
                assert
                    f(x) = f(y)
                    f(y) in {z: s(f(x),z)}  -- s.is_reflexive
                    s(f(x),f(y))
                end
                ensure
                    r(y,x)                  -- f.is_reflecting(r,s)
                assert
                    f(y) = f(x)
                    f(x) in {z: s(f(y),z)}  -- s.is_reflexive
                    s(f(y),f(x))
                end
            end
    end


{:
# Continuous functions

An upcontinuous function preserves suprema i.e.

    x.is_supremum(p,r1) ==> f(x).is_supremum(f[p],r2)

and a downcontiunous function presevers infima

    x.is_infimum(p,r1) ==> f(x).is_infimum(f[p],r2)
:}

is_upcontinuous(f:A->B, r1:{A,A}, r2:{B,B}): ghost BOOLEAN
    -> r1.carrier <= f.domain
       and
       all(p,s)
           s.is_supremum(p,r1)
           ==>
           f(s).is_supremum(f[p],r2)


is_upcontinuous(f:A->A, r:{A,A}): ghost BOOLEAN
    -> f.is_upcontinuous(r,r)

is_prefixpoint(a:A, f:A->A, r:{A,A}): ghost BOOLEAN
    -> a in f.domain and r(a,f(a))

is_increasing(f:A->A, r:{A,A}): ghost BOOLEAN
    -> f.is_total(r) and all(x) x in r.carrier ==> r(x,f(x))


{:
# Complete partial order
:}

is_complete_partial_order(r:{A,A}): ghost BOOLEAN
    -> r.is_partial_order
       and
       all(d) d.is_updirected(r) ==> d.has_supremum(r)




{:
# Wellorder
:}


nonempty_has_least(r:{A,A}): ghost BOOLEAN
        -- Do all nonempty subsets of the carrier have a least element?
    -> all(p) p <= r.carrier ==> p.has_some ==> some(x) x.is_least(p,r)

all(r:{A,A})
    require
        r.nonempty_has_least
    ensure
        r.is_dichotomic
    assert
        all(x,y)
            require
                {x,y} <= r.carrier
            ensure
                r(x,y) or r(y,x)
            via some(z)
                z.is_least({x,y},r)
            if z in {x}
            orif z in {y}
            end
    end


is_wellorder(r:{A,A}): ghost BOOLEAN
        -- Is 'r' a wellorder i.e. a linear order where every nonempty set
        -- has a least element?
    -> r.is_linear_order
       and
       r.nonempty_has_least




{:
# Wellfounded
:}


accessibles (r:{A,A}): ghost {A}
        -- The elements which are accessible by the relation 'r' i.e. all
        -- elements in the carrier of 'r' which have no predecessor or whose
        -- predecessors are all accessible.
    -> {(p): all(y) y in r.carrier ==> (all(x) r(x,y) ==> x in p) ==> y in p}


is_wellfounded (r:{A,A}): ghost BOOLEAN
        -- Is the relation 'r' wellfounded i.e. are all its elements accessible?
    -> r.carrier <= r.accessibles


all(r:{A,A})
    ensure
        r.accessibles <= r.carrier
    assert
        all(a)
            require
                a in r.accessibles
            ensure
                a in r.carrier
            inspect
                a in r.accessibles
            end
    end




{:
# Closure system

A closure system in a partial order is a subset of the carrier such that all
subsets of the closure system have an infimum in the closure system. I.e. the
closure system is "closed" with respect to the infimum operation.

Because of this property it is always possible to find a least element in the
closure system which is above another element of the carrier.

A weak closure system is not completely closed with respect to the infimum
operation. Empty subsets of the closure system are excluded. However a weak
closure system must be big enough to contain arbitrarily large elements. The
last property is implicit in a normal closure system.

:}

is_weak_closure_system (p:{A}, r:{A,A}): ghost BOOLEAN
    -> r.is_partial_order
       and
       p <= r.carrier
       and
       (all(x)
            x in r.carrier ==>
            (p * x.upper_set(r)).has_some)  -- p is sufficiently large
       and
       all(q)
           q <= p ==> q.has_some
           ==>
           some(x) x.is_infimum(q,r) and x in p



is_closure_system (p:{A}, r:{A,A}): ghost BOOLEAN
    -> r.is_partial_order
       and
       p <= r.carrier
       and
       all(q)
           q <= p
           ==>
           some(x) x.is_infimum(q,r) and x in p


is_closure_map (f:A->A, r:{A,A}): ghost BOOLEAN
    -> f.is_total(r) and
       f.is_increasing(r) and
       f.is_monotonic(r) and
       f.is_idempotent


all(a:A, p:{A}, r:{A,A})
        -- For all elements there is always a least element in the closure system
        -- system above the element.
    require
        p.is_weak_closure_system(r)
        a in r.carrier
    ensure
        (p * a.upper_set(r)).has_least(r)

    assert
        all(q)
            require
                q = p * a.upper_set(r)  -- q represents all elements which are in
                                        -- the closure system above a
            ensure
                q.has_least(r)

            assert
                q <= p
                q.has_some
            via some(x) x.is_infimum(q,r) and x in p  -- def 'weak_closure_system'
            assert
                ensure
                    x.is_least(q,r)
                {: x is the infimum of q i.e. the greatest lower bound. a is a
                   lower bound of q. Therefore r(a,x) must be valid and x must
                   be in q. An infimum which is in a set is the least element
                   of the set.  :}
                assert
                    r.is_preorder
                    r(a,x)
                end
            end
    end



all(p:{A}, r:{A,A})
        -- A closure system is a weak closure system.
    require
        p.is_closure_system(r)
    ensure
        p.is_weak_closure_system(r)

    assert
        all(x)
            require
                x in r.carrier
            ensure
                (p * x.upper_set(r)).has_some
            assert
                x in x.upper_set(r)
            via some(z) z.is_infimum(empty,r) and z in p -- p is a closure system
            assert
                ensure
                    z in p * x.upper_set(r)
                {: The lower bounds of the empty set consists of the whole carrier
                   of the relation. Therefore the infimum of the empty set is the
                   greatest element of the carrier which is certainly in the upper
                   set of 'x'. :}
                assert
                    r.is_preorder
                    r(x,z)
                end
            end
    end




closed(a:A, p:{A}, r:{A,A}): ghost A
        -- The least element above 'a' of the closure system 'p' in the
        -- partial order 'r'.
    require
        p.is_weak_closure_system(r)
        a in r.carrier
    ensure
        -> least (p * a.upper_set(r), r)
    end



{:# Interior systems
:}


is_interior_system (p:{A}, r:{A,A}): ghost BOOLEAN
    -> r.is_partial_order
       and
       p <= r.carrier
       and
       all(q) q <= p ==> some(x) x.is_supremum(q,r) and x in p


is_weak_interior_system (p:{A}, r:{A,A}): ghost BOOLEAN
    -> r.is_partial_order
       and
       p <= r.carrier
       and
       (all(x) x in r.carrier ==> (p * x.lower_set(r)).has_some)
       and
       all(q)
           q <= p ==> q.has_some
           ==>
           some(x) x.is_supremum(q,r) and x in p


all(p:{A}, r:{A,A})
        -- Duality
    require
        p.is_interior_system(r)
    ensure
        p.is_closure_system(r.inverse)
    assert
        all(q)
            require
                q <= p
            ensure
                some(x) x.is_infimum(q,r.inverse) and x in p
            via
                some(x) x.is_supremum(q,r) and x in p
            assert
                x.is_infimum(q,r.inverse) and x in p
            end
    end
