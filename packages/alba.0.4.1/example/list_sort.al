use
    alba.base.list
    alba.base.linear_order
end


L: LINEAR_ORDER


into (x:L, a:[L]): [L]
    -> inspect
           a
       case [] then
           [x]
       case y ^ a then
           if x <= y then
               x ^ y ^ a
           else
               y ^ x.into(a)


sorted(a:[L]): [L]
     -> inspect
            a
        case [] then
            []
        case x ^ a then
            x.into(a.sorted)




sorted_lists: ghost {[L]}
    = {(sorted):
           [] in sorted,
           all(x) [x] in sorted,
           all(x,y,a) x <= y ==> y ^ a in sorted ==> x ^ y ^ a in sorted
      }



all(x:L, a:[L])
    require
        a in sorted_lists
    ensure
        x.into(a) in sorted_lists
    inspect
        a in sorted_lists
    case all(y:L) [y] in sorted_lists
        if x <= y
        else
    case all(y,z:L,a)
        y <= z ==> z ^ a in sorted_lists ==> y ^ z ^ a in sorted_lists
        assert
            x.into(z ^ a) in sorted_lists  -- ind hypo
            -- goal: x.into(y ^ z ^ a) in sorted_lists
            -- nontrivial case: x > y and x > z
        if x <= y
        else
            if x <= z
            else
                assert
                    ensure
                        z ^ x.into(a) in sorted_lists
                    assert
                        x.into(z ^ a) = z ^ x.into(a)
                    end
                    y ^ z ^ x.into(a) = x.into(y ^ z ^ a)
                    x.into(y ^ z ^ a) in sorted_lists  -- goal
    end

all(a:[L])
    ensure
        a.sorted in sorted_lists
    inspect
        a
    end




all(x:L, a:[L])
    ensure
        permutation(x ^ a, x.into(a))
    inspect
        a
    case y ^ a
        if x <= y
            assert
                x ^ y ^ a = x.into(y ^ a)
                x.into(y ^ a) in {b: permutation(x ^ y ^ a, b)}
        else
            assert
                permutation(x ^ a, x.into(a))             -- ind hypo
                permutation(y ^ x ^ a, y ^ x.into(a))     -- rule: prefix element
                x.into(y ^ a) = y ^ x.into(a)             -- def 'into'
            via [ x ^ y ^ a
                , y ^ x ^ a
                , y ^ x.into(a)
                , x.into(y ^ a)
                ]
    end

all(a:[L])
    ensure
        permutation(a, a.sorted)
    inspect
        a
    case x ^ a
        assert
            permutation(a, a.sorted)                    -- ind hypo
            (x ^ a).sorted = x.into(a.sorted)    -- def: 'sorted'
            -- goal: permutation(x ^ a, (x ^ a).sorted)

        via [ x ^ a
            , x ^ a.sorted        -- ind hypo
            , x.into(a.sorted)    -- previous theorem
            , (x ^ a).sorted      -- def 'sorted'
            ]
    end
