{:------------------------------------------------------------------------:}
class BOOLEAN
{:------------------------------------------------------------------------:}

false: BOOLEAN               note built_in end
(==>) (a,b:BOOLEAN): BOOLEAN note built_in end


-- negation ---------------------------------------------------------------
(not) (a:BOOLEAN): BOOLEAN   -> a ==> false

all(a:BOOLEAN)
    ensure
        not not a ==> a    -- double negation
    note axiom
    end

all(a:BOOLEAN)
        -- indirect proof
    require
        not a ==> false
    ensure
        a
    assert
        not not a  -- can be derived from the assumption and
                   -- proves 'a' by the double negation law
    end

all(a:BOOLEAN)
    require
        false
    ensure
        a
    via require
        not a
    end

true: BOOLEAN                = false ==> false


all
    ensure
        true
    end

-- conjunction -------------------------------------------------------------
(and) (a,b:BOOLEAN): BOOLEAN -> not (a ==> b ==> false)


all(a,b:BOOLEAN)
        -- 'and' elimination 1
    require
        a and b
    ensure
        a
        via require not a
    end

all(a,b:BOOLEAN)
        -- 'and' elimination 2
    require
        a and b
    ensure
        b
        via require not b
    end


all(a,b:BOOLEAN)
        -- 'and' introduction
    ensure
        a ==> b ==> a and b
    end

(and) (a,b:BOOLEAN):BOOLEAN   -- definition hiding



-- disjunction -------------------------------------------------------------
(or)  (a,b:BOOLEAN): BOOLEAN -> not a ==> b


all(a,b:BOOLEAN)
        -- 'or' introduction
    require
        a
    ensure
        a or b
    assert
        require
            not a
        ensure
            b
            via require not b
        end
    end


all(a,b:BOOLEAN)
        -- 'or' introduction 2
    ensure
        b ==> a or b
    end




all(a,b,c:BOOLEAN)
        -- 'or' elimination
    require
        a or b
        a ==> c
        b ==> c
    ensure
        c
        via require not c
        assert
            not a  -- contrapositive of 'a ==> c'
            b      -- def 'a or b'
            c
    end


all(a:BOOLEAN)
    require
        a or a
    ensure
        a
        if a
        orif a
    end


all(a:BOOLEAN)
    ensure
        a or not a
    end

all(a,b:BOOLEAN)
    ensure
        a or b ==> not a ==> b
    end


all(a,b:BOOLEAN)
    ensure
        (not a ==> b) ==> a or b
    end

(or) (a,b:BOOLEAN):BOOLEAN   -- definition hiding



-- equivalence -------------------------------------------------------------
(=)   (a,b:BOOLEAN): BOOLEAN -> (a ==> b) and (b ==> a)

all(a:BOOLEAN)
    ensure
        a = a
    end



{:------------------------------------------------------------------------:}
deferred class A:ANY
{:------------------------------------------------------------------------:}

(=)  (a,b:A): BOOLEAN    deferred end

(/=) (a,b:A): BOOLEAN -> not (a = b)

all(a:A)
    ensure
        a = a
    note
        axiom
    end


all(a:A)
    require
        a /= a
    ensure
        false
    end

all(a,b:A)
    ensure
        a = b or a /= b
    if a = b
    else
    end


class
    BOOLEAN
inherit
    ANY
end


{:------------------------------------------------------------------------:}
class PREDICATE[A]
{:------------------------------------------------------------------------:}


(in)  (e:A, p:{A}): BOOLEAN
(/in) (a:A, p:{A}): BOOLEAN -> not p(a)

(<=) (p,q:{A}): ghost BOOLEAN -> all(x) p(x) ==> q(x)


(=) (p,q:{A}): ghost BOOLEAN -> p <= q and q <= p



all(p:{A}) ensure p = p end


class
    PREDICATE[A]
inherit
    ghost ANY
end


all(a,b:A)
        -- leibniz rule
    require  a = b
    ensure   all(p:{A}) p(a) ==> p(b)
    note     axiom end

all(a:A) ensure a = a end
all(a,b:A)
        -- symmetry of equality
    require
        a = b
    ensure
        b = a
    assert
        b in {x: x = a}
    end

all(a,b,c:A)
        -- transitivity of equality
    require
        a = b
        b = c
    ensure
        a = c
    end





{: Empty and universal set
   ======================= :}

has_some (p:{A}): ghost BOOLEAN
    -> some(x) x in p

is_empty (p:{A}): ghost BOOLEAN
        -- Is the set 'p' empty?
    -> not p.has_some

is_universal (p:{A}): ghost BOOLEAN
        -- Is the set 'p' the universal set?
    -> all(x) x in p

all(p:{A})
    require
        p.is_empty
    ensure
        all(a) a /in p
    end


empty:{A}     = {x: false}

universal:{A} = {x: true}


{: Indirect Proof
   ==============
:}


all(a:A, p:{A})
    require
        a /in p ==> false
    ensure
        a in p
    via require
        not (a in p)
    end



{:------------------------------------------------------------------------:}
B:ANY
class FUNCTION[A,B]
{:------------------------------------------------------------------------:}


domain (f:A->B): ghost {A}  note built_in end

undefined: (A->B)           note built_in end

is_total (f:A->B): ghost BOOLEAN
    -> f.domain.is_universal

consistent (f,g:A->B): ghost BOOLEAN
    -> all(x) x in f.domain  ==>  x in g.domain  ==> f(x) = g(x)


(=) (f,g:A->B): ghost BOOLEAN
    -> f.domain = g.domain and consistent(f,g)


all(f:A->B)
    ensure
        consistent(undefined,f)
        note axiom
    end

all(f:A->B)
    ensure
        consistent(f,f)
    end

all(f,g:A->B)
    ensure
        consistent(f,g) ==> consistent(g,f)
    end

all(f:A->B) ensure f = f end




class
    FUNCTION[A,B]
inherit
    ghost ANY
end




range (f:A->B): ghost {B}
    -> {y: some(x) x in f.domain and f(x) = y}


[] (f:A->B, p:{A}): ghost {B}
    -> {y: some(x) x in p and x in f.domain and f(x) = y}


all(x:A, f:A->B)
    require
        x in f.domain
    ensure
        f(x) in f.range
    assert
        x in f.domain and f(x) = f(x)
    end

all(f:A->B)
    ensure
        f[f.domain] <= f.range
    assert
        all(y)
            require
                y in f[f.domain]
            ensure
                y in f.range
            via some (x)
                    x in f.domain and
                    x in f.domain and
                    f(x) = y
            end
    end

all(f:A->B)
    ensure
        f.range <= f[f.domain]
    assert
        all(y)
            require
                y in f.range
            ensure
                y in f[f.domain]
            via some(x) x in f.domain and f(x) = y
                assert
                    x in f.domain and x in f.domain and f(x) = y
            end
    end


all(p,q:{A}, f:A->B)
    require
        p <= q
    ensure
        f[p] <= f[q]
    assert
        all(y)
            require
                y in f[p]
            ensure
                y in f[q]
            via some(x) x in p and x in f.domain and f(x) = y
            assert
                x in q and x in f.domain and f(x) = y
            end
    end


origin (q:{B}, f:A->B): ghost {A}
    -> {x: x in f.domain and f(x) in q}


all(f:A->B)
    ensure
        f.domain <= f.range.origin(f)
    end

all(f:A->B)
    ensure
        f.range.origin(f) <= f.domain
    end




{:------------------------------------------------------------------------:}
class
    TUPLE[A,B]
create
    tuple(first:A, second:B)
end
{:------------------------------------------------------------------------:}



first (t:(A,B)): A
    -> inspect
           t
       case (a,_) then
           a

second (t:(A,B)): B
    -> inspect
           t
       case (_,b) then
           b


-- Only for demonstration of a call with explicit arguments
all(a:A,b:B)
    ensure
        first(a,b) = a
    end

{:------------------------------------------------------------------------:}
class
    LIST[A]
create
    []
    (^) (head:A, tail:[A])   -- [A] is a shorthand for LIST[A]
end
{:------------------------------------------------------------------------:}


{:------------------------------------------------------------------------:}
class
    MAYBE[A]
create
    nothing
    just (item:A)
end
{:------------------------------------------------------------------------:}



{:------------------------------------------------------------------------:}
class NAT
{:------------------------------------------------------------------------:}

(=) (a,b:NAT): BOOLEAN    note built_in end


class
    NAT
inherit
    ANY
end

0: NAT                     note built_in end
greatest: NAT              note built_in end
successor (n:NAT): NAT     note built_in end
predecessor (n:NAT): NAT   note built_in end
(+)  (a,b:NAT): NAT        note built_in end
(-)  (a,b:NAT): NAT        note built_in end
(*)  (a,b:NAT): NAT        note built_in end
(mod)(a,b:NAT): NAT        note built_in end
(/)  (a,b:NAT): NAT
    require
        b /= 0
    note built_in end

(<=) (a,b:NAT): BOOLEAN     note built_in end

upper_set (n:NAT): ghost {NAT}
    -> {(p): n in p,
             all(n) n /= greatest ==> n in p ==> n.successor in p}

all(a,b,n:NAT, p:{NAT})
    ensure
        n /= n.successor
        n = greatest ==> n.successor = 0
        n.successor.predecessor = n

        (n /= 0) = (some(m) m /= greatest and n = m.successor)

        0 in p
        ==> (all(n) n /= greatest ==> n in p ==> n.successor in p)
        ==> n in p

        a + 0 = a
        a + b.successor = (a + b).successor

        a * 0 = 0
        a * b.successor = (a * b) + a

        (a <= b) = (b in a.upper_set)
    note
        axiom
    end

all(a:NAT)
    ensure
        a = 0 ==> a /= 0 ==> false
    end
