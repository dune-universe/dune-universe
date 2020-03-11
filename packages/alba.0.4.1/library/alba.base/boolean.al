use core end


{: Negated Implication
   =================== :}

all(a,b:BOOLEAN)
    require
        not (a ==> b)
    ensure
        a
    via require not a
    end

all(a,b:BOOLEAN)
    require
        not (a ==> b)
    ensure
        not b
    end

all(a,b:BOOLEAN)
    require
        not (a ==> not b)
    ensure
        b
    via require not b
    end



{: de Morgan 1: 'not (a or b) = not a and not b'
   ============================================= :}


all(a,b:BOOLEAN)
    require
        not (a or b)
    ensure
        not a
    end


all(a,b:BOOLEAN)
    require
        not (a or b)
    ensure
        not b
    end


all(a,b:BOOLEAN)
    require
        not a
        not b
    ensure
        not (a or b)
    end




{: de Morgan 2: 'not (a and b) = not a or not b'
   ============================================= :}



all(a,b:BOOLEAN)
    require
        not (a and b)
    ensure
        not a or not b
    via require
        not (not a or not b)
            {: apply 'not (x or y) ==> not x' and  'not (x or y) ==> not y'
               and then the double negation law to derive a contradiction
               with the premise 'not (a and b)' :}
    end


all(a,b:BOOLEAN)
    require
        not a or not b
    ensure
        not (a and b)
        if not a
        orif not b
    end




{: Commutativity of 'or'
   ===================== :}


all(a,b:BOOLEAN)
    require
        a or b
    ensure
        b or a
        if a orif b
    end







{: Associativity of 'or'
   ==================== :}



all(a,b,c:BOOLEAN)
    require
        a or b or c
    ensure
        a or (b or c)
        if a or b
            if a orif b
        orif c
    end



all(a,b,c:BOOLEAN)
    require
        a or (b or c)
    ensure
        a or b or c
        if a
        orif b or c
            if b orif c
    end



{: 'or' and '==>'
   ============== :}

all(a,b:BOOLEAN)
    require
        a or b
        not a
    ensure
        b
    if a
        assert false
    orif b
    end

all(a,b:BOOLEAN)
    require
        not a ==> b
    ensure
        a or b
        if a orif not a
    end
