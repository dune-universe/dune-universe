use
    predicate
    list
end

G:ANY

class
    BINARY_TREE[G]
create
    leaf
    tree(info:G, left,right:BINARY_TREE[G])
end


preorder (t:BINARY_TREE[G]): [G]
    -> inspect t
       case leaf        then []
       case tree(i,l,r) then i ^ (l.preorder + r.preorder)

inorder (t:BINARY_TREE[G]): [G]
    -> inspect t
       case leaf        then []
       case tree(i,l,r) then l.inorder + i ^ r.inorder

postorder (t:BINARY_TREE[G]): [G]
    -> inspect t
       case leaf        then []
       case tree(i,l,r) then l.postorder + r.postorder + [i]


(-) (t:BINARY_TREE[G]): BINARY_TREE[G]
    -> inspect t
       case leaf        then leaf
       case tree(i,l,r) then tree(i, -r, -l)


all(t:BINARY_TREE[G])
    ensure
        - - t = t
    inspect
        t
    end


all(t:BINARY_TREE[G])
    ensure
        (- t).inorder = - t.inorder
    inspect
        t
    case tree(i,l,r)
        via [ (- tree(i,l,r)).inorder
            , - r.inorder + ([i] + - l.inorder)    -- def
            , - r.inorder + [i]  + - l.inorder     -- assoc
            , - i ^ r.inorder    + - l.inorder     -- - a + [x] = - x ^ a
            , - (l.inorder + i ^ r.inorder)        -- - a + - b = - (b + a)
            , - tree(i,l,r).inorder                -- def
            ]
    end


all(t:BINARY_TREE[G])
    ensure
        (-t).preorder = - t.postorder
    inspect t
    case tree(i,l,r)
        assert
            (-r.postorder) + - l.postorder = - (l.postorder + r.postorder)
        via [ (-tree(i,l,r)).preorder
            , i ^ ((-r).preorder + (-l).preorder)
            , i ^ (-r.postorder + -l.postorder)
            , i ^ - (l.postorder + r.postorder)
            , -(l.postorder + r.postorder + [i])
            , - tree(i,l,r).postorder
            ]
    end
