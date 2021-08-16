@type ('a, 'b) a = [`A of 'a | `B of 'b] with show, eq
@type ('a, 'b) b = [('b, 'a) a] with show, eq
