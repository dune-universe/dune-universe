@type ('a, 'b) a = A of 'a | B of 'b with show, gmap
@type ('a, 'b) t = X of ('a, 'b) a * ('a, 'b) t GT.list | 
                   Y of ('b, 'a) a * ('a, 'b) t GT.list with show, gmap
