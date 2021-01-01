@type a = A of b | C of GT.int GT.list with show
and   b = B of c | D of GT.string with show
and   c = E of a with show
