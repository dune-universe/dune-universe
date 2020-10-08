@type a = [`A of GT.int | `B of b] with show
and   b = [`C of GT.string | `D of a] with show

@type c = [a | b] with show
