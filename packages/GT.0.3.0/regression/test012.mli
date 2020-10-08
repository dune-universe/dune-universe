@type a = [`A of GT.int | `B of GT.string] with show, eq, compare
@type b = [`C of GT.int | `D of GT.string] with show, eq, compare
@type c = [a | b] with show, eq, compare
