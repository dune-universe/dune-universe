@type 'a a = [`A of 'a | `B of GT.string] with show, eq, compare
@type b = [`C of GT.int | `D of GT.string] with show, eq, compare
@type 'a c = ['a a | b] with show, eq, compare
