module M : sig
@type 'a t1 = [`A | `B of 'a] with show, gmap
@type 'a t2 = [`C | `D of 'a] with show, gmap
@type 'a t  = ['a t1 | 'a t2] with show, gmap
end
