(** [Proxy : 'a] is a proxy for type ['a], allowing one to pass the type around
    without having to present a witnessing value.

    See Haskell's
    {{:http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Proxy.html#t:Proxy}
    Proxy}. *)
type 'a proxy = Proxy
