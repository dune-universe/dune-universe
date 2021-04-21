# Mimic, a full-abstract way to instantiate a transmission protocol

`mimic` is a small project which gives you the opportunity to instantiate a
transmission protocol - such as a TCP/IP connection - from dynamic values. A
simple tutorial is available [here][tutorial]. It explains to implement a
ping-pong protocol and upgrade it to TLS.

## Some examples

[git][git] or [paf][paf] are examples where they use `mimic` as the only
transmission protocol implementation available. It permits to be compatible
with MirageOS without the complexity of _functors_ (commonly used with
[functoria][functoria] to unlock the possibility to abstract anything).

## Design

`mimic` is pretty-small (~ 700 lines) and the API wants to fit into several
different contexts (HTTP, [TLS][tls] or [SSH][ssh]). It's possible to make
helpers from it such as some derivations for `unix` or `mirage` - as we
commonly designed for [conduit][conduit]. However, with a big retro-spective,
such piece of code should **not** include these derivations.

Indeed, they give an opportunity to the user to assert a non-compaibility with
MirageOS if you use the `unix` derivation for example.

`mimic` wants to be abstract and "simple". Then, the user is able to construct
something more complex and easy to use at his level - and it's what [paf][paf]
does for example or [git-unix][git-unix].

## Reverse dependencies

`mimic` must be thought according to who use it. The API is not designed to be
canonic and usable as is. It has been thought to unlock the full abstraction
and the compatibility with MirageOS for others projects.

If you think that you can have an usage of `mimic` and something is missing,
you should implement what you want **outside** `mimic`.

## The `Mirage_flow.S` interface

Finally, the only assumption about design of protocols, transmission protocols,
etc. is `Mirage_flow.S`. Several issues exist about this interface but the cost
to upgrade the interface (to be unix-friendly for example) is huge when
several MirageOS projects trust on this specific interface.

## Documentation

`mimic` can be hard to explain when we don't know all details about the
MirageOS eco-system. The existence of this project can be critized when we
don't really understand all details and how this project fits in.

The documentation is not very clear and does not explain the big-picture of
`mimic`. So it's a real issue and the [tutorial][tutorial] wants to fix it but
my lack of English does not help me.

[tutorial]: https://dinosaure.github.io/mimic/
[git]: https://github.com/mirage/ocaml-git
[paf]: https://github.com/mirage/paf-le-chien
[functoria]: https://github.com/mirage/mirage
[tls]: https://github.com/mirleft/ocaml-tls
[ssh]: https://github.com/mirage/awa-ssh
[git-unix]: https://github.com/mirage/ocaml-git
