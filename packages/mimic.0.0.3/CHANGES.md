### 0.0.3 (2021-20-04) Paris - France

- Move the project to https://github.com/dinosaure/mimic (@dinosaure)
  Old distributions of `mimic` are still available on
  https://github.com/mirage/ocaml-git but `mimic` starts to be
  used by others projects than `ocaml-git`. We decided to make
  its own repository.
- Take the most recent value in the `ctx` instead of the older one
  **breaking changes**
  When `mimic` wants to instantiate a transmission protocol, if
  a value `'a Mimic.value` was inserted multiple times, `mimic`
  took the older one to instance the transmission protocol.

  Now, `mimic` takes the newer one. It useful when we want to
  implement the rediction in HTTP where we need to "replace" values
  by the new destination.

### 0.0.2 (2021-31-03) Paris - France

- Add documentation (#494, @dinosaure)
- Optimize projection of modules (#495, @dinosaure)
- Remove `hmap` dependency (which is vendored and tweaked)
  (fe55e14, @dinosaure)

### 0.0.1 (2021-08-01) Paris - France

- First release of `mimic`
