# v0.6.2 (2020-04-02)

Minor release with one new feature.

- Can now build with OCaml v4.10
- New feature: Traversable.Const, an implementation of a traversal that
  type-checks, but does nothing.

# v0.6.1 (2019-11-27)

Minor release cut to relax various dependency pain-points caused by the last
release being more than 4 months ago.  (I intend to make a major release in
the near future with a migration from monads to applicative functors, but
this depends on enough free time being available.)

- Travesty now expects Dune 2.0 to build.  This is technically a breaking
  change, but I felt that bumping to 0.7 at this stage of Travesty's lifecycle
  would give the wrong impression.
- Travesty now lets itself be built with v0.13 of the Jane Street packages.
- Travesty now lets itself be built with OCaml v4.09.
- Minor code formatting changes.

# v0.6.0 (2019-06-25)

Major release with various breaking changes and new features, the main one
being bi-traversables.

## Breaking changes

- Most module signatures have moved from `XYZ` to `XYZ_types`.  For example,
  `Bi_mappable.S2` is now `Bi_mappable_types.S2`.  This is to eliminate
  the 'intf pattern' previously used in Travesty.
- Removed `Travesty_core_kernel_exts`.  Use `Travesty_base_exts`
  instead.
- Made `Bi_mappable.Make2`'s return module type use sharing constraints
  instead of destructive substitutions.  This may cause shadowing where there
  previously wasn't any.

## Bi_traversable

This release adds a `Bi_traversable` module---effectively being to
`Traversable` as `Bi_mappable` is to `Mappable`.

`Alist`, `Tuple2`, and `Or_error` now expose `Bi_traversable.S2`, which
subsumes their previous interface.

## Biffs

This release also adds the ability to compose `Mappable`s on the inside of
`Bi_mappable`s (and `Traversables` on the inside of `Bi_traversables`): what
is often referred to as a 'biff' in the quirky world of Haskell.  This
complements the ability to compose them on the outside (a 'tannen').

## Minor improvements

- Removed spurious dependencies that caused travesty to be unavailable on
  OCaml 4.08.
- `Bi_mappable` arity-1 chaining functors now carry previously-missing
  sharing constraints equating their fixed types with those of their
  bi-mappable ancestor.
- Added experimental F#-style `>>` operator in
  `Travesty_base_exts.Fn.Compose_syntax`.
- Added bi-traversable instance for `Result`, from which `Or_error` now
 inherits.

# v0.5.1 (2019-05-10)

As is becoming tradition, fixes a minor documentation comment caught
between tagging on GitHub and publishing on OPAM.  See the main changelog
for details about what changed in v0.5.0.

# v0.5.0 (2019-05-10)

Major release with new features and breaking changes, especially to module
names and structures.

## Breaking changes

See also the changes for `v0.4.x`, as that version didn't reach OPAM.

### Extension modules

In a partial reversal of changes done in `v0.4.x`, the `Travesty_base_exts`
and `Travesty_core_kernel_exts` modules no longer re-export the `Base` and
`Core_kernel` modules on which they are based.

This means that any code using these extensions will need to open both
original and extension modules (for example, by opening `Travesty_base_exts`
as `Tx` and using `Tx.List` when Travesty extensions are needed.

This change avoids a large amount of coupling, inefficiency,
and possible legal issues caused by including swathes of third
party libraries.

### Traversable module renaming

The functors, and signatures in `Traversable` have been renamed drastically
to make them more consistent, and provide a better distinction between
'in-monad' and 'out-of-monad' signatures:

- `SX` is now `BasicX_on_monad`, to reflect that it
  only refers to the basic inner `On_monad` part of a traversable;
- `Generic` is now `Generic_basic_on_monad`, as per the above;
- `On_monad1` is now `S1_on_monad`.  There is no `S0_on_monad` yet, but this
   may change.
- `SX_container` is now just `SX`, as it forms the main output of the
  `Traversable` functors;
- `GenericX_container` is now just `GenericX`, to follow suit;
- `BasicX` is unchanged;
- `Basic_containerX` is now `BasicX_container`;
- `Extend_containerX` is now `MakeX_container` (as it makes an `SX` from a
  `BasicX_container`);
- `Make_containerX` is now `MakeX` (as it transforms a `BasicX`).

### Bi_mappable module renaming

Similarly, `Bi_mappable`'s functors and signatures have been renamed:

- `SX_with_extensions` is now just `SX`;
- The various `Extensions` modules no longer exist;
- The `Extend` functors are now `Make` functors.

### Other

- `Traversable.S1` no longer carries `With_elt`.  This is now a separate
  functor in `Traversable` called `Fix_elt`.
- The various `Bi_mappable.Extensions` signatures no longer carry
  `Fix_left`, `Fix_right`, `Map_left`, and `Map_right`.  These are now
  separate functors, and don't apply the extensions by default.
- See `when_m` and `unless_m` below: these now have an optional argument,
  which may cause breakage in rare situations.
- Various modules and functor targets that previously substituted destructively
  now output sharing constraints.  This means that, this side of OCaml 4.08,
  you may need to wrap some functor results in a
  `module type of X with type t := t` stanza.  This is to make it easier to
  chain together such modules.

## New features

### `Bi_mappable`

- New functors for fixing left and right types, as well as converting
  bi-mappables to mappables by focusing on one type.  These replace the
  modules previously generated by `Bi_mappable.Extensions`.
- Added several more composition functors.
- Conversion functors no longer destructively substitute.

### Extended `Option`

- Now includes `Monad_exts`.

## Extended `Or_error`

- Now a bi-mappable type.

### `Monad_exts`

- `M.when_m` and `M.unless_m` now take an optional parameter `?otherwise`,
  which allows generalising the application of `M.return` when the
  condition doesn't hold.
- New functions `M.map_when_m` and `M.map_unless_m`, which generalise
  `when_m` and `unless_m` to take an arbitrary `'a` (and pass it to `~f`).
- New functions `then_m` and `>>`, which act as Haskell's `>>` (then) operator.
- New functions `compose_m` and `>=>`, which act as Haskell's `>=>` (Kleisli
  composition) operator.

# v0.4.1 (2019-05-07)

Minor documentation fixup release.  No other changes since v0.4.0.
(Not released on OPAM.)

# v0.4.0 (2019-05-07)

Major release with incompatible name and library division changes.
(Not released on OPAM.)

## Breaking changes

The main change in this release is that all 'extension' modules (`T_xyz`)
have been renamed or moved into subpackages of `Travesty`:

- `Singleton` now lives in `Travesty_containers`.
- `T_monad` and `T_container` have changed to `Monad_exts` and `Container_exts`;
- Every other `T_` module now lives in either `Travesty_base_exts` or
  `Travesty_core_kernel_exts`, and no longer has the `T_` suffix.
- Modules in `Base_exts` depend on, and extend, the `Base` version of their
  namesake module.
- Modules in `Core_kernel_exts` depend on, and extend, the `Core_kernel` version
  of their namesake module.  Usually, they do so by importing the extensions from
  the `Base_exts` version on top of an import of the `Core_kernel` baseline module.

Other breaking changes:

- `Fn.on` now takes its second argument with the label `~f`.

## New features

This release contains a large amount of 'small' new features.  Most of these
are of minor extensions and convenience functions on top of
`Base` and `Core_kernel`

- Submodule split: Travesty-unique containers are now in `Travesty_containers`
  (dune: `travesty.containers`); extensions to Base containers are in
  `Travesty_base_exts` (`travesty.base_exts`); similar extensions to
  Core_kernel containers are in `Travesty_core_kernel_exts`.
- Add `Monad_exts.tee`, which is a counterpart to `tee_m` that accepts a
  non-monadic function.  (This is somewhat less useful, but still helps in
  terms of slotting, say, debug printing into a monadic pipeline.)
- Extensions: add `Or_error.combine_map[_unit]`, which are shorthand for
  mapping followed by `combine_errors[_unit]`.  These should be used
  instead of `map_m` and `iter_m` when using lists and `Or_error`.
- Extensions: add `Tuple2`, an extended `Core_kernel.Tuple2` adding bi-mappability.
- Add chaining for arity-2 bi-mappable containers across arity-1 mappable
  containers.  We now implement `Alist`'s bi-mappable interface using
  this and `Tuple2`.
- `Fn`: add `always`, which behaves like `const true`; and `never`, which
  behaves as `const false`.
- `Alist`: add `compose`, which is the relational composition on two associative
  lists.
- `Travesty_containers.Zipper`: implementations of list zippers (imported from
  `act`, and subject to change).
- `List` extensions: added `With_errors.replace_m`, `replace`, and `insert`
  functions (imported from `act`, and subject to change).
  Future versions may generalise `replace_m` to act on any monad.
- `List.Assoc` in the extension modules is now an alias for `Alist`,
  bringing the bifunctor extensions into scope there.

## Other

- `Bi_mappable`: `Fix_left` and `Fix_right`'s signatures no longer destructively
  substitute `t`, so  `Alist.Fix_left(String).t` should now work.

# v0.3.0 (2019-03-03)

Major release with incompatible dependency and name changes.

## Breaking changes

- Now targeting v0.12 of Jane Street's upstream libraries.  This release of
  travesty no longer supports v0.11.
- As a result, travesty no longer supports OCaml 4.06; please use 4.07+.
- Traversable signature names have changed: `Basic_container0` is now `Basic0`,
  and `Basic_container1` is now `Basic1`.  The original names are now used for
  stronger interfaces that include implementations of `Container.S*`; see
  'new features' below for information.

## New features

- Add `T_container.Extensions0` and `Extend0`, which generalise most
  of `Extensions1`/`Extend1` to arity-0 containers.
- Generalise `T_container`'s predicate extensions (`any`/`all`/`none`)
  over arity-0 containers, provided that their `elt` is `x -> bool` for
  some `x`.
- Add `Bi_mappable`, an implementation of bifunctors.
- Add `T_alist`, an extended form of `List.Assoc`.
- Split the Traversable container functors into two kinds: the
  `Make_container*` functors now take `Basic*` signatures (but are otherwise
  the same---they still produce their own `Container.S*` instances); the new
  `Extend_container*` functors take the now-stronger `Basic_container*`
  signatures, which include custom implementations of `Container.S*`, and
  use those instead.  The idea is that `Make` is for building new containers
  from traversals, and `Extend` is for adding traversals to existing containers.

## Other

- `T_list` and `T_option` now use `Extend_container1` internally: the upshot
  of this is that they re-use the existing Core implementations of container
  operations where possible, rather than (slowly) re-building them using
  `fold_m`.

# v0.2.0 (2018-12-23)

Major release.

## Breaking changes

- _Potentially breaking change:_ `Traversable.S0_container` now
  contains `module Elt : Equal.S`, and constrains `type elt` to be
  equal to `Elt.t`.  This reflects the situation in
  `Basic_container0`, and shouldn't break any code using
  `Make_container0`, but may cause custom-built modules to fail to
  type-check.
- `T_container.any`'s arguments have swapped order, to be more
  in line with `Core` idioms.

## New features

- Add `Traversable.Chain0`, a functor for combining two
  `S0_container` instances together for nested traversal.
- Add `T_fn.disj` to go with `T_fn.conj`.
- Add `Filter_mappable`, which generalises `List.filter_map`.
- Add `tee_m` to monad extensions.  This is a small wrapper over
  `f x >>| fun () -> x` that allows unit-returning monadic
  side-effects to be treated as part of a monad pipeline.
- Add `T_or_error`: monad extensions for `Core.Or_error`.
- `one` and `two` are now implemented on `T_container`, not just
  `T_list`.  The errors are slightly less precise, but otherwise
  nothing has changed.
- Add `T_container.at_most_one` to complement `one` and `two`.
- Add `Monad.To_mappable`, which makes sure that monads can be
  converted to mappables.
- Add `T_container.all` and `none`, to complement `any`.

## Other

- Improve API documentation.

# v0.1.3 (2018-12-13)

Bugfix release.

- Fix incorrect module name (was `Lib`, not `Travesty`).
- Restrict to OCaml v4.06+ (this was the case in the final v0.1.2
  OPAM release, but not upstream).

# v0.1.2 (2018-12-12)

Bugfix and minor improvement release.

- Improve API documentation.
- Move functors and concrete modules out of `Intf` files.
- Generally rationalise the interface ready for a public release.
- Add various container modules from `act`: `Singleton`, `T_list`, and
  `T_option`.

# v0.1.1 (2018-12-10)

Bugfix release.

- Move API documentation, in an attempt to get `dune-release` to work.

# v0.1 (2018-12-10)

Initial release.

Existing functionality migrated from `act`'s utils directory.
