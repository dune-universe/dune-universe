# Travesty

_Travesty_ is a library for defining containers with monadic traversals,
inspired by Haskell's
[Traversable](http://hackage.haskell.org/package/base/docs/Data-Traversable.html)
typeclass.  It sits on top of Jane Street's
[Core](https://opensource.janestreet.com/core/) library ecosystem.

Travesty also contains several other bits of Haskell-style monad functionality:

- state monads (`Travesty.State`);
- state transformers (`Travesty.State_transform`);
- miscellaneous extensions on monads (`Travesty.Monad_exts`) and containers (`Travesty.Containers_exts`);
- extensions and implementations of Travesty signatures for `Base`
  (`Travesty_base_exts`) and `Core_kernel` (`Travesty_core_kernel_exts`)
  containers;
- extra function combinators (`Travesty_base_exts.Fn` and `Travesty_core_kernel_exts.Fn`).

Note: the `Travesty_base_exts` and `Travesty_core_kernel_exts` modules form
separate subpackages (`travesty.base_exts` and `travesty.core_kernel_exts`
respectively).

Travesty is licenced under the MIT licence, and is a spin-off from the
[act](https://github.com/MattWindsor91/act) project.

[![Build Status](https://travis-ci.com/MattWindsor91/travesty.svg?branch=master)](https://travis-ci.com/MattWindsor91/travesty)

## Usage

See the [API documentation](https://MattWindsor91.github.io/travesty).

Travesty tries not to shadow existing modules except in the various
`exts` subpackages.

## Contributions

Any and all contributions (pull requests, issues, etc.) are welcome.
