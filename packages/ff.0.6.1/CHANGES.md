### Unreleased

### 0.6.1

#### Changes

* Add ff-bench
* Trunk test names to 100 characters to avoid ENAMETOOLONG error raised by alcotest

### 0.6.0

#### Changes

* Add CHANGES.md
* Add dune-release-distrib job in CI
* Add `sub` in signature of a base field
* Implement sub for `Ff.MakeFp` and `Ff.MakeFp2`
* Add `factor_power_of_two` in `PRIME` module type and implement it in `Ff.MakeFp`.
* Add `sqrt` in `PRIME` module type and implement it in `Ff.MakeFp` using Tonelli-Shanks algorithm.
