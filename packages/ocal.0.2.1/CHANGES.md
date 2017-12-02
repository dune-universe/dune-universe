### 0.2.1 (2017-11-30)

  * Update to use [notty][] 0.2.0 after breaking API change
  * Change default to display week-of-year; `-w` now stops display
  * Fix compiler and library constraints
  * Fix display of current month by default

### 0.2.0 (2017-08-28)

  * Update `pkg/pkg.ml` to *not* build docs during `topkg publish`
  * Use [notty][] for layout
  * Add option to display week-of-year
  * Set timezone to be Local rather than UTC

### 0.1.3 (2017-08-02)

  * Upgrade build to use `jbuilder` and modern `opam`, `topkg`, etc
  * Fix a few warnings

### 0.1.2 (2016-01-07)

  * Fix handling several command line flags (`--today`, `--plain`,
    `--separator`)

### 0.1.1 (2016-01-05)

  * Initial release

[notty]: https://pqwy.github.io/notty
