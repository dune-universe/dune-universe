# Packaging

The following steps should be sufficient to package `jhupllib` for distribution.

  1. Run `make clean && make test` one more time.

  2. Update the version numbers in the OPAM package file.

  3. Use `opam install .` and `opam pin remove jhupllib` to determine if the package metadata is correct and see whether it will install properly.

  4. Ensure that all changes have been committed and pushed and establish the hash `HASH` of the commit to be published.

  5. Run `opam publish http://github.com/JHU-PL-Lab/jhupllib/archive/HASH.zip` to create the PR to the OPAM package repository.

  6. Follow the CI builds on GitHub for the resulting pull request into the OPAM repository.

  7. Once the Travis CI is successful, tag the released commit: `git tag $VERSION && git push --tags`.

  8. Modify `jhupllib.opam` to contain a development version (e.g. `0.1+dev`) to distinguish the released version from future development.
