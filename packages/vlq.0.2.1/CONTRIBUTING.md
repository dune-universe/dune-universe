# Contributing to ocaml-vlq

We want to make contributing to this project as easy and transparent as
possible.

## Pull Requests

We actively welcome your pull requests.

1. Fork the repo and create your branch from `main`.
2. If you've added code that should be tested, add tests.
3. If you've changed APIs, update the documentation.
4. Ensure the test suite passes.
5. Make sure your code lints.
6. If you haven't already, complete the Contributor License Agreement ("CLA").

## Contributor License Agreement ("CLA")

In order to accept your pull request, we need you to submit a CLA. You only need
to do this once to work on any of Facebook's open source projects.

Complete your CLA here: <https://code.facebook.com/cla>

## Issues

We use GitHub issues to track public bugs. Please ensure your description is
clear and has sufficient instructions to be able to reproduce the issue.

Facebook has a [bounty program](https://www.facebook.com/whitehat/) for the safe
disclosure of security bugs. In those cases, please go through the process
outlined on that page and do not file a public issue.

## License

By contributing to ocaml-vlq, you agree that your contributions will be licensed
under the LICENSE file in the root directory of this source tree.

## Release Process

1. Install `dune-release`: `opam install dune-release`
2. Install dependencies: `opam install --deps-only --with-test .`
3. Update `CHANGES.md`, being careful to follow the existing format
4. Commit `CHANGES.md` and `git push`
5. Run some sanity checks: `dune-release lint`
6. Run the tests: `opam exec dune test`
7. Tag the distribution with a version: `dune-release tag`
8. Create the distribution archive: `dune-release distrib`
9. Upload the archive to GitHub: `dune-release publish distrib`
10. Create an opam package: `dune-release opam pkg`
11. Submit it to OCaml's opam repository: `dune-release opam submit`

For more details, see `dune-release help release`.
