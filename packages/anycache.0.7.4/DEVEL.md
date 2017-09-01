# Local development

Clone the repository:
```bash
# If package is on opam:
$ opam source --dev-repo anycache && cd anycache
# For unreleased packages:
$ git clone https://gitlab.com/edwintorok/ocaml-anycache.git && cd anycache
```

Install dependencies:
```bash
$ opam install topkg-care
$ opam pin add -kgit anycache .#HEAD
```

Build code, run tests, view documentation:
```bash
$ topkg build
$ topkg test
$ topkg doc -r
```

See the [topkg documentation](http://erratique.ch/software/topkg/doc/Topkg.html#basics) for more details.

# For coverage reports edit the pipeline settings and add to 'Test coverage parsing':
# ^Summary.*\(\d+.\d+\%\)

# Updating online documentation

The online documentation is automatically updated by GitLab CI when
committing to master. (see pages section in [`.gitlab-ci.yml`](.gitlab-ci.yml))

# Preparing a release

Follow the `topkg-release(7)` instructions:

```bash
$ opam install topkg-care
$ topkg help release
```
