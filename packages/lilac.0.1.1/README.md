[![.github/workflows/ci.yml](https://github.com/shnewto/lilac/workflows/.github/workflows/ci.yml/badge.svg)](https://github.com/shnewto/lilac/actions)
[![codecov](https://codecov.io/gh/shnewto/lilac/branch/main/graph/badge.svg?token=0OUYOWVMOL)](https://codecov.io/gh/shnewto/lilac)

# lilac

lilac is a simple library for getting the value of any field in a YAML file as a string.

## Summary

Take this example yaml:

```yaml
lilac-params:
  source:
    url: "https://ttaw.dev"
    user: "lilac+source@ttaw.dev"
  dest:
    url: "https://walkandtalk.dev"
    user: "lilac+dest@walkandtalk.dev"
```

To get the value `"lilac+source@ttaw.dev"`, you'd specify the path `lilac-params.source.user`.

Here's an example of how you might do that in code with the yaml used in this project's `test/res` directory:

```ocaml
let yaml = yaml_from_fpath "test/res/config.yaml" in
  yaml_value_str ~path:"lilac-params.source.user" yaml
  |> Option.value ~default:"Oops! It wasn't there."
```

### lilacbin
The `lilacbin` app in this repo's `bin/` is really just used for debugging, so it's dependencies are tagged as test only requirements in the package manifest. It does have some niceties like a `--help` flag, but if there's something you're interested in it doing beyond debugging, let me know! Raising an issue is my preferred channel for that kinda thing.

## Dependencies

I'll start with the basics for macOS because putting this project together required I learn them too. I hope they'll
serve as at least some direction for other operating systems but if you notice something important I've missed,
please create an issue :heart:

First you'll need `opam` and `ocaml`.

For macOS:

- `brew install opam`
- `brew install ocaml`

For anything else, the opam installation docs live [here](https://opam.ocaml.org/doc/Install.html),
and the ocaml intallation docs live [here](https://ocaml.org/docs/install.html).

Once OCaml and opam are installed, run `opam init` from the terminal to get the environment sorted. I chose to
let it modify my terminal's rc file (`.zshrc`) so I wouldn't have to think about it again, but that's up to you :)

## Installing

If you just want to install lilac as a library and use it in your application, you can run `opam install lilac`.
Otherwise, see the Developing section below.

## Developing
## Dev dependencies

For local development, I'll recommend not installing with opam beforehand. Once you've cloned the repo and have navigated into its directory, you'll want to run these commands, in this order:

- `opam pin .`
- `opam install . --deps-only --with-test`

## Building

`make`

## Running the tests

`make test`

## Checking code coverage

`make coverage`

## Running lilac bin for stdout debugging / validation

`make run args='-i <path to yaml file>'`

For example `make run args='-i test/res/config.yaml'`
