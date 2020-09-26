# Ezcmdliner

Ezcmdliner is a wrapper for the [cmdliner](https://erratique.ch/software/cmdliner) library that allows to define CLI commands more easily in a declarative way. 
It comes with a library which allows to
define manually the CLI and a PPX which gives some type-driven automatization.

## The library

### Installation 
[TODO]

### Usage

The library uses three concepts : the configuration, options and command. Each CLI uses a configuration to store its definition. To create a new CLI, simply
create a new configuration :
```ocaml
open Ezcmdliner

let cfg = create ()
```
It's also possible to extend another CLI by inheriting its configuration:
```ocaml
let extended_cfg = from cfg
```
Then, one can add options to the configuration by registering a cmdliner style definition into a configuration:
```ocaml
let foo = register cfg @@ value @@ opt
  ~doc:"Foo parameter"
  string
  "foo"
  ["f"; "foo"]
```
Here, the value part and its arguments follow roughly the cmdliner's conventions (except that info part is merged directly into the option specification).
This example registers the command line option "foo" as a string with a default value "foo" with a documentation string. The resuting value is an function
that gives the option value. One can add as many options as
required by the implemented CLI and when done, it's time to given the final command definition.

The command definition is the CLI part of the main program. For example, let's say the main program is:
```ocaml
let main () = Format.printf "foo = %s@." (foo ())
```
As we can see, there is no more need to parametrize the main function by the option as in cmdliner. We retrieve the parameter value by simply calling the
foo function. Then, we give the specification of this function by defining a command term (the equivalent of cmdliner one):
```ocaml
let foo_cmd = command ~cfg ~doc:"Foo printing." main
```
At last, the final CLI tool is simply run by:
```ocaml
let _ = run foo_cmd
```
Please note that run parses the command line and call the main function. Therefore, the returned value is the returned value of this function which may
return a Lwt value. Thus, the library is fully compatible with Lwt or Async or any library wrapping the function evaluation; simply call the appropriate
function on the run result (like Lwt_main.run for Lwt for example).

## The PPX

### Installation
[TODO]

### Usage

Historically, the PPX was written before the library as a modern rewriting of the ppx_deriving_cmdliner package and follow roughly the same
conventions as the latter. However, it appeared that type driven generation of CLI specification was not as clever as expected and the idea of
giving a declarative way to specify the CLI arised. For this reason, I keep this PPX in the package but I won't go into further developments until I
feel this package is really useful. That beeing said, interested people play with it and I will answer any question.

Here is the old documentation:

#### Quickstart

Assuming you want to build a binary using the command line to get its configuration (or at least a part of it), you simply define a record type which represent this configuration:

```ocaml
type config = {
  feature : bool;
  paths : string list;
  port : int;
}
```

where **feature** would stand for a flag activating, or not, a particular feature; **paths** would configure some lookup path for files and **port** for some communication port. You can now annotate this type using this specification:

```ocaml
type config = {
  feature : bool; (** Activates the feature *)
  paths : string list; (** Adds directories to include paths *) [@sep ':']
  port : int; (** The port to use *) [pos 0]
}
[@@deriving cmdliner]
[@@doc "This tool do something very interesting."]
```

Each annotation contributes to the command line parsing and man page generation. The **cmdliner** derivation will automatically generate the [Cmdliner](https://erratique.ch/software/cmdliner) terms and a function **cmdliner** that
takes the main function as parameter and acts as the binary entrypoint:

```ocaml
let main : config -> _ = fun cfg -> (* do the job *)
let () = cmdliner f
```
You can then compile with a **cmdliner** as library dependency and **ppx_cmdliner** as ppx tool and your binary will have a nice looking CLI with **--help** and other stuff automatically managed. See the documentation part for more information about annotations.

The documentation isn't published yet but I believe the quickstsart section or examples in test directory are self explaining about **ppx_cmdliner** usage. I only give here the available annotations for record labels:

Annotation | Description
--- | ---
[@aka ["f"; "foo"]] | Gives the command line option name that can be used
[@doc "doc"] | Overrides this option documentation which is by default given by ocamldoc
[@env "FOO"] | The option may take its value from the environment variable FOO
[@default 42] | Gives the option default value
[@pos 0] | The option is a positionnal argument with given index (starting at 0)
[@enum ["foo", 42; "bar", 24]] | Gives an explicit value mapping for this option
[@sep ':'] | Specifies the list separator for optional values that are lists (default is ',')

and for the record type:
Annotation | Description
--- | ---
[@@doc "My awesome tool"] | Overrides the default ocamldoc documentation
[@@version "1.2.3"] | Adds a version specification

There are others annotations but most useful ones are given above. The whole list will be documented at publication.

## Related Work

This PPX is roughly a fork of [ppx_deriving_cmdliner](https://github.com/hammerlab/ppx_deriving_cmdliner) but completely rewrote using [ppxlib](https://github.com/ocaml-ppx/ppxlib) instead of [ppx_deriving](https://github.com/ocaml-ppx/ppx_deriving).

I first tried to really fork [ppx_deriving_cmdliner](https://github.com/hammerlab/ppx_deriving_cmdliner) but it comes that modifying it for my own needs would be more painful than simply rewrote it in a more concise and maintainable way. I usually don't like to rewrote things if I can avoid it but in this case, this decision saves me a significant time. When this project will be mature enough, I expect the two projects could merge in a near future to avoid duplication noise in OCaml ecosystem.
