# tldr-ocaml
[![Build Status](https://travis-ci.org/RosalesJ/tldr-ocaml.svg?branch=master)](https://travis-ci.org/RosalesJ/tldr-ocaml)

A client for [tldr](https://github.com/tldr-pages/tldr) written in ```ocaml```.

![tldr screenshot](screenshot.png)

## Installation
Install from opam with:
```
opam install tldr
```
Or from source with dune:
```
git clone https://github.com/RosalesJ/tldr-ocaml.git
cd tldr-ocaml
dune build @install
dune install
```

## Configuration
You can configure tldr by setting environment variables.

### Colors
You can customize the colors and style of the display using the following variables
```
TLDR_COLOR_TITLE
TLDR_COLOR_DESCRIPTION
TLDR_COLOR_EXAMPLE
TLDR_COLOR_COMMAND
TLDR_COLOR_ARGUMENT
```
And these options
```
Colors: black | white | red | green | yellow | blue | magenta | cyan 
Styles: bold | underlined | blink
```

To denote a foreground color use ```<color>``` and a background color use ```on_<color>```.
List the colors and styles in a string delimited by semicolons. Here is a sample style configuration. Don't use it though, it's really obnoxious.
```bash
export TLDR_COLOR_TITLE="red;on_white;blink"
export TLDR_COLOR_DESCRIPTION="yellow"
export TLDR_COLOR_EXAMPLE="cyan;on_magenta;underlined"
export TLDR_COLOR_ARGUMENT="green"
export TLDR_COLOR_COMMAND="blue"
```

### Cache
If caching is enabled then tldr will attempt to load from the cache before fetching from the internet.
The cache is located in one of the following locations in decreasing order of precedence.
* ```$XDG_CACHE_HOME/tldr```
* ```$HOME/.cache/tldr```
* ```~/.cache/tldr```

Here's a sample configuration

```bash
export TLDR_CACHE_ENABLED=0      # Caching is disabled (default is enabled)
export TLDR_MAX_CACHE_AGE=168    # Cached pages last 168 hours (default is 24)
```
