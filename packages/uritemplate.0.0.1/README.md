uritemplate-ocaml
===============

Work in progress implementation of URI templates for OCaml. (RFC6570 - http://tools.ietf.org/html/rfc6570)

**Currently compliant to level 3**

## Usage

```ocaml
# #require "uritemplate";;

# template_uri ~template:"https://example.com{/a,b}{?b}{#e,f}" ~variables:[("a", "a"); ("b", "b"); ("e", "e"); ("f", "f")];;
- : string = "https://example.com/a/b?b=b#e,f"
```

