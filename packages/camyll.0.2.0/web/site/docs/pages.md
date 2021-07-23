+++
title = "Pages"
layout = "main.jingoo"
+++

# Pages

*Pages* are files that have a one-to-one mapping with the webpages of the
generated site. At the top of a page, there must be a pair of `+++`s. In
between the `+++` is the page's *frontmatter*, which is written in
[TOML](https://toml.io/en/). For example:

```markdown
+++
title = "Pages"
layout = "main.jingoo"
+++

# Pages

*Pages* are files that have a one-to-one mapping with the webpages of the
generated site. At the top of a page, there must be a pair of `+++`s. In
between the `+++` is the page's *frontmatter*, which is written in
[TOML](https://toml.io/en/). For example:
```

## Frontmatter

The frontmatter attributes that Camyll uses are:

```toml
# The Jingoo layout to use
layout = # : string
# A map from taxonomies to a list of taxonomy terms.
taxonomies = # : { string -> string list }
```

Other attributes can be added to the frontmatter, and they will be accessible
from the Jingoo layout. An enhanced frontmatter could look like this:

For example:

```toml
title = "Introduction to OCaml"
layout = "post.jingoo"
date = 2021-01-05

[taxonomies]
categories = ["Programming"]
tags = ["OCaml", "Functional programming"]
```

This frontmatter describes a page that will be transformed according to the
layout "post.jingoo", belongs to category "Programming", and has tags "OCaml"
and "Functional programming". In addition, the page has a title of "Introduction
to OCaml" and an associated date of January 5th, 2021.

## File Extensions

The file extension of a page determines how Camyll handles it. The supported
file extensions are:

- `.html`: HTML pages are left as-is.
- `.md`: Markdown files are translated to HTML.
- `.lagda.md`: Literate Agda Markdown files are transformed into regular
  Markdown files by the Agda compiler, then handled like other Markdown files.
