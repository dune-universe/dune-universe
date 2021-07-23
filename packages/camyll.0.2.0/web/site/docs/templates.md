+++
title = "Templates"
layout = "main.jingoo"
+++

# Templates

Camyll uses the [Jingoo](
http://tategakibunko.github.io/jingoo/templates/templates.en.html) templating
engine.

## Built-ins

In addition to the functions provided by Jingoo, Camyll offers two additional
built-in functions:

`format_date(format : string, date : float) : string`

Formats a date given in Unix time. The format string is [that of the
Calendar library](
https://github.com/ocaml-community/calendar/blob/a447a88ae3c1e9873e32d2a95d3d3e7c5ed4a7da/src/printer.mli#L34),
which itself closely follows the Unix `date` command.

`slugify(str : string) : string`

"Slugifies" a string for use in a URL, consistent with Camyll's internal slugify
operation. This function is useful for getting the URL of a tag page.

## Pages

Individual pages have the following names in scope:

- `content` - The content of the page, expressed in HTML.
- `frontmatter` - The page frontmatter.
- `pages` - All the pages in the current directory.
  Each page is an object of the following format:
  - `frontmatter` - The frontmatter of the post
  - `url` - The URL of the post
