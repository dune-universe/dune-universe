+++
title = "Taxonomies"
layout = "main.jingoo"
+++

# Taxonomies

Camyll supports arbitrary, user-defined page classifications. A *taxonomy* is
a classification, such as "categories" or "tags." A *taxonomy term* is a name
for a set of pages that is associated with a particular taxonomy, such as
a tag called "OCaml" or a category called "programming."

Taxonomies must be declared in the [configuration file](configuration.html).
Camyll will generate a page for each taxonomy term at the address
`/<taxonomy-name>/<term-name>` using the template specified in the configuration
file.

A [page's frontmatter](pages.html) lists the terms that the page belongs to.
Each term belongs to the namespace of one taxonomy.
