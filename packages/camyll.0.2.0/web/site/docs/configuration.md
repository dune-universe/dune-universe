+++
title = "Configuration"
layout = "main.jingoo"
+++

# Configuration

Camyll uses [TOML](https://toml.io/en/) for configuration.

```toml
# The directory that contains the page data
source_dir = # : string

# The directory that contains the rendered site
dest_dir = # : string

# The directory that contains the grammars for syntax highlighting
grammar_dir = # : string

# The directory that contains the templates
layout_dir = # : string

# The directory that contains the template partials
partial_dir = # : string

# The directory within the dest_dir that contains the rendered Literate Agda
agda_dir = # : string

# A list of globs of files to ignore
exclude = # : string list

# A list of taxonomies. The name is the name of the taxonomy (e.g. "tags",
# "categories"), and the template is the file used to generate the page for
# an individual taxonomy list page.
taxonomies = # : { name : string; template : string } list
```
