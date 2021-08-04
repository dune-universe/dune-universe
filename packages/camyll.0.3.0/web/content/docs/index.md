+++
title = "Documentation"
layout = "docs.jingoo"
+++

# Documentation

## Installation

The easiest way to install Camyll is through Opam.

```
opam install camyll
```

## Directory Structure

A site directory should look like this:

```
.
├── config.toml
├── content/
├── grammars/
├── includes/
├── layouts/
└── theme.tmTheme
```

- `config.toml` is the configuration file.
- `content/` is a directory that contains the site content to be transformed.
- `grammars/` is a directory that contains the TextMate grammars used for
  syntax highlighting.
- `includes/` is a directory that contains partial templates that may be
  included in other templates.
- `layouts/` is a directory that contains whole-page templates.
- `theme.tmTheme` is a file that contains the TextMate theme used for syntax
  highlighting.
