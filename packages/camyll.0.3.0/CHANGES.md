## 0.3.0 (July 28, 2020)

- Validate colors from TextMate themes.
- Implement other theme attributes.
- Use proper URI parsing library in server; previous code didn't handle ? and
  #.
- Use opinionated directory names instead of making them configurable.
- Determine Agda module names by parsing file instead of deriving it from the
  filepath. This makes it possible to set the project root in a different
  directory (such as through `.agda-lib`).
- Don't leave Agda-processed Markdown files in the Agda documentation directory.

## 0.2.0 (July 21, 2020)

- Switch template language from Mustache to Jingoo.
- Switch config language from YAML to TOML.
- Require frontmatter.
- Add the `serve` command for serving a site.
- Add taxonomies.
- Use TextMate themes for syntax highlighting.

## 0.1.0 (October 15, 2020)

Initial release.
