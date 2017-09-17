# coding: utf8

# Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net>

project = "sphinxcontrib-ocaml"
author = '<a href="http://vincent-jacques.net/">Vincent Jacques</a>'
copyright = ('2017 {} <script>var jacquev6_ribbon_github="{}"</script>'.format(author, project) +
             '<script src="https://jacquev6.github.io/ribbon.js"></script>')

version = "0.3.0"  # @todo Remove duplication of version (/sphincontrib-ocaml.opam and /doc/conf.py)
release = version

master_doc = "index"
extensions = []
nitpicky = True

# http://www.sphinx-doc.org/en/stable/ext/githubpages.html
extensions.append("sphinx.ext.githubpages")

# https://github.com/bitprophet/alabaster
html_sidebars = {
    "**": ["about.html", "navigation.html", "searchbox.html"],
}
html_theme_options = {
    "github_user": "jacquev6",
    "github_repo": project,
    "travis_button": True,
}

# https://github.com/jacquev6/sphinxcontrib-ocaml
extensions.append("sphinxcontrib.ocaml")
