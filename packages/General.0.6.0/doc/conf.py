# coding: utf8

# Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net>

master_doc = "index"
project = "General"
author = '<a href="http://vincent-jacques.net/">Vincent Jacques</a>'
copyright = ('2017 {} <script>var jacquev6_ribbon_github="{}"</script>'.format(author, project) +
             '<script src="https://jacquev6.github.io/ribbon.js"></script>')

version = "0.4.0"  # @todo Remove duplication of version (/General.opam and /doc/conf.py)
release = version

nitpicky = True
extensions = []

# https://github.com/jacquev6/sphinx-ocaml
extensions.append("sphinxcontrib.ocaml")
primary_domain = "ocaml"
ocaml_source_directories = ["_build/default/src"]

# https://github.com/bitprophet/alabaster
# html_theme_path
extensions.append("alabaster")
html_theme = "alabaster"
html_sidebars = {
    "**": ["about.html", "navigation.html", "searchbox.html"],
}
html_theme_options = {
    "github_user": "jacquev6",
    "github_repo": project,
    "travis_button": True,
}
# html_logo = "logo.png"
