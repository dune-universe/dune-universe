+++
title = "Syntax Highlighting"
layout = "main.jingoo"
+++

# Syntax Highlighting

Camyll supports syntax highlighting using TextMate themes and grammars. If a
TextMate theme file called `theme.tmTheme` exists in the project directory,
Camyll will use it to highlight Markdown fenced code blocks. Put the grammars
for the languages you want to highlight in the directory specified in the
[configuration](configuration.html), and Camyll will find them. When searching
for a language, Camyll will first search case-insensitively by the `name`
attribute of the grammar files, then search case-sensitively by the `fileTypes`
attribute of the grammar files.
