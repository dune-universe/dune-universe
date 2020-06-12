% OCAML-GETTEXT(5)
% Sylvain Le Gall
% 2008-04-29

# NAME

ocaml-gettext - common options to manage internationalisation in OCaml program
through ocaml-gettext library.

# SYNOPSIS

[--gettext-failsafe [{ignore} | {inform-stderr} | {raise-exception}]]<br>
[--gettext-disable] [--gettext-domain-dir {textdomain} {dir}]<br>
[--gettext-dir {dir}]<br>
[--gettext-language {language}]<br>
[--gettext-codeset {codeset}]

# DESCRIPTION

This section describes briefly the common options provided by programs using
ocaml-gettext library.

--gettext-failsafe ignore
:   Defines the behaviour of ocaml-gettext regarding any error that could be
    encountered during the processing of string translation.  ignore is the
    default behaviour. The string returned is the original string untranslated.
    This behaviour is consistent and allows to have a usable output, even if it
    is not perfect.

--gettext-failsafe inform-stderr
:   Same behaviour as ignore, except that a message is printed on stderr,

--gettext-failsafe raise-exception
:   Stops the program by raising an exception when an error is encountered.

--gettext-disable
:   Disables any translation made by ocaml-gettext. All translations return the
   original string untranslated.

--gettext-domain-dir textdomain dir
:   Defines a dir to search for a specific domain. This could be useful if MO
    files are stored in a non standard directory.

--gettext-dir dir
:   Adds a directory to search for MO files.

--gettext-language language
:   Sets the language to use in ocaml-gettext library. The language should be
    POSIX compliant. The language should follow the following convention:
    lang\[_territory]\[.charset]\[@modifier]. The lang and territory should be
    two letters ISO code. Charset should be a valid ISO character set (at least
    recognised by the underlying charset recoding routine). For example, valid
    languages are: fr_FR.ISO-8859-1@euro, de_DE.UTF-8.

--gettext-codeset codeset
:   Sets the codeset for output.

Users should be aware that these command line options, apply only for strings
after the initialisation of the library. This means that if the options
initially guessed by ocaml-gettext don't match the command line provided, there
should be some untranslated string, because these strings are translated before
parsing options. This is particularly true for the usage message itself
(--help): even if the strings are translated, they are translated before
setting the correct option.

Some options (--gettext-codeset for example) are overrided internally for
particular use. It should be required to always translate strings to UTF-8 in
graphical user interface (because GTK2 requires it).
