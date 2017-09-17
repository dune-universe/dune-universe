# coding: utf8

# Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net>

import json
import os.path
import subprocess

import sphinx.ext.autodoc


class Generator:
    def __init__(self, reference):
        self.__reference = reference
        self.__reference.pop("hidden")

    def __call__(self):
        yield from self.__ensure_single_lines(self.__item_module(self.__reference))

    def __ensure_single_lines(self, lines):
        for line in lines:
            yield " ".join(part.rstrip() for part in line.splitlines())

    def __item_module(self, module):
        yield ".. module:: {}".format(module.pop("name"))
        alias_of = module.pop("alias_of")
        if alias_of is not None:
            yield "  :alias_of: {}".format(alias_of)
        yield from self.__contents_from(module)
        yield ""
        yield from self.__indent(self.__doc(module))
        yield from self.__indent(self.__module_contents(module))

    def __contents_from(self, thing):
        contents_from = thing.pop("contents_from")
        if contents_from is not None:
            yield "  :contents_from: {}".format(contents_from)

    def __indent(self, lines):
        for line in lines:
            if line != "":
                yield "  " + line
            else:
                yield ""

    def __doc(self, thing):
        for doc in thing.pop("doc"):
            yield from self.__gen_doc(doc)
            yield ""

    def __gen_doc(self, text):
        lines = text.strip().splitlines()
        yield lines[0]
        if len(lines) > 1:
            lines = lines[1:]
            leading_spaces = min(self.__count_leading_spaces(line) for line in lines if line)
            for line in lines:
                if line != "":
                    assert line.startswith(" " * leading_spaces), (line, leading_spaces)
                    yield line[leading_spaces:]
                else:
                    yield ""

    def __count_leading_spaces(self, s):
        for i in range(len(s)):  # pragma no branch
            if s[i] != " ":
                return i
        return 0  # pragma no cover

    def __module_contents(self, thing):
        for parameter in thing.pop("functor_parameters"):
            yield ".. functor_parameter:: {}".format(parameter.pop("name"))
            yield from self.__contents_from(parameter)
            yield ""
            yield from self.__indent(self.__doc(parameter))
            yield from self.__indent(self.__module_contents(parameter))
        yield from self.__contents(thing)

    def __contents(self, thing):
        for content in thing.pop("contents"):
            if not content.pop("hidden"):
                c = content.pop("__class__")
                yield from getattr(self, "_Generator__item_{}".format(c))(content)
                assert content == {}, (c, content)  # Everything was used

    def __item_module_type(self, module_type):
        yield ".. module_type:: {}".format(module_type.pop("name"))
        yield from self.__contents_from(module_type)
        yield ""
        yield from self.__indent(self.__doc(module_type))
        yield from self.__indent(self.__module_contents(module_type))

    def __item_include(self, include):
        yield ".. incl::"
        yield from self.__contents_from(include)
        yield ""
        yield from self.__indent(self.__doc(include))
        yield from self.__indent(self.__contents(include))

    def __item_floating_documentation(self, floating_documentation):
        yield from self.__gen_doc(floating_documentation.pop("text"))
        yield ""

    def __item_value(self, value):
        yield ".. val:: {}".format(value.pop("name"))
        yield "  :type: {}".format(value.pop("type"))
        yield ""
        yield from self.__indent(self.__doc(value))

    def __item_type(self, type_):
        yield ".. type:: {}".format(type_.pop("name"))
        parameters = type_.pop("parameters")
        if parameters is not None:
            yield "  :parameters: {}".format(parameters)
        manifest = type_.pop("manifest")
        if manifest is not None:
            yield "  :manifest: {}".format(manifest)
        kind = type_.pop("kind")
        if kind is not None:
            yield "  :kind: {}".format(kind)
        for constructor in type_.pop("constructors"):
            if constructor["doc"]:
                yield ""
                yield "  :constructor {}: {}".format(
                    constructor["name"], " ".join(x.strip() for x in constructor["doc"])
                )
        for label in type_.pop("labels"):
            if label["doc"]:
                yield ""
                yield "  :label {}: {}".format(label["name"], " ".join(x.strip() for x in label["doc"]))
        yield ""
        yield from self.__indent(self.__doc(type_))

    def __item_exception(self, exception_):
        yield ".. exception:: {}".format(exception_.pop("name"))
        payload = exception_.pop("payload")
        if payload is not None:
            yield "  :payload: {}".format(payload)
        for label in exception_.pop("labels"):
            if label["doc"]:
                yield ""
                yield "  :label {}: {}".format(label["name"], " ".join(x.strip() for x in label["doc"]))
        yield ""
        yield from self.__indent(self.__doc(exception_))

    def __item_known_bug(self, bug):
        yield "Known bug in sphinxcontrib-ocaml (we'd love some help from a compiler-libs expert): {} {}".format(
            bug.pop("kind"),
            bug.pop("name"),
        )
        yield ""


class ModuleDocumenter(sphinx.ext.autodoc.Documenter):
    objtype = "ocamlmodule"

    def generate(self, more_content=None, real_modname=None, check_module=False, all_members=False):
        if self.env.config.ocaml_source_directories is None:
            # @todo Use Sphinx's warnings and errors infrastructure
            print("ERROR: please set ocaml_source_directories in conf.py")
            exit(1)

        module_name = self.directive.arguments[0]
        for d in self.env.config.ocaml_source_directories:
            interface_file_name = "{}/{}.mli".format(d, module_name)
            if os.path.isfile(interface_file_name):
                break
        else:
            # @todo Use Sphinx's warnings and errors infrastructure
            print("ERROR: {}.mli not found in any ocaml_source_directories".format(module_name))
            exit(1)

        includes = self.env.config.ocaml_include_directories + [
            subprocess.run(
                ["ocamlfind", "query", package],
                stdout=subprocess.PIPE,
                universal_newlines=True,
                check=True,
            ).stdout.strip()
            for package in self.env.config.ocaml_findlib_packages
        ]

        contents = json.loads(subprocess.run(
            [self.env.config.ocaml_autodoc_executable, interface_file_name] + includes,
            stdout=subprocess.PIPE,
            check=True,
            universal_newlines=True,
        ).stdout)

        contents["name"] = module_name

        generator = Generator(contents)
        for line in generator():
            self.add_line(line, "a")  # @todo Set file and line number (for error messages)

# @todo Documenters for elements found recursively inside modules
# (same path syntax as xref roles in the domain, using '.', ':' and '$')

# @todo Investigate what sphinx-apidoc does
# If its --separate option actually generates a file per submodule, recursively, then mimic this behavior
