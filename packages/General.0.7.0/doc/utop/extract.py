import contextlib
import os
import re
import subprocess
import sys


class Module:
    def __init__(self, name, arguments, signature):
        self.name = name
        self.arguments = arguments
        self.signature = signature

    def text(self, indent=""):
        arguments = ""
        if indent == "" and self.arguments:
            arguments = "".join(argument.text(indent) for argument in self.arguments)
        signature = ""
        if indent == "" or not self.signature.is_empty():
            signature = ": {}".format(self.signature.text(indent))
        return "{}module {}{}{}\n".format(indent, self.name, arguments, signature)


class FunctorArgument:
    def __init__(self, name, signature):
        self.name = name
        self.signature = signature

    def text(self, indent):
        return "({}: {})".format(self.name, self.signature.text(indent))


class Alias:
    def __init__(self, name, base):
        self.name = name
        self.base = base

    def text(self, indent):
        return "{}module {} = {}\n".format(indent, self.name, self.base)


class Name:
    def __init__(self, name):
        self.name = name

    def is_empty(self):
        return False

    def text(self, indent):
        return "{}".format(self.name)


class Signature:
    def __init__(self, elements):
        self.elements = elements

    def is_empty(self):
        return not self.elements

    def text(self, indent):
        return "sig\n{}{}end".format("".join(element.text("  " + indent) for element in self.elements), indent)


class ModuleType:
    def __init__(self, name, signature):
        self.name = name
        self.signature = signature

    def text(self, indent):
        return "{}module type {} = {}\n".format(indent, self.name, self.signature.text(indent))


class Atom:
    def __init__(self, definition):
        self.definition = definition

    def text(self, indent):
        return "{}{}\n".format(indent, " ".join(self.definition))


class UTop:
    def __init__(self, process):
        process.stdout.readline()
        process.stdout.readline()
        self.__process = process
        self.__tokens = []

    def show_module(self, module_name):
        self.__process.stdin.write("#show_module {};;\n".format(module_name))
        self.__process.stdin.flush()
        self.__assert_token("#")
        return self.__parse_module()

    def __refill_tokens(self, n):
        if len(self.__tokens) <= n:
            self.__tokens = re.split(r"\s+", self.__process.stdout.readline().strip().replace("(", "( ").replace(")", " )"))
            # print("Tokens:", self.__tokens)

    def __next_token(self):
        self.__refill_tokens(0)
        tok = self.__tokens[0]
        self.__tokens = self.__tokens[1:]
        return tok

    def __peek_token(self, n=0):
        self.__refill_tokens(n)
        return self.__tokens[n]

    def __assert_token(self, expected):
        tok = self.__next_token()
        if tok != expected:
            print("Expected '{}' but got '{}'".format(expected, tok))
            exit(1)

    kinds = {"module", "val", "type", "exception", "external"}
    ends = {None, "end"}
    atom_ends = kinds | ends

    def __parse_module(self):
        self.__assert_token("module")
        name = self.__next_token()
        if self.__peek_token() == ":":
            self.__assert_token(":")
            arguments = self.__parse_functor_arguments()
            signature = self.__parse_signature()
            return Module(name, arguments, signature)
        else:
            self.__assert_token("=")
            base = self.__next_token()
            return Alias(name, base)

    def __parse_functor_arguments(self):
        arguments = []
        if self.__peek_token() == "functor":
            self.__assert_token("functor")
            while self.__peek_token() == "(":
                self.__assert_token("(")
                name = self.__next_token()
                self.__assert_token(":")
                signature = self.__parse_signature()
                argument = FunctorArgument(name, signature)
                self.__assert_token(")")
                arguments.append(argument)
            self.__assert_token("->")
        return arguments

    def __parse_signature(self):
        if self.__peek_token() == "sig":
            self.__assert_token("sig")
            if self.__peek_token() == "...":
                self.__assert_token("...")
                elements = []
            else:
                elements = self.__parse_elements()
            self.__assert_token("end")
            return Signature(elements)
        else:
            return Name(self.__next_token())

    def __parse_elements(self):
        elements = []
        while self.__peek_token() not in self.ends:
            element = self.__parse_element()
            elements.append(element)
        return elements

    def __parse_element(self):
        assert self.__peek_token() in self.kinds, "Unkwnown kind: {}".format(self.__peek_token())
        if self.__peek_token() == "module":
            if self.__peek_token(1) == "type":
                return self.__parse_module_type()
            else:
                return self.__parse_module()
        else:
            return self.__parse_atom()

    def __parse_module_type(self):
        self.__assert_token("module")
        self.__assert_token("type")
        name = self.__next_token()
        self.__assert_token("=")
        signature = self.__parse_signature()
        return ModuleType(name, signature)

    def __parse_atom(self):
        definition = [self.__next_token()]
        while self.__peek_token() not in self.atom_ends:
            definition.append(self.__next_token())
        return Atom(definition)


@contextlib.contextmanager
def utop(*options):
    process = subprocess.Popen(["utop"] + list(options), stdin=subprocess.PIPE, stdout=subprocess.PIPE, universal_newlines=True)
    yield UTop(process)
    process.communicate()


if __name__ == "__main__":
    subprocess.run(["dune", "build", "src/.General.objs/byte/General.cmi"], check=True)
    with utop("-I", "_build/default/src/.General.objs/byte") as utop:
        def show(module_name):
            # print(module_name)
            module = utop.show_module(module_name)
            with open("{}/{}.txt".format(sys.argv[1], module_name), "w") as f:
                f.write(module.text().replace("( ", "(").replace(" )", ")").replace(" :", ":"))

            if module_name not in [
                "General.Pervasives.OCamlStandard",
                "General.Abbr",
                "General.Standard",
            ]:
                for element in module.signature.elements:
                    if isinstance(element, Module) and element.signature.is_empty():
                        show("{}.{}".format(module_name, element.name))

        show("General")
