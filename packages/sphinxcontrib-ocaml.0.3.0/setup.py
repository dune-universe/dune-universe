#!/usr/bin/env python3
# coding: utf8

# Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net>

import setuptools


version = "0.3.0"

setuptools.setup(
    name="sphinxcontrib-ocaml",
    version=version,
    description="Sphinx extension to document OCaml libraries",
    long_description=open("README.rst").read(),
    author="Vincent Jacques",
    author_email="vincent@vincent-jacques.net",
    url="http://jacquev6.github.io/sphinxcontrib-ocaml/",
    packages=setuptools.find_packages(),
    namespace_packages=["sphinxcontrib"],
    license="MIT",
    classifiers=[
        "Development Status :: 2 - Pre-Alpha",
        "Intended Audience :: Developers",
        "License :: OSI Approved",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
        "Programming Language :: Python",
        "Programming Language :: OCaml",
        "Topic :: Documentation",
        "Topic :: Documentation :: Sphinx",
    ],
    use_2to3=True,
    # install_requires=[],
    # tests_require=[],
    # test_suite="",
    command_options={
        "build_sphinx": {
            "version": ("setup.py", version),
            "release": ("setup.py", version),
            "source_dir": ("setup.py", "doc"),
        },
    },
)
