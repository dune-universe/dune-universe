ElasticSearch Guided (code) Generator
=====================================

[![Build Status](https://travis-ci.org/ahrefs/esgg.svg?branch=master)](https://travis-ci.org/ahrefs/esgg)

Development
-----------

Install dependencies with `opam install --deps-only .`

Buld with `make`

Variables
---------

Syntax for variables in template json files is as follows:
- `$var` for regular required variable
- `$var?` for optional variable (minimal surrounding scope is conditionally expunged)
- full form `$(var:hint)` where `hint` can be either `list` or `list?` currently

`list` hint is useful to chose between named (default) and unnamed dynamic filters

Tests
-----

`make test` runs regression tests in [test/](test/) verifying
that input and output atd generated from query stays unchanged.
Once there is an expected change in generated query - it should be committed.
Tests are easy to add and fast to run.

TODO tests to verify that:

	* code generated for query application of input variables does actually compile and produce correct query when run
	* atd description of output (generated from query) can indeed unserialize ES output from that actual query

Conditions
----------

Copyright (c) 2018 Ahrefs <github@ahrefs.com>

This project is distributed under the terms of GPL Version 2. See LICENSE file for full license text.

NB the output of esgg, i.e. the generated code, is all yours of course :)

----
2019-03-19
