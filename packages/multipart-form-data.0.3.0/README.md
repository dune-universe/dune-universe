multipart/form-data (RFC2388) parser for OCaml
==============================================

 [![Build Status](https://travis-ci.org/cryptosense/multipart-form-data.svg?branch=master)](https://travis-ci.org/cryptosense/multipart-form-data) [![docs](https://img.shields.io/badge/doc-online-blue.svg)](https://cryptosense.github.io/multipart-form-data/doc/)

This is a parser for structured form data based on `Lwt_stream` in order to use
it with [cohttp](https://github.com/mirage/ocaml-cohttp/). You can use it to
send POST parameters.

There are two APIs:

- a high-level one: `parse_stream` and `get_parts`. It works for strings, but
  has some problems with files.
- a low-level one: `parse`. It works for well for both strings and files.
