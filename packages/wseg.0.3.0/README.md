# wseg

A word identification system

## Usage

In the test directory, there are two plain text files that serve as dictionary files, one for characters and one for words. And there is a test.ml program that illustrates how to build dictionary and index with wseg and how to apply several rules to identify words from a sentence. Just invoke `make runtest` to play with the demo.

`char.dic` contains 12640 Chinese characters and `word.dic` contains 157202 words. So you can expand the dict or demo for common usage.
