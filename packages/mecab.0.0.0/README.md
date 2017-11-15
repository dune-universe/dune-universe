# ocaml-mecab

[![Build Status](https://travis-ci.org/akabe/ocaml-mecab.svg?branch=master)](https://travis-ci.org/akabe/ocaml-mecab)

An OCaml binding of MeCab, a part-of-speech and morphological analyzer.

MeCab is a open-sourced part-of-speech and morphological analyzer independent from languages, dictionaries, and corpuses, developed by a joint project team of
[Graduate School of Informatics in Kyoto University][GSI-KU] and
[NTT Communication Science Laboratories][NTT-CSL].
MeCab is almost faster than [ChaSen][ChaSen], [Juman][Juman], and [KAKASHI][KAKASHI].
The official documentation (written in Japanese) is published at [MeCab: Yet Another Part-of-Speech and Morphological Analyzer][MeCab-ja], and English-translated version is at [MeCab English Documentation][MeCab-en].

[GSI-KU]: http://www.i.kyoto-u.ac.jp/en/
[NTT-CSL]: http://www.kecl.ntt.co.jp/rps/english/index_e.html
[ChaSen]: http://chasen-legacy.osdn.jp/
[Juman]: http://nlp.ist.i.kyoto-u.ac.jp/index.php?JUMAN
[KAKASHI]: http://kakasi.namazu.org/index.html.en
[MeCab-ja]: http://taku910.github.io/mecab/
[MeCab-en]: https://github.com/jordwest/mecab-docs-en/blob/master/README.md

# Getting started

Install the latest-released version by

```
opam install mecab
```

or the latest snapshot by `opam pin add mecab .`.

You can parse a sentense as follows:

```ocaml
# #require "mecab" ;;

# let mecab = Mecab.Tagger.create [|""|];;
val mecab : Mecab.Tagger.t = <abstr>

# Mecab.Tagger.sparse_tostr mecab "すもももももももものうち" |> print_endline;;
すもも	名詞,一般,*,*,*,*,すもも,スモモ,スモモ
も	助詞,係助詞,*,*,*,*,も,モ,モ
もも	名詞,一般,*,*,*,*,もも,モモ,モモ
も	助詞,係助詞,*,*,*,*,も,モ,モ
もも	名詞,一般,*,*,*,*,もも,モモ,モモ
の	助詞,連体化,*,*,*,*,の,ノ,ノ
うち	名詞,非自立,副詞可能,*,*,*,うち,ウチ,ウチ
EOS

- : unit = ()
```

# Documentation

- API documentation: [https://akabe.github.io/ocaml-mecab/api/](https://akabe.github.io/ocaml-mecab/api/)
