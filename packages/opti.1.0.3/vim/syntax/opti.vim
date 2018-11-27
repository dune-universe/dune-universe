" Vim syntax file
" Language: Opti
" Maintainer: Magnus Jonsson
" Latest Revision: N/A

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword optiKeyword range cname extern public private proc sum recomputes propagates delta sets increments scales unit min max abs float32 float64

let b:current_syntax = "opti"

highlight link optiKeyword Keyword
