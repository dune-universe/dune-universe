if exists("b:current_syntax")
  finish
endif


let b:current_syntax = "alba"



syntax keyword albaKeyword all
syntax keyword albaKeyword create
syntax keyword albaKeyword inspect
syntax keyword albaKeyword where

syntax match albaComment "--.*$"
syntax region albaComment start="{-" end="-}"

highlight link albaKeyword Keyword
highlight link albaComment Comment
