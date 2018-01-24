" Minima.l syntax file
" Language: Minima.l 
" Maintainer: Xavier Guerin
" Last Change: 2018 Jan 18

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

" Minima.l is case sensitive.
syn case match

setl iskeyword+=?,+,*,/,=,>,<,_

syn match  MinimalComment     /#.*$/
syn match  MinimalNumber      /\v<[-+]?\d+(\.\d+)?>/
syn region MinimalString      start=/"/ skip=/\\\\\|\\"/ end=/"/
syn match  MinimalParentheses /[()\[\]]/

syn keyword MinimalSpecial  NIL T _ @ @@ @@@

syn keyword MinimalFuncs case ? ?: ?! while
syn keyword MinimalFuncs + - * / = <> < <= > >= and or not
syn keyword MinimalFuncs prog nil? fun? lst? num? str?
syn keyword MinimalFuncs car cdr chars conc cons def env eval join let list load
syn keyword MinimalFuncs flush in line out prin prinl print println read
syn keyword MinimalFuncs quit quote setq split sym sh

syn keyword MinimalDebug trace

hi default link MinimalComment Comment
hi default link MinimalCommentRegion Comment

hi default link MinimalNumber    Number
hi default link MinimalString    String
hi default link MinimalSpecial   Constant
hi default link MinimalCond      Conditional
hi default link MinimalFuncs     Function
hi default link MinimalOperator  Operator
hi default link MinimalDebug     Type

set lisp

set lispwords=
set lispwords+=prog,case,while,?,?:,?!,+,-,*,/,=,<>,<,<=,>,>=
set lispwords+=fun?,lst?,num?,str?
set lispwords+=car,cdr,chars,conc,cons,def,env,eval,join,let,list,load
set lispwords+=flush,in,line,out,prin,prinl,print,println,read
set lispwords+=quit,quote,setq,split,sym

let b:current_syntax = "minimal"
