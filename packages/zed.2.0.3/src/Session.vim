let SessionLoad = 1
if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
imap <Nul> <C-Space>
inoremap <expr> <Up> pumvisible() ? "\" : "\<Up>"
inoremap <expr> <S-Tab> pumvisible() ? "\" : "\<S-Tab>"
inoremap <expr> <Down> pumvisible() ? "\" : "\<Down>"
inoremap <silent> <C-Tab> =UltiSnips#ListSnippets()
snoremap <silent>  c
xnoremap <silent>  :call multiple_cursors#new("v", 0)
nnoremap <silent>  :call multiple_cursors#new("n", 1)
xnoremap <silent>  :call UltiSnips#SaveLastVisualSelection()gvs
snoremap <silent>  :call UltiSnips#ExpandSnippet()
map  <Plug>(ctrlp)
snoremap  "_c
omap <silent> % <Plug>(MatchitOperationForward)
xmap <silent> % <Plug>(MatchitVisualForward)
nmap <silent> % <Plug>(MatchitNormalForward)
nnoremap ,d :YcmShowDetailedDiagnostic
nmap <silent> ,ig <Plug>IndentGuidesToggle
vnoremap <silent> ,,w :call EasyMotion#WB(1, 0)
onoremap <silent> ,,w :call EasyMotion#WB(0, 0)
nnoremap <silent> ,,w :call EasyMotion#WB(0, 0)
vnoremap <silent> ,,t :call EasyMotion#T(1, 0)
onoremap <silent> ,,t :call EasyMotion#T(0, 0)
nnoremap <silent> ,,t :call EasyMotion#T(0, 0)
vnoremap <silent> ,,n :call EasyMotion#Search(1, 0)
onoremap <silent> ,,n :call EasyMotion#Search(0, 0)
nnoremap <silent> ,,n :call EasyMotion#Search(0, 0)
vnoremap <silent> ,,k :call EasyMotion#JK(1, 1)
onoremap <silent> ,,k :call EasyMotion#JK(0, 1)
nnoremap <silent> ,,k :call EasyMotion#JK(0, 1)
vnoremap <silent> ,,j :call EasyMotion#JK(1, 0)
onoremap <silent> ,,j :call EasyMotion#JK(0, 0)
nnoremap <silent> ,,j :call EasyMotion#JK(0, 0)
vnoremap <silent> ,,gE :call EasyMotion#EW(1, 1)
onoremap <silent> ,,gE :call EasyMotion#EW(0, 1)
nnoremap <silent> ,,gE :call EasyMotion#EW(0, 1)
vnoremap <silent> ,,f :call EasyMotion#F(1, 0)
onoremap <silent> ,,f :call EasyMotion#F(0, 0)
nnoremap <silent> ,,f :call EasyMotion#F(0, 0)
vnoremap <silent> ,,e :call EasyMotion#E(1, 0)
onoremap <silent> ,,e :call EasyMotion#E(0, 0)
nnoremap <silent> ,,e :call EasyMotion#E(0, 0)
vnoremap <silent> ,,b :call EasyMotion#WB(1, 1)
onoremap <silent> ,,b :call EasyMotion#WB(0, 1)
nnoremap <silent> ,,b :call EasyMotion#WB(0, 1)
vnoremap <silent> ,,W :call EasyMotion#WBW(1, 0)
onoremap <silent> ,,W :call EasyMotion#WBW(0, 0)
nnoremap <silent> ,,W :call EasyMotion#WBW(0, 0)
vnoremap <silent> ,,T :call EasyMotion#T(1, 1)
onoremap <silent> ,,T :call EasyMotion#T(0, 1)
nnoremap <silent> ,,T :call EasyMotion#T(0, 1)
vnoremap <silent> ,,N :call EasyMotion#Search(1, 1)
onoremap <silent> ,,N :call EasyMotion#Search(0, 1)
nnoremap <silent> ,,N :call EasyMotion#Search(0, 1)
vnoremap <silent> ,,ge :call EasyMotion#E(1, 1)
onoremap <silent> ,,ge :call EasyMotion#E(0, 1)
nnoremap <silent> ,,ge :call EasyMotion#E(0, 1)
vnoremap <silent> ,,F :call EasyMotion#F(1, 1)
onoremap <silent> ,,F :call EasyMotion#F(0, 1)
nnoremap <silent> ,,F :call EasyMotion#F(0, 1)
vnoremap <silent> ,,E :call EasyMotion#EW(1, 0)
onoremap <silent> ,,E :call EasyMotion#EW(0, 0)
nnoremap <silent> ,,E :call EasyMotion#EW(0, 0)
vnoremap <silent> ,,B :call EasyMotion#WBW(1, 1)
onoremap <silent> ,,B :call EasyMotion#WBW(0, 1)
nnoremap <silent> ,,B :call EasyMotion#WBW(0, 1)
nnoremap ,t :YcmCompleter GetType
map Q gq
xmap S <Plug>VSurround
omap <silent> [% <Plug>(MatchitOperationMultiBackward)
xmap <silent> [% <Plug>(MatchitVisualMultiBackward)
nmap <silent> [% <Plug>(MatchitNormalMultiBackward)
omap <silent> ]% <Plug>(MatchitOperationMultiForward)
xmap <silent> ]% <Plug>(MatchitVisualMultiForward)
nmap <silent> ]% <Plug>(MatchitNormalMultiForward)
xmap a% <Plug>(MatchitVisualTextObject)
nmap cS <Plug>CSurround
nmap cs <Plug>Csurround
nmap ds <Plug>Dsurround
vmap gx <Plug>NetrwBrowseXVis
nmap gx <Plug>NetrwBrowseX
xmap gS <Plug>VgSurround
xnoremap <silent> gî :call multiple_cursors#select_all("v", 0)
nnoremap <silent> gî :call multiple_cursors#select_all("n", 0)
xnoremap <silent> g :call multiple_cursors#new("v", 0)
nnoremap <silent> g :call multiple_cursors#new("n", 0)
omap <silent> g% <Plug>(MatchitOperationBackward)
xmap <silent> g% <Plug>(MatchitVisualBackward)
nmap <silent> g% <Plug>(MatchitNormalBackward)
nmap ySS <Plug>YSsurround
nmap ySs <Plug>YSsurround
nmap yss <Plug>Yssurround
nmap yS <Plug>YSurround
nmap ys <Plug>Ysurround
nnoremap <silent> <Plug>(ocpindex-print) :call ocpindex#print()
nnoremap <silent> <Plug>(ocpindex-jump-back) :call ocpindex#jump_back()
nnoremap <silent> <Plug>(ocpindex-jump) :call ocpindex#jump()
nnoremap <silent> <Plug>(ocpindex-echo-type) :call ocpindex#echo_type()
xnoremap <silent> <Plug>OCamlPrintType :call Ocaml_print_type("visual")`<
nnoremap <silent> <Plug>OCamlPrintType :call Ocaml_print_type("normal")
nnoremap <Plug>OCamlSwitchNewWin :call OCaml_switch(1)
nnoremap <Plug>OCamlSwitchEdit :call OCaml_switch(0)
vnoremap <silent> <Plug>NetrwBrowseXVis :call netrw#BrowseXVis()
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#BrowseX(expand((exists("g:netrw_gx")? g:netrw_gx : '<cfile>')),netrw#CheckIfRemote())
nnoremap <silent> <Plug>(ctrlp) :CtrlP
nnoremap <silent> <Plug>SurroundRepeat .
snoremap <silent> <Del> c
snoremap <silent> <BS> c
snoremap <silent> <C-Tab> :call UltiSnips#ListSnippets()
vmap <silent> <Plug>(MatchitVisualTextObject) <Plug>(MatchitVisualMultiBackward)o<Plug>(MatchitVisualMultiForward)
onoremap <silent> <Plug>(MatchitOperationMultiForward) :call matchit#MultiMatch("W",  "o")
onoremap <silent> <Plug>(MatchitOperationMultiBackward) :call matchit#MultiMatch("bW", "o")
vnoremap <silent> <Plug>(MatchitVisualMultiForward) :call matchit#MultiMatch("W",  "n")m'gv``
vnoremap <silent> <Plug>(MatchitVisualMultiBackward) :call matchit#MultiMatch("bW", "n")m'gv``
nnoremap <silent> <Plug>(MatchitNormalMultiForward) :call matchit#MultiMatch("W",  "n")
nnoremap <silent> <Plug>(MatchitNormalMultiBackward) :call matchit#MultiMatch("bW", "n")
onoremap <silent> <Plug>(MatchitOperationBackward) :call matchit#Match_wrapper('',0,'o')
onoremap <silent> <Plug>(MatchitOperationForward) :call matchit#Match_wrapper('',1,'o')
vnoremap <silent> <Plug>(MatchitVisualBackward) :call matchit#Match_wrapper('',0,'v')m'gv``
vnoremap <silent> <Plug>(MatchitVisualForward) :call matchit#Match_wrapper('',1,'v')m'gv``
nnoremap <silent> <Plug>(MatchitNormalBackward) :call matchit#Match_wrapper('',0,'n')
nnoremap <silent> <Plug>(MatchitNormalForward) :call matchit#Match_wrapper('',1,'n')
imap S <Plug>ISurround
imap s <Plug>Isurround
inoremap <expr> 	 pumvisible() ? "\" : "\	"
inoremap <silent>  =UltiSnips#ExpandSnippet()
imap  <Plug>Isurround
inoremap  u
xnoremap <silent> î :call multiple_cursors#select_all("v", 0)
nnoremap <silent> î :call multiple_cursors#select_all("n", 1)
let &cpo=s:cpo_save
unlet s:cpo_save
set background=dark
set backspace=indent,eol,start
set completefunc=youcompleteme#CompleteFunc
set completeopt=preview,menuone
set display=truncate
set expandtab
set fileencodings=utf8,gbk
set helplang=cn
set history=200
set hlsearch
set incsearch
set nojoinspaces
set langnoremap
set nolangremap
set listchars=eol:¶
set mouse=a
set nrformats=bin,hex
set printoptions=paper:letter
set ruler
set runtimepath=~/.opam/4.06.1/share/ocp-indent/vim,~/.vim,~/.vim/bundle/Vundle.vim,~/.vim/bundle/YouCompleteMe,~/.vim/bundle/ultisnips,~/.vim/bundle/vim-snippets,~/.vim/bundle/syntastic,~/.vim/bundle/EasyMotion,~/.vim/bundle/Tabular,~/.vim/bundle/The-NERD-tree,~/.vim/bundle/Indent-Guides,~/.vim/bundle/vim-indent-object,~/.vim/bundle/LargeFile,~/.vim/bundle/vim-multiple-cursors,~/.vim/bundle/paredit.vim,~/.vim/bundle/emmet-vim,~/.vim/bundle/vim-surround,~/.vim/bundle/vim-ruby,~/.vim/bundle/rails.vim,~/.vim/bundle/SWIG-syntax,~/.vim/bundle/vim-systemd-syntax,~/.vim/bundle/ctrlp.vim,~/.vim/bundle/gruvbox,~/.vim/bundle/molokai,~/.vim/bundle/vim-opencl,/var/lib/vim/addons,/usr/share/vim/vimfiles,/usr/share/vim/vim81,/usr/share/vim/vim81/pack/dist/opt/matchit,/usr/share/vim/vimfiles/after,/var/lib/vim/addons/after,~/.vim/after,~/.vim/bundle/Vundle.vim,~/.vim/bundle/Vundle.vim/after,~/.vim/bundle/YouCompleteMe/after,~/.vim/bundle/ultisnips/after,~/.vim/bundle/vim-snippets/after,~/.vim/bundle/syntastic/after,~/.vim/bundle/EasyMotion/after,~/.vim/bundle/Tabular/after,~/.vim/bundle/The-NERD-tree/after,~/.vim/bundle/Indent-Guides/after,~/.vim/bundle/vim-indent-object/after,~/.vim/bundle/LargeFile/after,~/.vim/bundle/vim-multiple-cursors/after,~/.vim/bundle/paredit.vim/after,~/.vim/bundle/emmet-vim/after,~/.vim/bundle/vim-surround/after,~/.vim/bundle/vim-ruby/after,~/.vim/bundle/rails.vim/after,~/.vim/bundle/SWIG-syntax/after,~/.vim/bundle/vim-systemd-syntax/after,~/.vim/bundle/ctrlp.vim/after,~/.vim/bundle/gruvbox/after,~/.vim/bundle/molokai/after,~/.vim/bundle/vim-opencl/after,~/.opam/4.06.1/share/ocp-index/vim,~/.opam/4.06.1/share/merlin/vim
set scrolloff=3
set shiftround
set shiftwidth=2
set shortmess=filnxtToOc
set showcmd
set smarttab
set softtabstop=2
set statusline=%#warningmsg#%{SyntasticStatuslineFlag()}%*
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set tabpagemax=100
set ttimeout
set ttimeoutlen=100
set wildmenu
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/pro/ml/newterm/zed/src
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
argglobal
%argdel
$argadd zed_input.mli
set stal=2
tabnew
tabnew
tabnew
tabnew
tabrewind
edit zed_input.ml
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
argglobal
vmap <buffer> <silent> 	 :MerlinPhrase
nmap <buffer> <silent> [[ :MerlinPhrasePrev
nmap <buffer> <silent> \t :MerlinTypeOf
vmap <buffer> <silent> \t :MerlinTypeOfSel
map <buffer> <silent> \p :MerlinShrinkEnclosing
map <buffer> <silent> \n :MerlinGrowEnclosing
omap <buffer> <silent> \t :MerlinTypeOf
let s:cpo_save=&cpo
set cpo&vim
nmap <buffer> \S <Plug>OCamlSwitchNewWin
nmap <buffer> \s <Plug>OCamlSwitchEdit
xmap <buffer> \C <Plug>BUncomOff
nmap <buffer> \C <Plug>LUncomOff
xmap <buffer> \c <Plug>BUncomOn
nmap <buffer> \c <Plug>LUncomOn
nmap <buffer> <silent> ]] :MerlinPhraseNext
nmap <buffer> <silent> gd :MerlinLocate
nmap <buffer> <silent> <Plug>(MerlinRenameAppend) :call merlin_find#IncrementalRename()//ea
nmap <buffer> <silent> <Plug>(MerlinRename) :call merlin_find#IncrementalRename()//c//e
nmap <buffer> <silent> <Plug>(MerlinSearchOccurrencesBackward) :call merlin_find#OccurrencesSearch('?'):let v:searchforward=0
nmap <buffer> <silent> <Plug>(MerlinSearchOccurrencesForward) :call merlin_find#OccurrencesSearch('/'):let v:searchforward=1
xnoremap <buffer> <Plug>BUncomOff :'<,'>`<dd`>dd`<
xnoremap <buffer> <Plug>BUncomOn :'<,'>`<O0i(*`>o0i*)`<
nnoremap <buffer> <Plug>LUncomOff :s/^(\* \(.*\) \*)/\1/:noh
nnoremap <buffer> <Plug>LUncomOn gI(* <End> *)
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=
setlocal noarabic
setlocal noautoindent
setlocal backupcopy=
setlocal balloonexpr=
setlocal nobinary
setlocal nobreakindent
setlocal breakindentopt=
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),0],:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=sr:(*,mb:*,ex:*)
setlocal commentstring=/*%s*/
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=youcompleteme#CompleteFunc
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=%EFile\ \"%f\"\\,\ line\ %l\\,\ characters\ %c-%*\\d:,%EFile\ \"%f\"\\,\ line\ %l\\,\ character\ %c:%m,%+EReference\ to\ unbound\ regexp\ name\ %m,%Eocamlyacc:\ e\ -\ line\ %l\ of\ \"%f\"\\,\ %m,%Wocamlyacc:\ w\ -\ %m,%-Zmake%.%#,%C%m,%D%*\\a[%*\\d]:\ Entering\ directory\ `%f',%X%*\\a[%*\\d]:\ Leaving\ directory\ `%f',%D%*\\a:\ Entering\ directory\ `%f',%X%*\\a:\ Leaving\ directory\ `%f',%DMaking\ %*\\a\ in\ %f
setlocal expandtab
if &filetype != 'ocaml'
setlocal filetype=ocaml
endif
setlocal fixendofline
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=9
setlocal foldmarker={{{,}}}
set foldmethod=indent
setlocal foldmethod=indent
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=tcqor
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal formatprg=
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=-1
setlocal include=
setlocal includeexpr=
setlocal indentexpr=GetOcpIndent(v:lnum)
setlocal indentkeys=0{,0},0),0],:,0#,!^F,o,O,e,0=and,0=class,0=constraint,0=done,0=else,0=end,0=exception,0=external,0=if,0=in,0=include,0=inherit,0=initializer,0=let,0=method,0=open,0=then,0=type,0=val,0=with,0;;,0>],0|],0>},0|,0},0],0)
setlocal noinfercase
setlocal iskeyword=@,48-57,_,192-255
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal lispwords=
setlocal nolist
setlocal makeencoding=
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=bin,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=merlin#Complete
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal scrolloff=-1
setlocal shiftwidth=2
setlocal noshortname
setlocal sidescrolloff=-1
setlocal signcolumn=auto
setlocal nosmartindent
setlocal softtabstop=2
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'ocaml'
setlocal syntax=ocaml
endif
setlocal tabstop=8
setlocal tagcase=
setlocal tags=
setlocal termmode=
setlocal termwinkey=
setlocal termwinscroll=10000
setlocal termwinsize=
setlocal textwidth=0
setlocal thesaurus=
setlocal noundofile
setlocal undolevels=-123456
setlocal varsofttabstop=
setlocal vartabstop=
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
31
normal! zo
58
normal! zo
59
normal! zo
64
normal! zo
64
normal! zo
65
normal! zo
115
normal! zo
116
normal! zo
122
normal! zo
122
normal! zo
124
normal! zo
127
normal! zo
let s:l = 90 - ((13 * winheight(0) + 24) / 49)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
90
normal! 032|
tabnext
edit zed_input.mli
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
argglobal
vmap <buffer> <silent> 	 :MerlinPhrase
nmap <buffer> <silent> [[ :MerlinPhrasePrev
nmap <buffer> <silent> \t :MerlinTypeOf
vmap <buffer> <silent> \t :MerlinTypeOfSel
map <buffer> <silent> \p :MerlinShrinkEnclosing
map <buffer> <silent> \n :MerlinGrowEnclosing
omap <buffer> <silent> \t :MerlinTypeOf
let s:cpo_save=&cpo
set cpo&vim
nmap <buffer> \S <Plug>OCamlSwitchNewWin
nmap <buffer> \s <Plug>OCamlSwitchEdit
xmap <buffer> \C <Plug>BUncomOff
nmap <buffer> \C <Plug>LUncomOff
xmap <buffer> \c <Plug>BUncomOn
nmap <buffer> \c <Plug>LUncomOn
nmap <buffer> <silent> ]] :MerlinPhraseNext
nmap <buffer> <silent> gd :MerlinLocate
nmap <buffer> <silent> <Plug>(MerlinRenameAppend) :call merlin_find#IncrementalRename()//ea
nmap <buffer> <silent> <Plug>(MerlinRename) :call merlin_find#IncrementalRename()//c//e
nmap <buffer> <silent> <Plug>(MerlinSearchOccurrencesBackward) :call merlin_find#OccurrencesSearch('?'):let v:searchforward=0
nmap <buffer> <silent> <Plug>(MerlinSearchOccurrencesForward) :call merlin_find#OccurrencesSearch('/'):let v:searchforward=1
xnoremap <buffer> <Plug>BUncomOff :'<,'>`<dd`>dd`<
xnoremap <buffer> <Plug>BUncomOn :'<,'>`<O0i(*`>o0i*)`<
nnoremap <buffer> <Plug>LUncomOff :s/^(\* \(.*\) \*)/\1/:noh
nnoremap <buffer> <Plug>LUncomOn gI(* <End> *)
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=
setlocal noarabic
setlocal noautoindent
setlocal backupcopy=
setlocal balloonexpr=
setlocal nobinary
setlocal nobreakindent
setlocal breakindentopt=
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),0],:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=sr:(*,mb:*,ex:*)
setlocal commentstring=/*%s*/
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=youcompleteme#CompleteFunc
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=%EFile\ \"%f\"\\,\ line\ %l\\,\ characters\ %c-%*\\d:,%EFile\ \"%f\"\\,\ line\ %l\\,\ character\ %c:%m,%+EReference\ to\ unbound\ regexp\ name\ %m,%Eocamlyacc:\ e\ -\ line\ %l\ of\ \"%f\"\\,\ %m,%Wocamlyacc:\ w\ -\ %m,%-Zmake%.%#,%C%m,%D%*\\a[%*\\d]:\ Entering\ directory\ `%f',%X%*\\a[%*\\d]:\ Leaving\ directory\ `%f',%D%*\\a:\ Entering\ directory\ `%f',%X%*\\a:\ Leaving\ directory\ `%f',%DMaking\ %*\\a\ in\ %f
setlocal expandtab
if &filetype != 'ocaml'
setlocal filetype=ocaml
endif
setlocal fixendofline
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=6
setlocal foldmarker={{{,}}}
set foldmethod=indent
setlocal foldmethod=indent
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=tcqor
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal formatprg=
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=-1
setlocal include=
setlocal includeexpr=
setlocal indentexpr=GetOcpIndent(v:lnum)
setlocal indentkeys=0{,0},0),0],:,0#,!^F,o,O,e,0=and,0=class,0=constraint,0=done,0=else,0=end,0=exception,0=external,0=if,0=in,0=include,0=inherit,0=initializer,0=let,0=method,0=open,0=then,0=type,0=val,0=with,0;;,0>],0|],0>},0|,0},0],0)
setlocal noinfercase
setlocal iskeyword=@,48-57,_,192-255
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal lispwords=
setlocal nolist
setlocal makeencoding=
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=bin,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=merlin#Complete
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal scrolloff=-1
setlocal shiftwidth=2
setlocal noshortname
setlocal sidescrolloff=-1
setlocal signcolumn=auto
setlocal nosmartindent
setlocal softtabstop=2
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'ocaml'
setlocal syntax=ocaml
endif
setlocal tabstop=8
setlocal tagcase=
setlocal tags=
setlocal termmode=
setlocal termwinkey=
setlocal termwinscroll=10000
setlocal termwinsize=
setlocal textwidth=0
setlocal thesaurus=
setlocal noundofile
setlocal undolevels=-123456
setlocal varsofttabstop=
setlocal vartabstop=
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
15
normal! zo
57
normal! zo
58
normal! zo
58
normal! zo
59
normal! zo
61
normal! zo
63
normal! zo
let s:l = 67 - ((45 * winheight(0) + 24) / 49)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
67
normal! 0
tabnext
edit zed_edit.ml
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
argglobal
vmap <buffer> <silent> 	 :MerlinPhrase
nmap <buffer> <silent> [[ :MerlinPhrasePrev
nmap <buffer> <silent> \t :MerlinTypeOf
vmap <buffer> <silent> \t :MerlinTypeOfSel
map <buffer> <silent> \p :MerlinShrinkEnclosing
map <buffer> <silent> \n :MerlinGrowEnclosing
omap <buffer> <silent> \t :MerlinTypeOf
let s:cpo_save=&cpo
set cpo&vim
nmap <buffer> \S <Plug>OCamlSwitchNewWin
nmap <buffer> \s <Plug>OCamlSwitchEdit
xmap <buffer> \C <Plug>BUncomOff
nmap <buffer> \C <Plug>LUncomOff
xmap <buffer> \c <Plug>BUncomOn
nmap <buffer> \c <Plug>LUncomOn
nmap <buffer> <silent> ]] :MerlinPhraseNext
nmap <buffer> <silent> gd :MerlinLocate
nmap <buffer> <silent> <Plug>(MerlinRenameAppend) :call merlin_find#IncrementalRename()//ea
nmap <buffer> <silent> <Plug>(MerlinRename) :call merlin_find#IncrementalRename()//c//e
nmap <buffer> <silent> <Plug>(MerlinSearchOccurrencesBackward) :call merlin_find#OccurrencesSearch('?'):let v:searchforward=0
nmap <buffer> <silent> <Plug>(MerlinSearchOccurrencesForward) :call merlin_find#OccurrencesSearch('/'):let v:searchforward=1
xnoremap <buffer> <Plug>BUncomOff :'<,'>`<dd`>dd`<
xnoremap <buffer> <Plug>BUncomOn :'<,'>`<O0i(*`>o0i*)`<
nnoremap <buffer> <Plug>LUncomOff :s/^(\* \(.*\) \*)/\1/:noh
nnoremap <buffer> <Plug>LUncomOn gI(* <End> *)
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=
setlocal noarabic
setlocal noautoindent
setlocal backupcopy=
setlocal balloonexpr=
setlocal nobinary
setlocal nobreakindent
setlocal breakindentopt=
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),0],:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=sr:(*,mb:*,ex:*)
setlocal commentstring=/*%s*/
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=youcompleteme#CompleteFunc
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=%EFile\ \"%f\"\\,\ line\ %l\\,\ characters\ %c-%*\\d:,%EFile\ \"%f\"\\,\ line\ %l\\,\ character\ %c:%m,%+EReference\ to\ unbound\ regexp\ name\ %m,%Eocamlyacc:\ e\ -\ line\ %l\ of\ \"%f\"\\,\ %m,%Wocamlyacc:\ w\ -\ %m,%-Zmake%.%#,%C%m,%D%*\\a[%*\\d]:\ Entering\ directory\ `%f',%X%*\\a[%*\\d]:\ Leaving\ directory\ `%f',%D%*\\a:\ Entering\ directory\ `%f',%X%*\\a:\ Leaving\ directory\ `%f',%DMaking\ %*\\a\ in\ %f
setlocal expandtab
if &filetype != 'ocaml'
setlocal filetype=ocaml
endif
setlocal fixendofline
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
set foldmethod=indent
setlocal foldmethod=indent
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=tcqor
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal formatprg=
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=-1
setlocal include=
setlocal includeexpr=
setlocal indentexpr=GetOcpIndent(v:lnum)
setlocal indentkeys=0{,0},0),0],:,0#,!^F,o,O,e,0=and,0=class,0=constraint,0=done,0=else,0=end,0=exception,0=external,0=if,0=in,0=include,0=inherit,0=initializer,0=let,0=method,0=open,0=then,0=type,0=val,0=with,0;;,0>],0|],0>},0|,0},0],0)
setlocal noinfercase
setlocal iskeyword=@,48-57,_,192-255
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal lispwords=
setlocal nolist
setlocal makeencoding=
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=bin,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=merlin#Complete
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal scrolloff=-1
setlocal shiftwidth=2
setlocal noshortname
setlocal sidescrolloff=-1
setlocal signcolumn=auto
setlocal nosmartindent
setlocal softtabstop=2
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'ocaml'
setlocal syntax=ocaml
endif
setlocal tabstop=8
setlocal tagcase=
setlocal tags=
setlocal termmode=
setlocal termwinkey=
setlocal termwinscroll=10000
setlocal termwinsize=
setlocal textwidth=0
setlocal thesaurus=
setlocal noundofile
setlocal undolevels=-123456
setlocal varsofttabstop=
setlocal vartabstop=
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
678
normal! zo
679
normal! zo
680
normal! zo
681
normal! zo
683
normal! zo
687
normal! zo
694
normal! zo
702
normal! zo
736
normal! zo
737
normal! zo
737
normal! zo
773
normal! zo
let s:l = 724 - ((13 * winheight(0) + 24) / 49)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
724
normal! 019|
tabnext
edit zed_macro.mli
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
argglobal
vmap <buffer> <silent> 	 :MerlinPhrase
nmap <buffer> <silent> [[ :MerlinPhrasePrev
nmap <buffer> <silent> \t :MerlinTypeOf
vmap <buffer> <silent> \t :MerlinTypeOfSel
map <buffer> <silent> \p :MerlinShrinkEnclosing
map <buffer> <silent> \n :MerlinGrowEnclosing
omap <buffer> <silent> \t :MerlinTypeOf
let s:cpo_save=&cpo
set cpo&vim
nmap <buffer> \S <Plug>OCamlSwitchNewWin
nmap <buffer> \s <Plug>OCamlSwitchEdit
xmap <buffer> \C <Plug>BUncomOff
nmap <buffer> \C <Plug>LUncomOff
xmap <buffer> \c <Plug>BUncomOn
nmap <buffer> \c <Plug>LUncomOn
nmap <buffer> <silent> ]] :MerlinPhraseNext
nmap <buffer> <silent> gd :MerlinLocate
nmap <buffer> <silent> <Plug>(MerlinRenameAppend) :call merlin_find#IncrementalRename()//ea
nmap <buffer> <silent> <Plug>(MerlinRename) :call merlin_find#IncrementalRename()//c//e
nmap <buffer> <silent> <Plug>(MerlinSearchOccurrencesBackward) :call merlin_find#OccurrencesSearch('?'):let v:searchforward=0
nmap <buffer> <silent> <Plug>(MerlinSearchOccurrencesForward) :call merlin_find#OccurrencesSearch('/'):let v:searchforward=1
xnoremap <buffer> <Plug>BUncomOff :'<,'>`<dd`>dd`<
xnoremap <buffer> <Plug>BUncomOn :'<,'>`<O0i(*`>o0i*)`<
nnoremap <buffer> <Plug>LUncomOff :s/^(\* \(.*\) \*)/\1/:noh
nnoremap <buffer> <Plug>LUncomOn gI(* <End> *)
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=
setlocal noarabic
setlocal noautoindent
setlocal backupcopy=
setlocal balloonexpr=
setlocal nobinary
setlocal nobreakindent
setlocal breakindentopt=
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),0],:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=sr:(*,mb:*,ex:*)
setlocal commentstring=/*%s*/
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=youcompleteme#CompleteFunc
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=%EFile\ \"%f\"\\,\ line\ %l\\,\ characters\ %c-%*\\d:,%EFile\ \"%f\"\\,\ line\ %l\\,\ character\ %c:%m,%+EReference\ to\ unbound\ regexp\ name\ %m,%Eocamlyacc:\ e\ -\ line\ %l\ of\ \"%f\"\\,\ %m,%Wocamlyacc:\ w\ -\ %m,%-Zmake%.%#,%C%m,%D%*\\a[%*\\d]:\ Entering\ directory\ `%f',%X%*\\a[%*\\d]:\ Leaving\ directory\ `%f',%D%*\\a:\ Entering\ directory\ `%f',%X%*\\a:\ Leaving\ directory\ `%f',%DMaking\ %*\\a\ in\ %f
setlocal expandtab
if &filetype != 'ocaml'
setlocal filetype=ocaml
endif
setlocal fixendofline
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
set foldmethod=indent
setlocal foldmethod=indent
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=tcqor
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal formatprg=
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=-1
setlocal include=
setlocal includeexpr=
setlocal indentexpr=GetOcpIndent(v:lnum)
setlocal indentkeys=0{,0},0),0],:,0#,!^F,o,O,e,0=and,0=class,0=constraint,0=done,0=else,0=end,0=exception,0=external,0=if,0=in,0=include,0=inherit,0=initializer,0=let,0=method,0=open,0=then,0=type,0=val,0=with,0;;,0>],0|],0>},0|,0},0],0)
setlocal noinfercase
setlocal iskeyword=@,48-57,_,192-255
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal lispwords=
setlocal nolist
setlocal makeencoding=
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=bin,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=merlin#Complete
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal scrolloff=-1
setlocal shiftwidth=2
setlocal noshortname
setlocal sidescrolloff=-1
setlocal signcolumn=auto
setlocal nosmartindent
setlocal softtabstop=2
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'ocaml'
setlocal syntax=ocaml
endif
setlocal tabstop=8
setlocal tagcase=
setlocal tags=
setlocal termmode=
setlocal termwinkey=
setlocal termwinscroll=10000
setlocal termwinsize=
setlocal textwidth=0
setlocal thesaurus=
setlocal noundofile
setlocal undolevels=-123456
setlocal varsofttabstop=
setlocal vartabstop=
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
16
normal! zo
let s:l = 55 - ((49 * winheight(0) + 24) / 49)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
55
normal! 0
tabnext
edit zed_macro.ml
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
argglobal
vmap <buffer> <silent> 	 :MerlinPhrase
nmap <buffer> <silent> [[ :MerlinPhrasePrev
nmap <buffer> <silent> \t :MerlinTypeOf
vmap <buffer> <silent> \t :MerlinTypeOfSel
map <buffer> <silent> \p :MerlinShrinkEnclosing
map <buffer> <silent> \n :MerlinGrowEnclosing
omap <buffer> <silent> \t :MerlinTypeOf
let s:cpo_save=&cpo
set cpo&vim
nmap <buffer> \S <Plug>OCamlSwitchNewWin
nmap <buffer> \s <Plug>OCamlSwitchEdit
xmap <buffer> \C <Plug>BUncomOff
nmap <buffer> \C <Plug>LUncomOff
xmap <buffer> \c <Plug>BUncomOn
nmap <buffer> \c <Plug>LUncomOn
nmap <buffer> <silent> ]] :MerlinPhraseNext
nmap <buffer> <silent> gd :MerlinLocate
nmap <buffer> <silent> <Plug>(MerlinRenameAppend) :call merlin_find#IncrementalRename()//ea
nmap <buffer> <silent> <Plug>(MerlinRename) :call merlin_find#IncrementalRename()//c//e
nmap <buffer> <silent> <Plug>(MerlinSearchOccurrencesBackward) :call merlin_find#OccurrencesSearch('?'):let v:searchforward=0
nmap <buffer> <silent> <Plug>(MerlinSearchOccurrencesForward) :call merlin_find#OccurrencesSearch('/'):let v:searchforward=1
xnoremap <buffer> <Plug>BUncomOff :'<,'>`<dd`>dd`<
xnoremap <buffer> <Plug>BUncomOn :'<,'>`<O0i(*`>o0i*)`<
nnoremap <buffer> <Plug>LUncomOff :s/^(\* \(.*\) \*)/\1/:noh
nnoremap <buffer> <Plug>LUncomOn gI(* <End> *)
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=
setlocal noarabic
setlocal noautoindent
setlocal backupcopy=
setlocal balloonexpr=
setlocal nobinary
setlocal nobreakindent
setlocal breakindentopt=
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),0],:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=sr:(*,mb:*,ex:*)
setlocal commentstring=/*%s*/
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=youcompleteme#CompleteFunc
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=%EFile\ \"%f\"\\,\ line\ %l\\,\ characters\ %c-%*\\d:,%EFile\ \"%f\"\\,\ line\ %l\\,\ character\ %c:%m,%+EReference\ to\ unbound\ regexp\ name\ %m,%Eocamlyacc:\ e\ -\ line\ %l\ of\ \"%f\"\\,\ %m,%Wocamlyacc:\ w\ -\ %m,%-Zmake%.%#,%C%m,%D%*\\a[%*\\d]:\ Entering\ directory\ `%f',%X%*\\a[%*\\d]:\ Leaving\ directory\ `%f',%D%*\\a:\ Entering\ directory\ `%f',%X%*\\a:\ Leaving\ directory\ `%f',%DMaking\ %*\\a\ in\ %f
setlocal expandtab
if &filetype != 'ocaml'
setlocal filetype=ocaml
endif
setlocal fixendofline
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
set foldmethod=indent
setlocal foldmethod=indent
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=tcqor
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal formatprg=
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=-1
setlocal include=
setlocal includeexpr=
setlocal indentexpr=GetOcpIndent(v:lnum)
setlocal indentkeys=0{,0},0),0],:,0#,!^F,o,O,e,0=and,0=class,0=constraint,0=done,0=else,0=end,0=exception,0=external,0=if,0=in,0=include,0=inherit,0=initializer,0=let,0=method,0=open,0=then,0=type,0=val,0=with,0;;,0>],0|],0>},0|,0},0],0)
setlocal noinfercase
setlocal iskeyword=@,48-57,_,192-255
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal lispwords=
setlocal nolist
setlocal makeencoding=
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=bin,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=merlin#Complete
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal scrolloff=-1
setlocal shiftwidth=2
setlocal noshortname
setlocal sidescrolloff=-1
setlocal signcolumn=auto
setlocal nosmartindent
setlocal softtabstop=2
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'ocaml'
setlocal syntax=ocaml
endif
setlocal tabstop=8
setlocal tagcase=
setlocal tags=
setlocal termmode=
setlocal termwinkey=
setlocal termwinscroll=10000
setlocal termwinsize=
setlocal textwidth=0
setlocal thesaurus=
setlocal noundofile
setlocal undolevels=-123456
setlocal varsofttabstop=
setlocal vartabstop=
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
13
normal! zo
24
normal! zo
28
normal! zo
73
normal! zo
74
normal! zo
let s:l = 72 - ((57 * winheight(0) + 24) / 49)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
72
normal! 05|
tabnext 3
set stal=1
badd +103 zed_input.ml
badd +0 zed_input.mli
badd +740 zed_edit.ml
badd +1 zed_macro.mli
badd +72 zed_macro.ml
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToOc
set winminheight=1 winminwidth=1
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
