" Vim syntax file
" Language: tlang

" Usage instructions
" Put this file in $VIM_CONFIG/syntax.tlang.vim
" And add in your .vimrc:
" autocmd BufRead, BufNewFile *.tl set filetype=tlang


if exists("b:current_syntax")
    finish
endif

syntax keyword tlangTodos TODO

" Language keywords.
syntax keyword tlangKeywords Executable let call print

" Comments.
syntax region tlangLineComment start="//" end="$" contains=tlangTodos

" Strings.
syntax region tlangString start=/\v"/ skip=/\v\\./ end=/\v"/

" Set highlights
highlight default link tlangTodos Todo
highlight default link tlangKeywords Keyword
highlight default link tlangLineComment Comment
highlight default link tlangString String

let b:current_syntax = "tlang"
