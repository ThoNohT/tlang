" Vim syntax file
" Language: tlang

" Usage instructions
" Put this file in $VIM_CONFIG/syntax.tlang.vim
" And add in your .vimrc:
" autocmd BufRead, BufNewFile *.tl set filetype=tlang


if exists("b:current_syntax")
    finish
endif

syntax keyword tlangTodo TODO

" Language keywords.
syntax keyword tlangKeyword Executable let call print

" Comments.
syntax region tlangLineComment start="//" end="$" contains=tlangTodos

" Strings.
syntax region tlangString start=/\v"/ skip=/\v\\./ end=/\v"/

" Operators.
syntax match tlangOperator "\v\+"
syntax match tlangOperator "\v\-"
syntax match tlangOperator "\v\="

" Separator
syntax match tlangSeparator "\v\-\-\-+"

" Set highlights
highlight default link tlangTodo Todo
highlight default link tlangKeyword Keyword
highlight default link tlangLineComment Comment
highlight default link tlangString String
highlight default link tlangOperator Operator
highlight default link tlangSeparator Delimiter

let b:current_syntax = "tlang"
