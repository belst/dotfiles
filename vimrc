set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'Valloric/YouCompleteMe'
Plugin 'elzr/vim-json'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'tomasr/molokai'
Plugin 'tpope/vim-surround'
Plugin 'evanmiller/nginx-vim-syntax'
Plugin 'altercation/vim-colors-solarized'
Plugin 'wting/rust.vim'

" Track the engine.
Plugin 'SirVer/ultisnips'

" Snippets are separated from the engine. Add this if you want them:
Plugin 'honza/vim-snippets'

call vundle#end()



syntax enable
set background=dark
let g:solarized_termtrans=1
colorscheme solarized
filetype plugin indent on
syntax on
set t_Co=256 ts=4 sw=4 sts=4 et nu ls=2

autocmd FileType haskell setlocal ts=2 sw=2 sts=2

let g:ycm_global_ycm_extra_conf = "~/.vim/.ycm_extra_conf.py"

" Keymappings
let g:ycm_key_list_select_completion=[]
let g:ycm_key_list_previous_completion=[]

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<F4>"
let g:UltiSnipsJumpForwardTrigger="<F5>"
let g:UltiSnipsJumpBackwardTrigger="<F3>"
