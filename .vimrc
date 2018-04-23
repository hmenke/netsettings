set nocompatible

" Normal things
set autoindent        " enable indentation
filetype indent on    " Filetype specific indentation rules
set expandtab         " Expand tab to spaces
set softtabstop=4     " Expand to 2 spaces
set tabstop=8         " A tab is 8 spaces
set shiftwidth=4      " Autoindent uses 2 spaces
set nonumber          " disable line numbers
set modeline          " execute options local to file
syntax on             " enable synatx highlighting
set enc=utf-8         " Set default encoding to utf-8
set hlsearch          " Highlight search terms
set spelllang=de      " Set spellcheck to de
nmap <Tab> ==         " In normal and visual mode
vmap <Tab> ==         " indent selection or line
set backup            " Create backup files
set backupcopy=yes    " make a copy of the file and overwrite the original one
set wildmode=longest,list " Bash-like autocompletion for filenames
set wildmenu
set listchars=eol:¬,tab:»·,trail:~,extends:>,precedes:<,space:␣

" use system clipboard
if has('unnamedplus')
    set clipboard=unnamed,unnamedplus
endif

" Execute local vimrc's
set exrc   " enable per-directory .vimrc files
set secure " disable unsafe commands in local .vimrc files

" cd to file
set autochdir

" GVim Setting
set guioptions-=m "remove menu bar
set guioptions-=T "remove toolbar
set guioptions-=r "remove right-hand scroll bar
set guioptions-=L "remove left-hand scroll bar

" Statusline
set laststatus=2                                   " Display statusline
set statusline=
set statusline+=[%n]                               " buffer number
set statusline+=\ %<%f\ %m%r%w                     " full file path
set statusline+=\ %y\                              " file type
set statusline+=\ %{''.(&fenc!=''?&fenc:&enc).''}\ " encoding
set statusline+=\ %{&ff}\                          " file format
set statusline+=\ %=                               " right align
set statusline+=\ %l,%c                            " row,col
set statusline+=\ \ %P                             " top/bot

" Plugins
runtime bundle/vim-pathogen/autoload/pathogen.vim
execute pathogen#infect()

" Color theme
if has('gui_running')
    set background=dark
    colors gruvbox
endif

