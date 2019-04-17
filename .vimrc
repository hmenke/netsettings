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
set incsearch         " Start searching before pressing enter
set spelllang=de      " Set spellcheck to de
set backup            " Create backup files
set backupcopy=yes    " make a copy of the file and overwrite the original one
set tabpagemax=100    " Allow opening up to 100 tabs
set wildmode=longest,list " Bash-like autocompletion for filenames
set wildmenu
set listchars=eol:¬,tab:»·,trail:~,extends:>,precedes:<,space:␣

" Key mappings
set ttimeoutlen=0     " eliminate delays on <esc>
" In normal and visual mode indent selection or line
nmap <Tab> ==
vmap <Tab> ==
" Map <Esc> to exit terminal-mode
tnoremap <Esc> <C-\><C-n>

" use system clipboard
if has('unnamedplus')
    set clipboard+=unnamedplus
endif

" enabled mouse support
if has('mouse')
    set mouse=a
endif

" autoread files when buffer gains focus
au FocusGained,BufEnter * :checktime

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

" Statusline will be overwritten by airline (if available)
set laststatus=2
set statusline=
set statusline+=[%n]                               " buffer number
set statusline+=\ %<%f\ %m%r%w                     " full file path
set statusline+=\ %y\                              " file type
set statusline+=\ %{''.(&fenc!=''?&fenc:&enc).''}\ " encoding
set statusline+=\ %{&ff}\                          " file format
set statusline+=\ %=                               " right align
set statusline+=\ %l,%c                            " row,col
set statusline+=\ \ %P                             " top/bot

" Airline settings
let g:airline_extensions=[]
let g:airline_highlighting_cache=1

" Color theme
try
    set background=dark
    colors gruvbox
catch
endtry
