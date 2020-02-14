set nocompatible

" Normal things
set autoindent        " enable indentation
filetype plugin indent on " Filetype specific indentation rules
set expandtab         " Expand tab to spaces
set softtabstop=4     " Expand to 2 spaces
set tabstop=8         " A tab is 8 spaces
set shiftwidth=4      " Autoindent uses 2 spaces
set nonumber          " disable line numbers
set nomodeline        " don't execute options local to file
syntax enable         " enable synatx highlighting
set enc=utf-8         " Set default encoding to utf-8
set hlsearch          " Highlight search terms
set incsearch         " Start searching before pressing enter
set spelllang=de      " Set spellcheck to de
set hidden            " Unload abandoned buffers
set backup            " Create backup files
set backupcopy=yes    " make a copy of the file and overwrite the original one
set undofile          " Save the undo history to a file
set tabpagemax=100    " Allow opening up to 100 tabs
set iskeyword+=@-@    " include '@' in word search
set wildmode=longest,list " Bash-like autocompletion for filenames
set wildmenu
set listchars=eol:¬,tab:»·,trail:~,extends:>,precedes:<,space:␣

" Key mappings
set ttimeoutlen=0     " eliminate delays on <esc>

" In normal and visual mode indent selection or line
nmap <Tab> ==
vmap <Tab> ==
noremap <C-q> q:i

try
    " Map <Esc> to exit terminal-mode (NeoVim only)
    tnoremap <Esc> <C-\><C-n>
catch
endtry
" Selection to X clipboard in visual mode (only works in NeoVim and
" requires 'set mouse=a')
vmap <LeftRelease> "*ygv

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

" Don't store temporary files for pass
au BufNewFile,BufRead /dev/shm/*pass.* setlocal noswapfile nobackup noundofile

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

" Plugins
call plug#begin('~/.config/nvim/plugged')

Plug 'jnurmine/Zenburn'
Plug 'morhetz/gruvbox'
Plug 'othree/eregex.vim'
Plug 'chrisbra/unicode.vim'
Plug 'vim-airline/vim-airline'
Plug 'rhysd/vim-clang-format'
Plug 'tpope/vim-fugitive'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-sensible'
Plug 'Yggdroot/indentLine'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

call plug#end()

" Airline settings
let g:airline_highlighting_cache=1
let g:airline_powerline_fonts=0
let g:airline_theme='gruvbox'
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#tabline#buffer_min_count=2
let g:airline#extensions#tabline#left_sep=' '
let g:airline#extensions#tabline#left_alt_sep=''
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_symbols.branch = '⎇'

" Disable eregex by default
let g:eregex_default_enable = 0

" Always treat tex files as sty files
let g:tex_stylish = 1
let g:tex_conceal = ''

" indentLine character
" let g:indentLine_char_list = ['|', '¦', '┆', '┊', '│',' ⎸', '▏']
let g:indentLine_char = '▏'

" snippets
let g:UltiSnipsNoPythonWarning = 1
let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpForwardTrigger = '<tab>'
let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'

" Color theme
try
    set termguicolors
    set background=dark
    colors gruvbox
catch
endtry

" fzf
if filereadable("/usr/share/doc/fzf/examples/fzf.vim")
    source /usr/share/doc/fzf/examples/fzf.vim
endif
