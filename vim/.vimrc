" Sections:
"   -> General
"   -> Vundle
"   -> vim user interface
"   -> Colors and Fonts
"   -> Files and backups
"   -> Text, tab and indent related
"   -> Visual mode related
"   -> Moving around, tabs and buffers
"   -> Status line
"   -> Editing mappings
"   -> vimgrep searching and cope displaying
"   -> Misc
"   -> Helper functions
"   -> Tslime
"   -> Turbux
"   -> NERDTree
"   -> Alignment
"   -> Tags
"   -> Git
"   -> Commenting
"   -> Conversion
"   -> Abbreviation
"   -> Filetypes
"   -> Rails.vim commands
"   -> Local Settings

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Sets how many lines of history vim has to remember
set history=700

" Enable filetype plugins
filetype plugin on
filetype indent on

" Set to auto read when a file is changed from the outside
set autoread

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Vundle
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'

" My Bundles here:
"
" original repos on github
Bundle 'Lokaltog/vim-powerline'
Bundle 'airblade/vim-gitgutter'
Bundle 'edsono/vim-matchit'
Bundle 'godlygeek/tabular'
Bundle 'guns/vim-clojure-static'
Bundle 'int3/vim-extradite'
Bundle 'jgdavey/tslime.vim'
Bundle 'jgdavey/vim-blockle'
Bundle 'jgdavey/vim-turbux'
Bundle 'kana/vim-textobj-user'
Bundle 'kchmck/vim-coffee-script'
Bundle 'leshill/vim-json'
Bundle 'majutsushi/tagbar'
Bundle 'michaeljsmith/vim-indent-object'
Bundle 'nelstrom/vim-textobj-rubyblock'
Bundle 'pangloss/vim-javascript'
Bundle 'rking/ag.vim'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
Bundle 'slim-template/vim-slim'
Bundle 'tpope/vim-abolish'
Bundle 'tpope/vim-cucumber'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-foreplay'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-git'
Bundle 'tpope/vim-haml'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-pathogen'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-rake'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-speeddating'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-vividchalk'
Bundle 'tsaleh/vim-matchit'
Bundle 'vim-ruby/vim-ruby'
Bundle 'vim-scripts/Gundo'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM user interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Set 7 lines to the cursor - when moving vertically using j/k
set so=7

" Turn on the WiLd menu
set wildmenu
" Tab-complete files up to longest unambiguous prefix
set wildmode=longest,list,full

" Ignore compiled files
set wildignore=*.o,*~,*.pyc
if has("win16") || has("win32")
    set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
else
    set wildignore+=.git\*,.hg\*,.svn\*
endif

" Always show current position
set ruler
set number

" Show trailing whitespace
set list

" But only interesting whitespace
if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif

" Height of the command bar
set cmdheight=1


" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases
set smartcase

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

" Don't redraw while executing macros (good performance config)
set lazyredraw

" For regular expressions turn magic on
set magic

" Show matching brackets when text indicator is over them
set showmatch
" How many tenths of a second to blink when matching brackets
set mat=2

" No annoying sound on errors
set noerrorbells
set vb t_vb=

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable syntax highlighting
syntax on
filetype off
filetype on

colorscheme vividchalk
set background=dark

augroup vimrc
  autocmd!
  autocmd GuiEnter * set guifont=Monaco:h16 guioptions-=T columns=120 lines=70 number
augroup END

" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf8

" Use Unix as the standard file type
set ffs=unix,dos,mac

" Fix gutter for vim-gitgutter
highlight clear SignColumn

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Files, backups and undo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Turn backup off, since most stuff is in SVN, git et.c anyway...
set nobackup
set nowb
set noswapfile

" Source the vimrc file after saving it
augroup sourcing
  autocmd!
  autocmd bufwritepost .vimrc source $MYVIMRC
augroup END

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use spaces instead of tabs
set expandtab

" Be smart when using tabs ;)
set smarttab

" 1 tab == 2 spaces
set shiftwidth=2
set tabstop=2

set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines

""""""""""""""""""""""""""""""
" => Visual mode related
""""""""""""""""""""""""""""""

" Visual mode pressing * or # searches for the current selection
" Super useful! From an idea by Michael Naumann
vnoremap <silent> * :call VisualSelection('f', '')<CR>
vnoremap <silent> # :call VisualSelection('b', '')<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Moving around, tabs, windows and buffers
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Return to last edit position when opening files (You want this!)
augroup last_edit
  autocmd!
  autocmd BufReadPost *
       \ if line("'\"") > 0 && line("'\"") <= line("$") |
       \   exe "normal! g`\"" |
       \ endif
augroup END
" Remember info about open buffers on close
set viminfo^=%

" navigate windows
map <silent> <C-h> :wincmd h<CR>
map <silent> <C-Left> :wincmd h<CR>
map <silent> <C-k> :wincmd k<CR>
map <silent> <C-Up> :wincmd k<CR>
map <silent> <C-j> :wincmd j<CR>
map <silent> <C-Down> :wincmd j<CR>
map <silent> <C-l> :wincmd l<CR>
map <silent> <C-Right> :wincmd l<CR>


""""""""""""""""""""""""""""""
" => Status line
""""""""""""""""""""""""""""""

" Always show the status line
set laststatus=2

" Format the status line
"set statusline=\ %F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ \ \ Line:\ %l

"let g:Powerline_symbols = 'fancy'

" Allow colored status line in tmux & iTerm:
set t_Co=256

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Editing mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Delete trailing white space on save, useful for Python and CoffeeScript ;)
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc

augroup whitespace
  autocmd!
  autocmd BufWrite *.py :call DeleteTrailingWS()
  autocmd BufWrite *.coffee :call DeleteTrailingWS()
augroup END

" Trim trailing whitespace
command! -bar -range=% Trim :<line1>,<line2>s/\s\+$//e

" stop complaining about saving with :W
cmap W w

" Yank to end of line
map Y y$


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Tslime
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" tslime key combos per https://github.com/jgdavey/tslime.vim
vmap <C-c><C-c> <Plug>SendSelectionToTmux
nmap <C-c><C-c> <Plug>NormalModeSendToTmux
nmap <C-c>r <Plug>SetTmuxVars

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Turbux
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

map <C-c>t <Plug>SendTestToTmux
map <C-c>T <Plug>SendFocusedTestToTmux

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => NERDTree
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Close nerdtree after a file is selected
let NERDTreeQuitOnOpen = 1

function! IsNERDTreeOpen()
  return exists("t:NERDTreeBufName") && (bufwinnr(t:NERDTreeBufName) != -1)
endfunction

function! ToggleFindNerd()
  if IsNERDTreeOpen()
    exec ':NERDTreeToggle'
  else
    exec ':NERDTreeFind'
  endif
endfunction

" If nerd tree is closed, find current file, if open, close it
map <silent> <C-s> <ESC>:call ToggleFindNerd()<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Tags
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Prevent automatic ctag updates
let g:easytags_auto_update = 0
let g:easytags_auto_highlight = 0
let g:easytags_on_cursorhold = 0

" Scan recursively, not just current file
let g:easytags_autorecurse = 1
" Follow symbolic links
let g:easytags_resolve_links = 1

" Close tagbar after jumping to a tag
let g:tagbar_autoclose = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Git
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:extradite_width = 60
" Hide messy Ggrep output and copen automatically
function! NonintrusiveGitGrep(term)
  execute "copen"
  " Map 't' to open selected item in new tab
  execute "nnoremap <silent> <buffer> t <C-W><CR><C-W>T"
  execute "silent! Ggrep " . a:term
endfunction

command! -nargs=1 GGrep call NonintrusiveGitGrep(<q-args>)
nnoremap <silent> <C-\> :call NonintrusiveGitGrep(expand("<cword>"))<CR>

function! CommittedFiles()
  " Clear quickfix list
  let qf_list = []
  " Find files committed in HEAD
  let git_output = system("git diff-tree --no-commit-id --name-only -r HEAD\n")
  for committed_file in split(git_output, "\n")
    let qf_item = {'filename': committed_file}
    call add(qf_list, qf_item)
  endfor
  " Fill quickfix list with them
  call setqflist(qf_list, '')
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Commenting
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

map \\ <plug>NERDCommenterInvert

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Abbreviation
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Ruby debugger
iabbrev rdebug require 'ruby-debug'; Debugger.start; Debugger.settings[:autoeval] = 1; Debugger.settings[:autolist] = 1; debugger


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Filetypes
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Match .js.coffee files as CoffeeScript
au BufNewFile,BufRead *.js.coffee set filetype=coffee

" Match .md files as Markdown, not Modula-2
au BufNewFile,BufRead *.md set filetype=markdown

" .scss files are Sass
au BufNewFile,BufRead *.scss set filetype=sass

" .slim files are Slim Templates
au BufNewFile,BufRead *.slim set filetype=slim

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Rails.vim commands
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

autocmd User Rails Rnavcommand decorator app/decorators -suffix=_decorator.rb
autocmd User Rails Rnavcommand steps spec/acceptance/steps -suffix=_steps.rb
autocmd User Rails Rnavcommand accept spec/acceptance -suffix=.feature
autocmd User Rails Rnavcommand factory spec/factories -suffix=_factory.rb
autocmd User Rails Rnavcommand sass app/assets/stylesheets -suffix=.sass,.scss -default=partials/_page
autocmd User Rails Rnavcommand coffee app/assets/javascripts -suffix=.coffee


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Local Settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

if filereadable(expand("~/.vimrc.local"))
  source ~/.vimrc.local
endif
