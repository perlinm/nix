" default colemak bindings
" source ~/.colemak.vim " pull configs from file

" general configs
set nocompatible " helpful default vim settings
set number " number lines
set relativenumber " relative line numbering
set ruler " show cursor position (row / col / %-of-file, in bottom right)
set incsearch " incremental search
set scrolloff=2 " minimum number of lines above/below cursor
set mouse=a " enable mouse
set winaltkeys=no	" allows mapping alt (meta) key shortcuts
" TODO: fix Alt+key also pressing key...

" set color scheme
try
  colorscheme desert256
catch
  colorscheme default
endtry

" space management
set expandtab " use spaces instead of tabs
set tabstop=2 " tabs are 2 spaces
set breakindent " indented line wrapping
" TODO: fix column numbers with indented text

" syntax highlighting
syntax on
filetype plugin on
filetype indent on

" searching
set smartcase " automagically figure out whether casing is important
set hlsearch " highlight search matches

" allow moving the cursor beyond the last character
set virtualedit=onemore

set timeoutlen=1000 " delay for key mapping (interpreted by vim)
set ttimeoutlen=5 " delay for keycodes (interpreted by terminal)

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" key bindings
" see :help map-modes

" remove undesired mappings
map j <nop>
map J <nop>
map k <nop>
map K <nop>
map p <nop>
map P <nop>
map w <nop>
map W <nop>
map b <nop>
map B <nop>
map x <nop>
map d <nop>
map D <nop>

" mode management
noremap t <Insert>| " insert
noremap T a| " append
noremap s v| " visual mode

" make backspace delete selected text
vnoremap <BS> DELETE

" cursor navigation
noremap n <Left>|
map u <Up>|
map e <Down>|
noremap i <Right>|
map l <Home>|
map y <End>|
noremap <silent> <expr> ; (winheight(0)/2) . "\<C-u>" " half page up
noremap <silent> <expr> o (winheight(0)/2) . "\<C-d>" " half page down

" repeat search forward/backward
noremap f n|
noremap w N|

" copy/paste/undo
set clipboard=unnamed,unnamedplus
noremap <C-c> "+y| " copy selection to clipboard
noremap <C-x> "+d| " cut selection to clipboard
noremap <C-v> "+P| " paste
noremap <C-z> :undo<CR>| " undo
noremap <C-r> :redo<CR>| " redo

" miscellaneous
noremap k zz| " center line
nnoremap Q @q| " replay the macro recorded by qq

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" key modifiers

" make up/down keys move one 'displayed' line up/down
noremap <Up> g<Up>
noremap <Down> g<Down>

" make <Home> clever about going to beginning of line vs. first nonblank character
function! SmartHome()
  let first_nonblank = match(getline('.'), '\S') + 1
  if first_nonblank == 0 " there are no whitespace characters
    " toggle between beginning and end of line
    return col('.') + 1 >= col('$') ? '0' : '^'
  endif
  if col('.') == first_nonblank " if currently at first nonblank
    return '0' " go to start of line
  endif
  " if word wrapping, go to beginning of 'displayed' line, else go to beginning of line
  return &wrap ? 'g^' : '^'
endfunction
noremap <expr> <silent> <Home> SmartHome()

" make <End> go to the end of the 'displayed' line, if word wrapping
function SmartEnd()
  return &wrap ? 'g$' : '$'
endfunction
noremap <expr> <silent> <end> SmartEnd()
