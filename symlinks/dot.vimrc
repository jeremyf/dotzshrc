" Add numbers in the gutter
set number
syntax on

nnoremap <F5> :!rake <Enter>

" Disable markdown folding by default
let g:vim_markdown_folding_disabled = 1

let g:vim_markdown_fenced_languages = ['ruby=ruby', 'bash=sh']

" YAML front matter highlighting
let g:vim_markdown_frontmatter = 1

" Use the terminal colors
if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
endif

" Add column indicator at 80 and 120.
au FileType ruby setlocal colorcolumn=80,120

" Good commit messages. 50 columns for title, 72 columns for body
au FileType gitcommit setlocal colorcolumn=50,72

" Align airline them with vim background
let g:airline_theme='base16_google'

" Replace :Ack search program with `ag`
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif
