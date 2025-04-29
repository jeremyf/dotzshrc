# Setup fzf
# ---------
# if [[ ! "$PATH" == */opt/homebrew/opt/fzf/bin* ]]; then
#   export PATH="${PATH:+${PATH}:}/opt/homebrew/opt/fzf/bin"
# fi

# Auto-completion
# ---------------
if [ -f "/opt/homebrew/opt/fzf/shell/completion.zsh" ]; then
    source "/opt/homebrew/opt/fzf/shell/completion.zsh"
fi

# As per /usr/share/doc/fzf/README.Debian
if [ -f "/usr/share/doc/fzf/examples/completion.zsh" ]; then
    source "/usr/share/doc/fzf/examples/completion.zsh"
fi

# Key bindings
# ------------
if [ -f "/opt/homebrew/opt/fzf/shell/key-bindings.zsh" ]; then
    source "/opt/homebrew/opt/fzf/shell/key-bindings.zsh"
fi

# As per /usr/share/doc/fzf/README.Debian
if [ -f "/usr/share/doc/fzf/examples/key-bindings.zsh" ]; then
    source "/usr/share/doc/fzf/examples/key-bindings.zsh"
fi

# See https://github.com/junegunn/fzf#environment-variables
export FZF_DEFAULT_OPTS="--layout=reverse-list --marker=+ --bind 'ctrl-k:kill-line'"
export FZF_CTRL_T_COMMAND="fd --type f ."
export FZF_CTRL_T_OPTS="
  --preview 'bat -n --color=always {}'
  --height 80%
  --bind 'ctrl-o:execute(editor {})'
  --bind 'ctrl-s:execute-silent(echo {} | e-send)'
  --bind 'ctrl-y:execute-silent(echo {} | pbcopy)'
  --bind 'ctrl-/:change-preview-window(down|hidden|)'
  --header 'Press CTRL-o to open in EDITOR; CTRL-y to copy to clipboard'"
export FZF_CTRL_R_OPTS="
  --preview 'echo {}' --preview-window up:3:hidden:wrap
  --height 50%
  --bind 'ctrl-s:execute(echo -n {2..} | e-send)'
  --bind 'ctrl-y:execute-silent(echo -n {2..} | pbcopy)'
  --bind 'ctrl-/:toggle-preview'
  --color header:italic
  --header 'Press CTRL-y to copy command into clipboard'"
export FZF_ALT_C_COMMAND="fd --type d . $HOME"
export FZF_ALT_C_OPTS="
  --preview 'tree -C {}'
  --bind 'ctrl-o:execute(editor {})'
  --bind 'ctrl-s:execute-silent(echo {} | e-send)'
  --bind 'ctrl-y:execute-silent(echo {} | pbcopy)'
  --header 'Press CTRL-o to open in EDITOR; CTRL-y to copy to clipboard'"
