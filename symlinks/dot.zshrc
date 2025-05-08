
if [[ "$OSTYPE" == "darwin"* ]]; then
   if [[ -f "$(brew --prefix)/share/zsh/site-functions" ]]; then fpath=("$(brew --prefix)/share/zsh/site-functions" $fpath); fi
   zmodload zsh/complist
fi

autoload -U compinit; compinit

if [ -f $ZSH/oh-my-zsh.sh ]; then
    source $ZSH/oh-my-zsh.sh
fi

export STARSHIP_CONFIG=~/git/dotzshrc/.config/starship/starship.toml

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

if [[ "$OSTYPE" == "darwin"* ]]; then
   # The next line updates PATH for the Google Cloud SDK.
   if [ -f "$(brew --prefix)/share/google-cloud-sdk/path.zsh.inc" ]; then source "$(brew --prefix)/share/google-cloud-sdk/path.zsh.inc"; fi

   # The next line enables shell command completion for gcloud.
   if [ -f "$(brew --prefix)/share/google-cloud-sdk/completion.zsh.inc" ]; then source "$(brew --prefix)/share/google-cloud-sdk/completion.zsh.inc"; fi

   if [ -f "$(brew --prefix)/opt/asdf/libexec/asdf.sh" ]; then source "$(brew --prefix)/opt/asdf/libexec/asdf.sh"; fi
fi
