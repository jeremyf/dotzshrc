#export PATH="/home/jfriesen/.rbenv/shims:$PATH"

if [[ "$OSTYPE" == "darwin"* ]]; then
    source $HOME/git/dotzshrc/configs/darwin-defaults.zsh
fi

export PATH="$PATH:/home/jfriesen/.rbenv/bin"


source $HOME/git/dotzshrc/configs/config.zsh
source $HOME/git/dotzshrc/configs/ui.zsh
source $HOME/git/dotzshrc/configs/paths.zsh
source $HOME/git/dotzshrc/configs/aliases.zsh

eval "$(rbenv init -)"
