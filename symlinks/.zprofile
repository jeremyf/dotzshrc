if [[ "$OSTYPE" == "darwin"* ]]; then
    source $HOME/git/dotzshrc/configs/darwin-defaults.zsh
fi

if [[ "$OSTYPE" == "linux"* ]]; then
    source $HOME/git/dotzshrc/configs/linux-defaults.zsh
fi

ZSH_THEME='powerlevel10k/powerlevel10k'

source $HOME/git/dotzshrc/configs/config.zsh
source $HOME/git/dotzshrc/configs/paths.zsh
source $HOME/git/dotzshrc/configs/aliases.zsh

eval "$(rbenv init -)"
