if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # Check if I have homebrew installed; If so, add this to the path
    if [ -d "/home/linuxbrew/.linuxbrew/bin" ]; then
        export PATH="/home/linuxbrew/.linuxbrew/bin:$PATH"
    fi
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
    source $HOME/git/dotzshrc/configs/darwin-defaults.zsh
fi

source $HOME/git/dotzshrc/configs/config.zsh
source $HOME/git/dotzshrc/configs/ui.zsh
source $HOME/git/dotzshrc/configs/paths.zsh
source $HOME/git/dotzshrc/configs/aliases.zsh

eval "$(rbenv init -)"
