arch_name="$(uname -m)"
if [ "${arch_name}" = "x86_64" ]; then
    export HB_PATH="/usr/local";
elif [ "${arch_name}" = "arm64" ]; then
    export HB_PATH="/opt/homebrew";
else
    echo "Unknown architecture: ${arch_name}"
fi


if [[ "$OSTYPE" == "darwin"* ]]; then
    source $HOME/git/dotzshrc/configs/darwin-defaults.zsh
fi

if [[ "$OSTYPE" == "linux"* ]]; then
    source $HOME/git/dotzshrc/configs/linux-defaults.zsh
fi

ZSH_THEME='powerlevel10k/powerlevel10k'

eval "$($HB_PATH/bin/brew shellenv)"

source $HOME/git/dotzshrc/configs/config.zsh
source $HOME/git/dotzshrc/configs/paths.zsh
source $HOME/git/dotzshrc/configs/aliases.zsh

eval "$(rbenv init -)"
