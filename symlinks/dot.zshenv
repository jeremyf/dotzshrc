if [ -d "/Volumes/JOURNAL" ]; then
    echo "ℹ Profile Using Journal history"
    HISTFILE="/Volumes/JOURNAL/.zsh_history"
    echo $HISTFILE
fi

# Fig pre block. Keep at the top of this file.(
[[ -f "$HOME/.fig/shell/zprofile.pre.zsh" ]] && builtin source "$HOME/.fig/shell/zprofile.pre.zsh"
arch_name="$(uname -m)"
if [ "${arch_name}" = "x86_64" ]; then
    export HB_PATH="/usr/local";
elif [ "${arch_name}" = "arm64" ]; then
    export HB_PATH="/opt/homebrew";
else
    echo "Unknown architecture: ${arch_name}"
fi
if [ -f $HB_PATH/bin/brew ]; then
   eval "$($HB_PATH/bin/brew shellenv)"
fi

ZSH_THEME='powerlevel10k/powerlevel10k'

source $HOME/git/dotzshrc/configs/config.zsh
source $HOME/git/dotzshrc/configs/paths.zsh
source $HOME/git/dotzshrc/configs/aliases.zsh
source $HOME/git/dotzshrc/configs/functions.zsh

# appearance=`defaults read -g AppleInterfaceStyle 2>/dev/null`
# if [ -z "$appearance" ]
# then
#     # No value for AppleInterfaceStyle, so the OS has us in light mode,
#     # proceed accordingly.
#     sh term-light
# else
#     # AppleInterfaceStyle is set, and that means we're now in "Dark"
#     # mode.
#     sh term-dark
# fi
# 
# eval "$(rbenv init -)"
# 
# Fig post block. Keep at the bottom of this file.
[[ -f "$HOME/.fig/shell/zprofile.post.zsh" ]] && builtin source "$HOME/.fig/shell/zprofile.post.zsh"
