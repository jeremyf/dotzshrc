#export PATH="/home/jfriesen/.rbenv/shims:$PATH"
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    #  eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)
    export GIT_DIFF_HIGHLIGHT_PATH="/usr/share/git/diff-highlight/diff-highlight"
    export GIT_CREDENTIAL_HELPER="/usr/lib/git-core/git-credential-libsecret"
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
    export GIT_CREDENTIAL_HELPER="osxkeychain"
    export GIT_DIFF_HIGHLIGHT_PATH="`brew --prefix git`/share/git-core/contrib/diff-highlight/diff-highlight"
    source $HOME/git/dotzshrc/configs/darwin-defaults.zsh
fi

export PATH="$PATH:/home/jfriesen/.rbenv/bin"


source $HOME/git/dotzshrc/configs/config.zsh
source $HOME/git/dotzshrc/configs/ui.zsh
source $HOME/git/dotzshrc/configs/paths.zsh
source $HOME/git/dotzshrc/configs/aliases.zsh

eval "$(rbenv init -)"
