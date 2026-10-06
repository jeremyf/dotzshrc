if [ -d /opt/homebrew ]; then eval "$(/opt/homebrew/bin/brew shellenv)"; fi

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistor

export DO_NOT_TRACK=true

source $HOME/git/dotzshrc/configs/paths.zsh

if [[ $TERM = dumb ]]; then
  unset zle_bracketed_paste
fi

if [[ "$OSTYPE" == "darwin"* ]]
then
    # Something MacOS was injecting path variables in my interactive shell.
    # These were at the front of the line.  And creating issues with Homebrew.
    export PATH="$PATH:$DARWIN_PATH"
    awkPath="$(brew --prefix)/bin/awk"
    if [[ -x $awkPath ]]; then
        export PATH="$(echo "$PATH" | $awkPath 'BEGIN { RS=":"; } { sub(sprintf("%c$", 10), ""); if (A[$0]) {} else { A[$0]=1; printf(((NR==1) ?"" : ":") $0) }}')"
    else
        echo "AWK is not located at $awkPath" # for the truly paranoid
    fi
fi

source $HOME/git/dotzshrc/configs/config.zsh
source $HOME/git/dotzshrc/configs/aliases.zsh
source $HOME/git/dotzshrc/configs/functions.zsh

if [[ -f $HOME/git/converge-cloud/aliases.zsh ]]
then
  source $HOME/git/converge-cloud/aliases.zsh
fi

# From https://github.com/akermu/emacs-libvterm?tab=readme-ov-file#shell-side-configuration
vterm_printf() {
    if [ -n "$TMUX" ] \
        && { [ "${TERM%%-*}" = "tmux" ] \
            || [ "${TERM%%-*}" = "screen" ]; }; then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

if [[ "$OSTYPE" == "darwin"* ]]; then
   if [[ -f "$(brew --prefix)/share/zsh/site-functions" ]]; then fpath=("$(brew --prefix)/share/zsh/site-functions" $fpath); fi

fi
zmodload zsh/complist
autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }

autoload -U compinit; compinit

if [ -f $ZSH/oh-my-zsh.sh ]; then
    source $ZSH/oh-my-zsh.sh
fi

if [ -f ~/git/dotzshrc/.config/starship/starship.toml ]; then
    export STARSHIP_CONFIG=~/git/dotzshrc/.config/starship/starship.toml
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# # The next line updates PATH for the Google Cloud SDK.
# if [ -d "$(brew --prefix)/share/google-cloud-sdk" ]
# then
#    # We set this so that GCloud doesn't collide with Python's venv.
#    export CLOUDSDK_PYTHON="$(brew --prefix)/bin/python3"
#    source "$(brew --prefix)/share/google-cloud-sdk/path.zsh.inc"
#    source "$(brew --prefix)/share/google-cloud-sdk/completion.zsh.inc"
# fi


if command -v brew &> /dev/null; then
  if [ -f "$(brew --prefix)/opt/asdf/libexec/asdf.sh" ]; then source "$(brew --prefix)/opt/asdf/libexec/asdf.sh"; fi
fi

# Added by Antigravity CLI installer
export PATH="/Users/jfriesen/.local/bin:$PATH"

# Automatically activate mise runtime manager
if command -v mise &> /dev/null; then
    eval "$(mise activate zsh)"
fi
