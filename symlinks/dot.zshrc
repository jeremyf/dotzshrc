if [[ "$OSTYPE" == "darwin"* ]]
then
    # Something MacOS was injecting path variables in my interactive shell.
    # These were at the front of the line.  And creating issues with Homebrew.
    export PATH="$DARWIN_PATH:$PATH"
    if [[ -x /opt/homebrew/bin/awk ]]; then
        export PATH="$(echo "$PATH" | /opt/homebrew/bin/awk 'BEGIN { RS=":"; } { sub(sprintf("%c$", 10), ""); if (A[$0]) {} else { A[$0]=1; printf(((NR==1) ?"" : ":") $0) }}')"
    else
        echo "AWK is not located at /opt/homebrew/bin/awk" # for the truly paranoid
    fi

    if [[ -f "$(brew --prefix)/share/zsh/site-functions" ]]; then fpath=("$(brew --prefix)/share/zsh/site-functions" $fpath); fi
   zmodload zsh/complist
    # Hello what is likely Darwin
    appearance=`defaults read -g AppleInterfaceStyle 2>/dev/null`
    if [ -z "$appearance" ]
    then
        # No value for AppleInterfaceStyle, so the OS has us in light mode,
        # proceed accordingly.
        sh $HOME/bin/term-light
    else
        # AppleInterfaceStyle is set, and that means we're now in "Dark"
        # mode.
        sh $HOME/bin/term-light
    fi
fi

source $HOME/git/dotzshrc/configs/config.zsh
source $HOME/git/dotzshrc/configs/aliases.zsh
source $HOME/git/dotzshrc/configs/functions.zsh

autoload -U compinit; compinit

ZSH_THEME='powerlevel10k/powerlevel10k'
if [ -f $ZSH/oh-my-zsh.sh ]; then
    source $ZSH/oh-my-zsh.sh
fi

if [ -f ~/git/dotzshrc/.config/starship/starship.toml ]; then
   export STARSHIP_CONFIG=~/git/dotzshrc/.config/starship/starship.toml
fi

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

if [[ "$OSTYPE" == "darwin"* ]]; then
   # The next line updates PATH for the Google Cloud SDK.
   if [ -f "$(brew --prefix)/share/google-cloud-sdk/path.zsh.inc" ]; then source "$(brew --prefix)/share/google-cloud-sdk/path.zsh.inc"; fi

   # The next line enables shell command completion for gcloud.
   if [ -f "$(brew --prefix)/share/google-cloud-sdk/completion.zsh.inc" ]; then source "$(brew --prefix)/share/google-cloud-sdk/completion.zsh.inc"; fi

   if [ -f "$(brew --prefix)/opt/asdf/libexec/asdf.sh" ]; then source "$(brew --prefix)/opt/asdf/libexec/asdf.sh"; fi
fi
