VISUAL="editor -r"
export VISUAL
if [ -f "$HOME/.Xmodmap" ]; then
    # I'd like to launch this at login to Linux, but I have yet to find the
    # magic incantation.
#    xmodmap $HOME/.Xmodmap 2> /dev/null
fi

if [ -d "/Volumes/JOURNAL" ]; then
    echo "Profile Using Journal history"
    HISTFILE="/Volumes/JOURNAL/.zsh_history"
    echo $HISTFILE
fi

if [[ "$OSTYPE" == "linux-gnu"* ]]
then
    SSH_ENV="$HOME/.ssh/agent-environment"

    function start_agent {
        /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
        chmod 600 "${SSH_ENV}"
        . "${SSH_ENV}" > /dev/null
    }

    if [ -f "${SSH_ENV}" ]; then
        . "${SSH_ENV}" > /dev/null
        ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
            start_agent;
        }
    else
        start_agent;
    fi
fi

# TODO: This should probably move into the Mac
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

source $HOME/git/dotzshrc/configs/paths.zsh

if command -v rbenv &> /dev/null
then
  eval "$(rbenv init -)"
else
  [[ -d "$HOME/.rbenv/bin" ]] && eval "$(~/.rbenv/bin/rbenv init - --no-rehash zsh)"
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
    export DARWIN_PATH=$PATH
fi
