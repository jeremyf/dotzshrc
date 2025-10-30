if [[ $TERM = dumb ]]; then
    unset zle_bracketed_paste
fi
VISUAL="editor -r"
export VISUAL

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


# if [ -f $HB_PATH/bin/brew ]; then
#  eval "$($HB_PATH/bin/brew shellenv)"
# fi

# source $HOME/git/dotzshrc/configs/paths.zsh

if command -v rbenv &> /dev/null
then
    eval "$(rbenv init -)"
else
    [[ -d "$HOME/.rbenv/bin" ]] && eval "$(~/.rbenv/bin/rbenv init - --no-rehash zsh)"
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
    export DARWIN_PATH=$PATH
fi
