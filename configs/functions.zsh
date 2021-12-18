# `awsassumerole testlibnd-superAdmin` (see ~/.aws/config for profile)
# awsassumerole(){
#     unset AWS_VAULT
#     export $(aws-vault exec $1 --assume-role-ttl 1h -- env | grep AWS)
# }

# The following functions come from https://github.com/junegunn/fzf/wiki/examples

# fe [FUZZY PATTERN] - Open the selected file with the default editor
#   - Bypass fuzzy finder if there's only one match (--select-1)
#   - Exit if there's no match (--exit-0)
fe() {
  IFS=$'\n' files=($(fzf-tmux --query="$1" --multi --select-1 --exit-0 --cycle --preview 'cat {1}'))
  [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
}

# # fd - cd to selected directory
# fd() {
#   local dir
#   dir=$(find ${1:-.} -path '*/\.*' -prune \
#                   -o -type d -print 2> /dev/null | fzf +m) &&
#   cd "$dir"
# }

# fh - Open history and start searching
fh() {
  print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed -E 's/ *[0-9]*\*? *//' | sed -E 's/\\/\\\\/g')
}

# fkill - Use fzf to find and kill commands
fkill() {
    local pid
    if [ "$UID" != "0" ]; then
        pid=$(ps -f -u $UID | sed 1d | fzf --multi --exact | awk '{print $2}')
    else
        pid=$(ps -ef | sed 1d | fzf --multi --exact | awk '{print $2}')
    fi

    if [ "x$pid" != "x" ]
    then
        echo $pid | xargs kill -${1:-9}
    fi
}

# A wrapper around builtin cd, that will use fzf to do file searching.
function cd() {
    if [[ "$#" != 0 ]]; then
        builtin cd "$@";
        return
    fi
    while true; do
        local lsd=$(echo ".." && ls -p | grep '/$' | sed 's;/$;;')
        local dir="$(printf '%s\n' "${lsd[@]}" |
            fzf  --preview '
                __cd_nxt="$(echo {})";
                __cd_path="$(echo $(pwd)/${__cd_nxt} | sed "s;//;/;")";
                echo $__cd_path;
                echo;
                ls -p --color=always "${__cd_path}";
        ')"
        [[ ${#dir} != 0 ]] || return 0
        builtin cd "$dir" &> /dev/null
    done
}

if [ -d "$HOME/git/forgit" ]; then
    export PATH="$HOME/git/forgit/bin:$PATH"
    export FORGIT_NO_ALIASES=1
    source "$HOME/git/forgit/forgit.plugin.zsh"
    alias "${forgit_log:-glo}"='forgit::log'
    alias "${forgit_log:-gl}"='forgit::log'
    alias "${forgit_stash_show:-gss}"='forgit::stash::show'
fi