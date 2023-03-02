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
    IFS=$'\n' files=($(fzf-tmux --query="$1" --multi --select-1 --exit-0 --cycle --preview 'bat --color=always {1}'))
    [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
}

# # fd - cd to selected directory
# fd() {
#   local dir
#   dir=$(find ${1:-.} -path '*/\.*' -prune \
    #                   -o -type d -print 2> /dev/null | fzf +m) &&
#   cd "$dir"
# }

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
    export PATH="$PATH:$HOME/git/forgit/bin"
    source "$HOME/git/forgit/forgit.plugin.zsh"
    export FORGIT_NO_ALIASES=1
fi

# This function sets the tab color for iTerm based on the "term-color-get"
# results.
function auto_iterm_tag_color_cwd () {
    preline="\r\033[A"
    # Assumes format of `"#aabbcc"'
    hex=`term-color-get`

    first="${hex:0:1}"

    if [ "#" = "$first" ]; then
	hex="${hex:1:6}"
    fi

    hex_r="${hex:0:2}"
    hex_g="${hex:2:2}"
    hex_b="${hex:4:2}"

    rgb_r=`echo $((0x${hex_r}))`
    rgb_g=`echo $((0x${hex_g}))`
    rgb_b=`echo $((0x${hex_b}))`

    echo -e "\033]6;1;bg;red;brightness;$rgb_r\a"$preline
    echo -e "\033]6;1;bg;green;brightness;$rgb_g\a"$preline
    echo -e "\033]6;1;bg;blue;brightness;$rgb_b\a"$preline
}

auto_iterm_tag_color_cwd
autoload -U add-zsh-hook
add-zsh-hook chpwd auto_iterm_tag_color_cwd
