export EDITOR='emacsclient -a ""'
export GIT_EDITOR='emacsclient -a ""'

# For those pesky Rails configs that assume a password for
# development.  Someone added that without parameterization, so I've
# added parameterization to preserve current behavior but help me out.
export SKIP_MYSQL_PASSWORD_FOR_LOCAL_DEVELOPMENT="true"

# Prompt for confirmation
alias e=$EDITOR
alias rm='rm -i'
alias cp="cp -nv"
alias mv="mv -nv"
alias bx="bundle exec"
alias hb="gh browse"
# alias hammerspoon-focus-emacs="hs -c \"hs.application.launchOrFocus('Emacs')\""
# alias magit="$EDITOR --suppress-output --eval \"(magit)\"; hammerspoon-focus-emacs"
# alias v-samvera="rg \"^ +((bulk|hy)rax([_-].*)?|rails|(.*)iiif(.*)|blacklight([_-].*)?) \(\d+\.\d+\.\d+\" Gemfile.lock | sort"

if command -v batcat &> /dev/null; then
    alias bat="batcat"
fi

# Including these aliases as a reminder
# alias postgres-start="brew services start postgresql"
# alias postgres-stop="brew services stop postgresql"

# For pandoc on Apple Silicon chips

# SSH Tunnel:
# ssh libvirt6.library.nd.edu -L 8080:localhost:8080

alias dns-flush="sudo dscacheutil -flushcache; sudo killall -HUP mDNSResponder"
alias net_traffic="lsof -r -i"
alias lock-gnome="dbus-send --type=method_call --dest=org.gnome.ScreenSaver /org/gnome/ScreenSaver org.gnome.ScreenSaver.Lock"
if [ -f "$HOME/aliases.zsh" ]; then
  source $HOME/aliases.zsh
fi
