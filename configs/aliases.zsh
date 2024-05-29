# For bare-metal emacs
#
# There are three context's that I consider for using my text editor
# of choice:
#

# * EDITOR - uh? I forget.  I think it's less used for my cases.
# * GIT_EDITOR - this is used for editing your commit messages
export EDITOR='editor'
export GIT_EDITOR='editor'

# For those pesky Rails configs that assume a password for
# development.  Someone added that without parameterization, so I've
# added parameterization to preserve current behavior but help me out.
export SKIP_MYSQL_PASSWORD_FOR_LOCAL_DEVELOPMENT="true"

# Prompt for confirmation
alias e=$EDITOR
alias edaemon='editor-daemon'
alias e-reboot='cd ~/git/dotemacs; git stash ; edaemon ; git stash pop ; git edit'
alias rm='rm -i'
alias cp="cp -nv"
alias mv="mv -nv"
alias hugo-d="hugo serve -D --renderToMemory"
alias bx="bundle exec"
alias hb="gh browse"
alias hammerspoon-focus-emacs="hs -c \"hs.application.launchOrFocus('Emacs')\""
alias magit="$EDITOR --suppress-output --eval \"(magit)\"; hammerspoon-focus-emacs"
alias v-samvera="rg \"^ +((bulk|hy)rax([_-].*)?|rails|(.*)iiif(.*)|blacklight([_-].*)?) \(\d+\.\d+\.\d+\" Gemfile.lock | sort"

# Including these aliases as a reminder
alias postgres-start="brew services start postgresql"
alias postgres-stop="brew services stop postgresql"

# For pandoc on Apple Silicon chips
alias pand="arch -x86_64 pandoc"
alias ssh-tor="ssh takeonrules_takeonrules@ssh.phx.nearlyfreespeech.net"

# SSH Tunnel:
# ssh libvirt6.library.nd.edu -L 8080:localhost:8080

alias dns-flush="sudo dscacheutil -flushcache; sudo killall -HUP mDNSResponder"
alias net_traffic="lsof -r -i"
