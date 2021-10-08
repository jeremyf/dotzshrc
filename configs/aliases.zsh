# For bare-metal emacs
#
# There are three context's that I consider for using my text editor
# of choice:
#

# * EDITOR - uh? I forget.  I think it's less used for my cases.  (I
#   use VISUAL and GIT_EDITOR more often.)
# * VISUAL - this is for visual paging of things like man-pages or
#   grep results
# * GIT_EDITOR - this is used for editing your commit messages
export EDITOR='editor'
export GIT_EDITOR='git_editor'

# For those pesky Rails configs that assume a password for
# development.  Someone added that without parameterization, so I've
# added parameterization to preserve current behavior but help me out.
export SKIP_MYSQL_PASSWORD_FOR_LOCAL_DEVELOPMENT="true"

# Prompt for confirmation
alias e=$EDITOR
alias db="dashboard"
alias dr="dired.sh"
alias edaemon='editor-daemon'
alias tor-sync="$EDITOR --eval \"(jnf/git-data-sync)\""
alias rm='rm -i'
alias file-count="find . -type f -print | wc -l"
alias bx="bundle exec"
alias gl='git lg'
alias gd='git diff'
alias gst='git st'
alias gb='git branch -vv'
alias gwc='git whatchanged -p --abbrev-commit --pretty=medium'
alias hb="hub browse"
alias psx="ps ax | ag $1"
alias rss="$EDITOR --eval \"(rss)\""

# Including these aliases as a reminder
alias postgres-start="brew services start postgresql"
alias postgres-stop="brew services stop postgresql"

# For pandoc on Apple Silicon chips
alias pand="arch -x86_64 pandoc"
alias ssh-tor="ssh takeonrules_takeonrules@ssh.phx.nearlyfreespeech.net"
alias ledger-balance="bean-report ~/git/org/projects/jeremy-friesen-consulting/ledger.beancount balances"

# Hyrax aliases
alias hyrax-devup='cp $HOME/git/dotzshrc/hyrax/solr_wrapper_dev.yml $HOME/git/samvera/hyrax/.internal_test_app/config/ ; cd $HOME/git/samvera/hyrax/.internal_test_app ; rm -rf tmp/solr-development ; fcrepo_wrapper & solr_wrapper --config  config/solr_wrapper_dev.yml & redis-server &'
alias hyrax-testup='cd $HOME/git/samvera/hyrax/.internal_test_app ; rm -rf tmp/solr-valkyrie-test/server/solr/hyrax-valkyrie-test ; rm -rf tmp/solr-test/server/solr/hydra-test ; fcrepo_wrapper --config config/fcrepo_wrapper_test.yml & solr_wrapper --config config/solr_wrapper_test.yml & solr_wrapper --config config/solr_wrapper_valkyrie_test.yml & redis-server &'
alias hyrax-old-testup='cd $HOME/git/samvera/hyrax/.internal_test_app ; rm -rf tmp/solr-valkyrie-test/server/solr/hyrax-valkyrie-test ; rm -rf tmp/solr-test/server/solr/hydra-test ; fcrepo_wrapper --config config/fcrepo_wrapper_test.yml & solr_wrapper --config config/solr_wrapper_test.yml & redis-server &'
alias hyrax-devdown='pkill -f solr_wrapper & pkill -f fcrepo_wrapper & redis-cli shutdown'
alias sqlite-browser="/Applications/DB\ Browser\ for\ SQLite.app/Contents/MacOS/DB\ Browser\ for\ SQLite"

# SSH Tunnel:
# ssh libvirt6.library.nd.edu -L 8080:localhost:8080

alias dns-flush="sudo dscacheutil -flushcache; sudo killall -HUP mDNSResponder"
alias net_traffic="lsof -r -i"

# `awsassumerole testlibnd-superAdmin` (see ~/.aws/config for profile)
awsassumerole(){
    unset AWS_VAULT
    export $(aws-vault exec $1 --assume-role-ttl 1h -- env | grep AWS)
}
