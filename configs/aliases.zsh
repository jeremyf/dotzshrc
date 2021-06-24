# For bare-metal emacs
export EDITOR='editor'
export GIT_EDITOR='git_editor'

# Prompt for confirmation
alias e=$EDITOR
alias dr="dired.sh"
alias edaemon='editor-daemon'
alias org="$EDITOR ~/git/org/agenda.org"
alias tor-sync="$EDITOR --eval \"(tor-sync)\""
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
