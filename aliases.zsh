alias file-count="find . -type f -print | wc -l"
alias bx="bundle exec"
# For `a /path` open 'atom /path'; for `a` open `atom .`
alias a="var=\"$1\"; if [ -n \"$var\" ]; then atom \$var ;else atom .; fi;"
alias gl='git lg'
alias gst='git st'
alias gsp='git sp'
alias gc='git co'
alias gm='git commit'
alias gb='git ba'
alias ga='git add'
alias gd='git diff'
alias gwc='git whatchanged -p --abbrev-commit --pretty=medium'
alias hb="hub browse"
alias psx="ps ax | ag $1"

# Hyrax aliases
alias hyrax-devup='fcrepo_wrapper & solr_wrapper & redis-server &'
alias hyrax-testup='fcrepo_wrapper --config config/fcrepo_wrapper_test.yml & solr_wrapper --config config/solr_wrapper_test.yml & redis-server &'
alias hyrax-devdown='pkill -f solr_wrapper & pkill -f fcrepo_wrapper & redis-cli shutdown'

# SSH Tunnel:
# ssh libvirt6.library.nd.edu -L 8080:localhost:8080

alias dns-flush="dscacheutil -flushcache"
alias net_traffic="lsof -r -i"

# `awsassumerole testlibnd-superAdmin` (see ~/.aws/config for profile)
awsassumerole(){
  unset AWS_VAULT
  export $(aws-vault exec $1 --assume-role-ttl 1h -- env | grep AWS)
}
