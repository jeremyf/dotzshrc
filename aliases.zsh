alias file-count="find . -type f -print |wc -l"
alias bx="bundle exec"
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

# Hyrax aliases
alias hyrax-devup='fcrepo_wrapper & solr_wrapper & redis-server &'
alias hyrax-testup='fcrepo_wrapper --config config/fcrepo_wrapper_test.yml & solr_wrapper --config config/solr_wrapper_test.yml & redis-server &'
alias hyrax-devdown='sudo pkill -f solr_wrapper & sudo pkill -f fcrepo_wrapper & redis-cli shutdown'

# SSH Tunnel:
# ssh libvirt6.library.nd.edu -L 8080:localhost:8080

alias dns-flush="dscacheutil -flushcache"
alias net_traffic="lsof -r -i"

# `awsassumerole testlibnd-superAdmin` (see ~/.aws/config for profile)
awsassumerole(){
  unset AWS_VAULT
  export $(aws-vault exec $1 --assume-role-ttl 1h -- env | grep AWS)
}
