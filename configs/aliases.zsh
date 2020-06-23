# Prompt for confirmation
alias edaemon='emacs --daemon -fn "MesloLGS NF 13"'
alias e='emacsclient -cn -a "emacs" --frame-parameters="((fullscreen . maximized))"'
alias lr='lil-regy'
alias rm='rm -i'
alias file-count="find . -type f -print | wc -l"
alias bx="bundle exec"
alias gl='git lg'
alias gst='git st'
alias gb='git ba'
alias gwc='git whatchanged -p --abbrev-commit --pretty=medium'
alias hb="hub browse"
alias psx="ps ax | ag $1"
alias rss="newsboat -C ~/.newsboatrc -u ~/git/takeonrules.github.io/rss/urls.txt"
alias rss-init="newsboat -C ~/.newsboatrc -u ~/git/takeonrules.github.io/rss/urls.txt -i ~/git/takeonrules.github.io/rss/full-blogroll.opml"

# A convenience method to keep a few of the "long used" aliases
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    alias pbcopy="xclip"
    alias pbpaste="xsel"
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
    alias light=". $HOME/.config/base16-shell/scripts/base16-google-light.sh"
    alias dark=". $HOME/.config/base16-shell/scripts/base16-google-dark.sh"
fi

# Hyrax aliases
alias hyrax-devup='fcrepo_wrapper & solr_wrapper & redis-server &'
alias hyrax-testup='fcrepo_wrapper --config config/fcrepo_wrapper_test.yml & solr_wrapper --config config/solr_wrapper_test.yml & solr_wrapper --config config/solr_wrapper_valkyrie_test.yml & redis-server &'
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
