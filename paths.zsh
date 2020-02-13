if [ -d /usr/local/opt/mysql@5.7/bin ]; then
  echo "$PATH" | grep -q "/usr/local/opt/mysql@5.7/bin:" || export PATH="/usr/local/opt/mysql@5.7/bin:$PATH"
  # Needed for building mysql gem
  export LDFLAGS="-L/usr/local/opt/mysql@5.7/lib"
  export CPPFLAGS="-I/usr/local/opt/mysql@5.7/include"
fi
if [ -d /usr/local/opt/openssl@1.1/bin ]; then
  echo "$PATH" | grep -q "/usr/local/opt/openssl@1.1/bin:" || export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"
fi
if [ -d $HOME/bin ]; then
  echo "$PATH" | grep -q "$HOME/bin:" || export PATH="$HOME/bin:$PATH"
fi
if [ -d /usr/local/sbin ]; then
  echo "$PATH" | grep -q "/usr/local/sbin:" || export PATH="/usr/local/sbin:$PATH"
fi
export GOROOT=/usr/local/opt/go/libexec/
export GOPATH=$HOME/code_of_the_go
if [ -d $GOPATH ]; then
  echo "$PATH" | grep -q "$GOPATH" || export PATH="$PATH:$GOPATH/bin"
fi
