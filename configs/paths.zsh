if [ -d /usr/local/opt/mysql@5.7/bin ]; then
  export PATH="/usr/local/opt/mysql@5.7/bin:$PATH"
  # Needed for building mysql gem
  # export LDFLAGS="-L/usr/local/opt/mysql@5.7/lib"
  # export CPPFLAGS="-I/usr/local/opt/mysql@5.7/include"
fi
if [ -d /usr/local/opt/libxml2/bin ]; then
  export PATH="/usr/local/opt/libxml2/bin:$PATH"
  # export LDFLAGS="-L/usr/local/opt/libxml2/lib"
  # export CPPFLAGS="-I/usr/local/opt/libxml2/include"
fi

# Ensuring homebrew install of python is selected over OSX's
# ancient python
if [ -d "/usr/local/opt/python/libexec/bin" ]; then
  export PATH="/usr/local/opt/python/libexec/bin:$PATH"
fi

if [ -d "/usr/local/opt/icu4c/bin" ]; then
  export PATH="/usr/local/opt/icu4c/bin:$PATH"
fi

if [ -d "/usr/local/opt/icu4c/sbin" ]; then
  export PATH="/usr/local/opt/icu4c/sbin:$PATH"
fi

if [ -d "/usr/local/Cellar/graphviz/2.42.3/bin" ]; then
  export PATH="/usr/local/Cellar/graphviz/2.42.3/bin:$PATH"
fi

# This helps in building out Ruby using openssl
# It may require PATH-ing /usr/local/Cellar/openssl@1.1/bin (see below)
export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@1.1)"

# I'm not certain if this is a good idea or not
if [ -d /usr/local/opt/openssl@1.1/bin ]; then
  export PATH="$(brew --prefix openssl@1.1)/bin:$PATH"
fi

if [ -d $HOME/bin ]; then
  export PATH="$HOME/bin:$PATH"
fi
if [ -d /usr/local/sbin ]; then
  echo "$PATH" | grep -q "/usr/local/sbin:" || export PATH="/usr/local/sbin:$PATH"
fi

export GOROOT=/usr/local/opt/go/libexec/
export GOPATH=$HOME/code_of_the_go
if [ -d $GOPATH ]; then
  echo "$PATH" | grep -q "$GOPATH" || export PATH="$PATH:$GOPATH/bin"
fi
