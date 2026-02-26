if [[ -d $HOME/elixir-ls/elixir-ls-v0.28.0 ]]; then
    export PATH="$HOME/elixir-ls/elixir-ls-v0.28.0:$PATH"
fi

if command -v brew &> /dev/null; then
    if [ -d "$(brew --prefix swagger-codegen@2)" ]; then
        export PATH="$(brew --prefix swagger-codegen@2)/bin:$PATH"
    fi
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
    if [ -d $HOME/.cargo/bin ]; then
        export PATH="$HOME/.cargo/bin:$PATH"
    fi

    if [ -d "$(brew --prefix)/opt/libxml2/bin" ]; then
        export PATH="$(brew --prefix)/opt/libxml2/bin:$PATH"
        # export LDFLAGS="-L$HB_PATH/opt/libxml2/lib"
        # export CPPFLAGS="-I$HB_PATH/opt/libxml2/include"
    fi

    if [ -d "$(brew --prefix icu4c)/bin" ]; then
        export PATH="$(brew --prefix icu4c)/bin:$PATH"
        export PATH="$(brew --prefix icu4c)/sbin:$PATH"
    fi

    if [ -d "$(brew --prefix graphviz)" ]; then
        export PATH="$(realpath `brew --prefix graphviz`)/bin:$PATH"
    fi

    # This helps in building out Ruby using openssl
    # It may require PATH-ing $HB_PATH/Cellar/openssl@1.1/bin (see below)
    export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@3)"

    # I'm not certain if this is a good idea or not
    if [ -d "$(brew --prefix openssl@3)/bin" ]; then
        export PATH="$(brew --prefix openssl@3)/bin:$PATH"
    fi


    if [ -d "$(brew --prefix java)" ]; then
        export PATH="$(brew --prefix java)/bin:$PATH"
    fi
    # I saw this behavior in OS X, where "$(brew --prefix)/sbin" was not in the path
    if [ -d "$(brew --prefix)/sbin" ]; then
        echo "$PATH" | grep -q "$(brew --prefix)/sbin:" || export PATH="$(brew --prefix)/sbin:$PATH"
    fi
    if [ -d "$(brew --prefix asdf)/libexec/asdf.sh" ]; then
        export "$(brew --prefix asdf)/libexec/asdf.sh"
    fi
fi

if [ -d $HOME/.emacs.d/bin ]; then
    export PATH="$HOME/.emacs.d/bin:$PATH"
fi

if [ -d $HOME/bin ]; then
    export PATH="$HOME/bin:$PATH"
fi


if [ -d $HOME/.local/bin ]; then
    export PATH="$HOME/.local/bin:$PATH"
fi

export GO111MODULE=on
if [[ -d $HOME/.local/go ]]; then
    export GOPROXY=https://proxy.golang.org,direct
    export GOTOOLCHAIN=auto
    export GOROOT=$HOME/.local/go
    export GOPATH=$HOME/go
else
    export GOROOT="$(brew --prefix go)/libexec"
    export GOPATH=$HOME/go
fi
if [[ -d $GOPATH ]]; then
    echo "$PATH" | grep -q "$GOPATH" || export PATH="$PATH:$GOPATH/bin"
fi

if [ -d $HOME/.local/emacs/bin ]; then
    export PATH="$HOME/.local/emacs/bin:$PATH"
fi
