if [[ -d $HOME/elixir-ls/elixir-ls-v0.28.0 ]]; then
    export PATH="$HOME/elixir-ls/elixir-ls-v0.28.0:$PATH"
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
    brew_prefix=$(brew --prefix)
    export PATH="$brew_prefix/opt/libxml2/bin:$HOME/.cargo/bin:$brew_prefix/opt/swagger-codegen@2/bin:$PATH"
    export PATH="$brew_prefix/opt/icu4c/sbin:$brew_prefix/opt/icu4c/bin:$PATH"

    export PATH="$(realpath `brew --prefix graphviz`)/bin:$PATH"

    export PATH="$brew_prefix/opt/openjdk/bin:$PATH"
    echo "$PATH" | grep -q "$brew_prefix/sbin:" || export PATH="$brew_prefix/sbin:$PATH"
fi


export PATH="$HOME/.local/bin:$HOME/bin:$PATH"

# TODO: I would love to get rid of this particular nightmare of personal
# configuration I've carried since the days of bendo.
export GO111MODULE=on
if [[ -d $HOME/.local/go ]]; then
    export GOPROXY=https://proxy.golang.org,direct
    export GOTOOLCHAIN=auto
    export GOROOT=$HOME/.local/go
    export GOPATH=$HOME/go
else
    if command -v brew &> /dev/null; then
        export GOROOT="$(brew --prefix go)/libexec"
        export GOPATH=$HOME/go
    fi
fi
if [[ -d $GOPATH ]]; then
    echo "$PATH" | grep -q "$GOPATH" || export PATH="$PATH:$GOPATH/bin"
fi

if command -v brew &> /dev/null; then
    if [ -d "$(brew --prefix)/share/google-cloud-sdk/bin" ]; then
        export PATH="$(brew --prefix)/share/google-cloud-sdk/bin:$PATH"
    fi
fi


export PATH="$HOME/.local/emacs/bin:$PATH"


if [ -f $HOME/.local/zsh-exports ]; then
    source $HOME/.local/zsh-exports
fi
