if [[ "$OSTYPE" == "darwin"* ]]; then
    if [ -d $HOME/.cargo/bin ]; then
	export PATH="$HOME/.cargo/bin:$PATH"
    fi
    if [ -d $HB_PATH/opt/mysql@5.7/bin ]; then
        export PATH="$HB_PATH/opt/mysql@5.7/bin:$PATH"
        # Needed for building mysql gem
        # export LDFLAGS="-L$HB_PATH/opt/mysql@5.7/lib"
       # export CPPFLAGS="-I$HB_PATH/opt/mysql@5.7/include"
   fi
   if [ -d $HB_PATH/opt/libxml2/bin ]; then
       export PATH="$HB_PATH/opt/libxml2/bin:$PATH"
       # export LDFLAGS="-L$HB_PATH/opt/libxml2/lib"
       # export CPPFLAGS="-I$HB_PATH/opt/libxml2/include"
   fi

   # Ensuring homebrew install of python is selected over OSX's
   # ancient python
   if [ -d "$HB_PATH/opt/python/libexec/bin" ]; then
       export PATH="$HB_PATH/opt/python/libexec/bin:$PATH"
   fi

    if [ -d "$HB_PATH/opt/icu4c/bin" ]; then
        export PATH="$HB_PATH/opt/icu4c/bin:$PATH"
    fi

    if [ -d "$HB_PATH/opt/icu4c/sbin" ]; then
        export PATH="$HB_PATH/opt/icu4c/sbin:$PATH"
    fi

    if [ -d "$(brew --prefix graphviz)" ]; then
        export PATH="$(realpath `brew --prefix graphviz`)/bin:$PATH"
    fi

    # This helps in building out Ruby using openssl
    # It may require PATH-ing $HB_PATH/Cellar/openssl@1.1/bin (see below)
    export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@3)"

    # I'm not certain if this is a good idea or not
    if [ -d $HB_PATH/opt/openssl@3/bin ]; then
        export PATH="$(brew --prefix openssl@3)/bin:$PATH"
    fi

    if [ -d $HB_PATH/lib/pkgconfig ]; then
        export PKG_CONFIG_PATH=/opt/local/lib/pkgconfig
    fi

    if [ -d $HB_PATH/opt/openjdk/bin ]; then
	export PATH="$(brew --prefix java)/bin:$PATH"
    fi
    # I saw this behavior in OS X, where $HB_PATH/sbin was not in the path
    if [ -d $HB_PATH/sbin ]; then
	echo "$PATH" | grep -q "$HB_PATH/sbin:" || export PATH="$HB_PATH/sbin:$PATH"
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
    export GOROOT=$HB_PATH/opt/go/libexec/
    export GOPATH=$HOME/go
fi
if [ -d $GOPATH ]; then
    echo "$PATH" | grep -q "$GOPATH" || export PATH="$PATH:$GOPATH/bin"
fi

if [ -d $HOME/.local/emacs/bin ]; then
    export PATH="$HOME/.local/emacs/bin:$PATH"
fi
