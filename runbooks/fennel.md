Working with [fennel](https://fennel-lang.org/) and [lua](https://www.lua.org/), I ran the following:

- `brew install lua`
- `brew install luarocks` (following on install instructions https://github.com/bakpakin/Fennel/)
- `luarocks --local install fennel`
- `ln -sf ~/.luarocks/bin/fennel ~/bin/fennel`

I needed to manually build readline, assumes Applie Silicon:

- Downloaded version of Readline TAR that maps to homebrew install version (https://git.savannah.gnu.org/cgit/readline.git?h=devel)
- `tar xvfz readline-8.1.tar.gz`
- `mkdir -p /opt/homebrew/readline/8_1`
- `./configure --prefix=/opt/homebrew/readline/8_1`
- `make`
- `make install`

Then run the following: (note the HISTORY_DIR and READLINE_DIR are different)
- `luarocks --local install readline HISTORY_DIR=/opt/homebrew/readline/8_1 READLINE_DIR=/opt/homebrew/Cellar/readline/8.1/`