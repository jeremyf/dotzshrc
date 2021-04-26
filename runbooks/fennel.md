# Fennel (on OS X)

Working with [fennel](https://fennel-lang.org/) and [lua](https://www.lua.org/), I ran the following:

- `brew install lua`
- `brew install luarocks` (following on install instructions https://github.com/bakpakin/Fennel/)
- `luarocks --local install fennel`; this creates `~/.luarocks`
- `ln -sf ~/.luarocks/bin/fennel ~/bin/fennel`; because I have not added `~/.luarocks/bin` to my path.  I may want to do this in the future.

Next, I wanted `readline` for the `fennel`'s REPL.

Originally, I downloaded and built readline.  However, that proved unnecessary.

- `brew install readline`
- `luarocks --local install readline HISTORY_DIR=/opt/homebrew/Cellar/readline/8.1 READLINE_DIR=/opt/homebrew/Cellar/readline/8.1/`

Where `/opt/homebrew` is my homebrew path (which I set via `$HB_PATH`).

## Hack Version

This was what I did for downloading and building readline from source; partially circumventing `homebrew`

I needed to manually build readline, assumes Applie Silicon:

- Downloaded version of Readline TAR that maps to homebrew install version (https://git.savannah.gnu.org/cgit/readline.git?h=devel)
- `tar xvfz readline-8.1.tar.gz`
- `mkdir -p /opt/homebrew/readline/8_1`
- `./configure --prefix=/opt/homebrew/readline/8_1`
- `make`
- `make install`

Then run the following: (note the HISTORY_DIR and READLINE_DIR are different)

- `luarocks --local install readline HISTORY_DIR=/opt/homebrew/readline/8_1 READLINE_DIR=/opt/homebrew/Cellar/readline/8.1/`
