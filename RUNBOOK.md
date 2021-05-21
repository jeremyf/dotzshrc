# OS X

Install homebrew - https://brew.sh ; package manager
Install OhMyZsh - https://github.com/ohmyzsh/ohmyzsh ; zshell option
Install Powerlevel10k - https://github.com/romkatv/powerlevel10k
Install MesloLGS NF via Powerlevel10k instructions
Install Base16 Shell - https://github.com/chriskempson/base16-shell
Install ImageOptim - https://imageoptim.com/ ; remove image metadata

*   `brew cask install font-hack-nerd-font`
*   `brew cask install jumpcut` Multi-paste buffer; set Sticky Bezel and launch at login
*   `brew install ag` A solid replacement for grep
*   `brew install cmake`
*   `brew install git`
*   `brew install git-extras`
*   `brew install git-secrets`
*   `brew install hub`
*   `brew install openssl` You're going to want this for rbenv, though look at ./runbooks/ruby-openssl
*   `brew install postgresql`
*   `brew install rbenv`
*   `brew install redis`
*   `brew install ripgrep` Another solid replacement for grep that allows PERL Regexp
*   `brew install terminal-notifier`
*   `brew tap homebrew/cask-fonts`
*   `brew tap homebrew/cask`
*   `brew install coreutils`
*   `brew install pandoc`
*   `brew install mactex`
*   `ln -s $HB_PATH/bin/gls $HB_PATH/bin/ls` ; This addresses "*ERROR*: Listing directory failed but ‘access-file’ worked"

## Emacs

### From Homebrew

*   `brew tap d12frosted/emacs-plus`
*   `brew install emacs-plus --with-debug --with-dbus --with-xwidgets --with-modern-pen-lds56-icon`

Create symlinks in `~/bin/` of emacs and emacsclient; note the `editor` and `edaemon` files require `~/bin/emacs` and `~/bin/emacsclient`.  In adding that symlink indirection, I'm able to bring the best Emacs installation to my ecosystem and configure with that point of inflection.

## Download JetBrains Mono

Install this font as it's used in Emacs configuration.

## Necessary for some takeonrules dependencies

*   `brew install gmp`
*   `brew install gsl`

## SSH Keys

Ensure that I have up to date SSH keys

For Github, need to create a new app token

## Install Karabiner

https://karabiner-elements.pqrs.org/

Then set:

  *  Right option key to send ESC
  *  Caps lock to send CTRL
  *  Left shift + Right shift sends CAPS LOCK

# Manjaro

* Install rbenv
* Install "ag": `pacman -S the_silver_searcher`
* Install OhMyZsh - https://github.com/ohmyzsh/ohmyzsh
* Install Powerlevel10k - https://github.com/romkatv/powerlevel10k#oh-my-zsh
* Install xclip and xsel: `pacman -S xclip xsel`
* Install emacs
