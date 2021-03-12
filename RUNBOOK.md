# OS X

Install homebrew - https://brew.sh ; package manager
Install OhMyZsh - https://github.com/ohmyzsh/ohmyzsh ; zshell option
Install Karabiner-Elements - https://karabiner-elements.pqrs.org ; over-ride keyboard options

*   `brew tap d12frosted/emacs-plus; brew install emacs-plus@27 --with-dbus`
*   `brew cask install font-hack-nerd-font`
*   `brew cask install jumpcut` Multi-paste buffer
*   `brew install ag` A solid replacement for grep
*   `brew install git-extras`
*   `brew install git-secrets`
*   `brew install git`
*   `brew install openssl` You're going to want this for rbenv, though look at ./runbooks/ruby-openssl
*   `brew install postgresql`
*   `brew install rbenv`
*   `brew install redist`
*   `brew install ripgrep` Another solid replacement for grep that allows PERL Regexp
*   `brew install terminal-notifier`
*   `brew tap homebrew/cask-fonts`
*   `brew tap homebrew/cask`

## Necessary for some takeonrules dependencies

*   `brew install gmp`
*   `brew install gsl`

## SSH Keys

Ensure that I have up to date SSH keys

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
