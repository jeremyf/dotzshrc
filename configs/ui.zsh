POWERLEVEL9K_PROMPT_ON_NEWLINE=true
POWERLEVEL9K_PROMPT_ADD_NEWLINE=true
POWERLEVEL9K_COLOR_SCHEME='dark'

POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(aws dir newline vcs rbenv)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status command_execution_time date time)

POWERLEVEL9K_RBENV_PROMPT_ALWAYS_SHOW=true

POWERLEVEL9K_DATE_FORMAT="%D{%Y-%m-%d}"
POWERLEVEL9K_TIME_FORMAT="%D{%H:%M:%S %Z}"

POWERLEVEL9K_SHOW_CHANGESET=true # Show the VCS SHA in the vcs segment
POWERLEVEL9K_CHANGESET_HASH_LENGTH=8
POWERLEVEL9K_VCS_GIT_HOOKS=(vcs-detect-changes git-untracked git-aheadbehind git-remotebranch git-tagname)

# Assumes that you have Hack Nerd Font installed (https://github.com/Powerlevel9k/powerlevel9k/wiki/Install-Instructions#option-4-install-nerd-fonts)
POWERLEVEL9K_MODE='nerdfont-complete'
ZSH_THEME='powerlevel9k/powerlevel9k'

# Some great color options
if [ -f $HOME/.config/base16-shell/base16-shell.plugin.zsh ]; then
  source $HOME/.config/base16-shell/base16-shell.plugin.zsh
fi
