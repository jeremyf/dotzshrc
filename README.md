A git repository for managing my .zshrc configuration

Two key concepts:

* RUNBOOK.md - this is the software/scripts I install to ensure I have
  the appropirate dependencies installed.
* install.rb - this will create the symlinks and ensure your paths,
  aliases, etc are correct in the terminal.

You can run `ruby install.rb` to create the symlinks into the current
$HOME file

```
├── License
├── README.md
├── RUNBOOK.md
├── bin
├── configs
├── install.rb
├── iterm
├── runbooks
├── stylesheets
└── symlinks
```

## Emacs and Emacsclient

I have added two aliases: `e` and `edaemon`

## About Org

I make extensive use of Emacs [Orgmode](https://orgmode.org/) and [Org-Roam](https://orgroam.com/).  See [README-ORG](README-ORG.org).
