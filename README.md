A git repository for managing my .zshrc configuration

You can run `ruby install.rb` to create the symlinks into the current
$HOME file

```
├── License
├── README.md
├── RUNBOOK.md
├── bin
├── configs
├── emacs
├── emacs.d
├── install.rb
├── iterm
├── runbooks
├── stylesheets
└── symlinks
```

What's the deal with `emacs` and `emacs.d`?  The `install.rb` script
writes files from `emacs.d` directory to the `~/.emacs.d`.  The
`emacs` directory contains things referenced in my `.emacs` file.  I'm
also working to figure out how best to arrange things.

## Emacs and Emacsclient

I have added two aliases: `e` and `edaemon`

### About Ivy/Swiper/Counsel

I find myself using:

* Swiper as my search in buffer; on OS X, I mapped <kbd>Cmd</kbd>+<kbd>s</kbd> to the command.
* `iedit-mode` and related `counsel-edit-mode` to multi-edit; in particular `counsel-edit-mode` from a search (via `ag`).

Those two behaviors are part of my workflow, and any changes should be included.

I am favoring Selectrum/Consult instead of Ivy.  The `embark` setup is fantastic.

### About Selectrum/Consult

I'm exploring Selectrum as it's a more narrow focus/cleaner API to the Emacs methods.  Which, based on history, leads me to believe that this solution will have a better chance of ongoing maintenance.

## About Org

I make extensive use of Emacs [Orgmode](https://orgmode.org/) and [Org-Roam](https://orgroam.com/).  See [README-ORG](README-ORG.org).
