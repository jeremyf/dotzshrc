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
└── symlinks
```

What's the deal with `emacs` and `emacs.d`?  The `install.rb` script
writes files from `emacs.d` directory to the `~/.emacs.d`.  The
`emacs` directory contains things referenced in my `.emacs` file.  I'm
also working to figure out how best to arrange things.
