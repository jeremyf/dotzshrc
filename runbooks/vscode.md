# Enabling Commit Messages via File Editing

At the time of this writing, I felt the default commit message input dialogue of VS Code encouraged terse and rather crappy commit messages. I wanted a way to write my commit messages in a text editor.

What follows is my 30 minute hack on exposing a mechanism to edit commit messages.

## Steps to Follow

In your shell profile add a line: `export GIT_EDITOR="code --wait"`. Here's an example of [my shell profile](https://github.com/jeremyf/dotzshrc/blob/master/configs/config.zsh#L89). Note, I am still using `atom` as my VISUAL preference.

Then add a task to `~/Library/Application\ Support/Code/User/tasks.json`:

```json
{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "write-git-commit-message",
      "type": "shell",
      "command": "git commit",
      "problemMatcher": [],
      "presentation": {
        "reveal": "always",
        "focus": true
      }
    }
  ]
}
```

Then add a keybinding to `~/Library/Application\ Support/Code/User/keybindings.json`:

```json
[
  {
    "key": "cmd+k cmd+c",
    "command": "workbench.action.tasks.runTask",
    "args": "write-git-commit-message"
  }
]
```

The `args` of the key binding match the label of the `task`. I chose the key sequence based on what I have configured for Atom.
