local meh = {"ctrl", "alt", "cmd"}
local hyper = {"ctrl", "alt", "cmd", "shift"}

hs.loadSpoon("editWithEmacs")

  -- Create the keybindings for editing:
  --
  -- Edit by selecting everything in the current focus.
  hs.hotkey.bind(meh, 'e', nil, function()
     edit_in_emacs(true)
  end)

  -- Edit by using only the selected text of the current focus.
  hs.hotkey.bind(hyper, 'e', nil, function()
     edit_in_emacs(false)
  end)


hs.loadSpoon("MiroWindowsManager")
hs.window.animationDuration = 0.3
spoon.MiroWindowsManager:bindHotkeys({
  up = {meh, "up"},
  right = {meh, "right"},
  down = {meh, "down"},
  left = {meh, "left"},
  fullscreen = {meh, "="}
})