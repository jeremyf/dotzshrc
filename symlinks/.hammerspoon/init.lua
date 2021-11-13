local meh = {"ctrl", "alt", "cmd"}
local hyper = {"ctrl", "alt", "cmd", "shift"}

hs.loadSpoon("editWithEmacs")

spoon.editWithEmacs.beginEditShellCommand = "editor -e '(hammerspoon-edit-begin)'"

spoon.editWithEmacs:bindHotkeys({
      selection = {hyper, "e"},
      all = {meh, "e"}
})

hs.loadSpoon("MiroWindowsManager")
hs.window.animationDuration = 0.3
spoon.MiroWindowsManager:bindHotkeys({
      up = {meh, "up"},
      right = {meh, "right"},
      down = {meh, "down"},
      left = {meh, "left"},
      fullscreen = {meh, "="}
})