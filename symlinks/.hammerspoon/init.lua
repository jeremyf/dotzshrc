local meh = {"ctrl", "alt", "cmd"}

hs.loadSpoon("editWithEmacs")
hs.loadSpoon("MiroWindowsManager")

hs.window.animationDuration = 0.3
spoon.MiroWindowsManager:bindHotkeys({
  up = {meh, "up"},
  right = {meh, "right"},
  down = {meh, "down"},
  left = {meh, "left"},
  fullscreen = {meh, "="}
})
