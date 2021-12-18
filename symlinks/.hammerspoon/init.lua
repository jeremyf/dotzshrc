local meh = {"ctrl", "alt", "cmd"}
local hyper = {"ctrl", "alt", "cmd", "shift"}

-- I have a custom command for my editor:
-- https://github.com/jeremyf/dotzshrc/blob/main/bin/editor
hs.loadSpoon("editWithEmacs")
if spoon.editWithEmacs then
   spoon.editWithEmacs.openEditorShellCommand = "editor -e '(hammerspoon-edit-begin)'"
   local bindings = {
      edit_selection =  { hyper, "e"},
      edit_all       = { meh, "e"}
   }
   spoon.editWithEmacs:bindHotkeys(bindings)
end

hs.loadSpoon("MiroWindowsManager")
hs.window.animationDuration = 0.3
spoon.MiroWindowsManager:bindHotkeys({
      up = {meh, "up"},
      right = {meh, "right"},
      down = {meh, "down"},
      left = {meh, "left"},
      fullscreen = {meh, "="}
})