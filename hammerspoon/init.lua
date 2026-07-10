-- Function to toggle Ghostty's visibility
local function toggleGhosttyVisibility()
  local app = hs.application.find("Ghostty")
  if app then
    -- If Ghostty is the frontmost application, hide it
    if app:isFrontmost() then
      app:hide()
    else
      -- Otherwise, bring it to the front
      app:activate()
    end
  else
    -- If Ghostty isn't running, launch it
    hs.application.launchOrFocus("Ghostty")
  end
end

-- Needs to match the keybinding in ghostty/config
hs.hotkey.bind({"alt"}, "space", toggleGhosttyVisibility)

-- Function to toggle Emacs's visibility
local function toggleEmacsVisibility()
  local app = hs.application.find("Emacs")
  if app then
    -- If Emacs is the frontmost application, hide it
    if app:isFrontmost() then
      app:hide()
    else
      -- Otherwise, bring it to the front
      app:activate()
    end
  else
    -- If Emacs isn't running, launch it
    hs.application.launchOrFocus("Emacs")
  end
end

-- Cmd+Ctrl+E avoids Emacs's Meta (Option) and Super (Cmd) bindings
hs.hotkey.bind({"cmd", "ctrl"}, "e", toggleEmacsVisibility)