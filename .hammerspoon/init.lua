-- In order to use this
-- Hammerspoon needs to be allowed in
-- Privacy > Accessibility

local hyper = {"cmd", "alt", "ctrl"}
local hypershift = {"cmd", "alt", "ctrl", "shift"}

local logger = hs.logger.new('config', 'debug')
hs.hotkey.bind(hyper, "w", function()
    hs.alert.show("Hello World!")
end)

function h_bind(key, func)
    hs.hotkey.bind(hyper, key, func)
end

function hs_bind(key, func)
    hs.hotkey.bind(hypershift, key, func)
end

h_bind("r", hs.reload)
hs_bind("r", hs.toggleConsole)


function file_exists(path)
    local f=io.open(path,"r")
    if f~=nil then io.close(f) return true else return false end
    -- ~= is != in other languages
end

-- window functions
function positionWindow(x, y, w, h)
    return function()
        local win = hs.window.focusedWindow()
        if win == nil then return end
        local f = win:frame()
        local s = win:screen():frame()
        f.x = s.x + s.w * x
        f.y = s.y + s.h * y
        f.w = s.w * w
        f.h = s.h * h
        -- hs.alert.show(s)
        win:setFrame(f)
    end
end

hs.window.animationDuration = 0

h_bind("1", positionWindow(0, 0, 1/2, 1))
h_bind("2", positionWindow(1/2, 0, 1/2, 1))

h_bind("u", positionWindow(0, 0, 1/2, 1/2))
h_bind("i", positionWindow(1/2, 0, 1/2, 1/2))
h_bind("j", positionWindow(0, 1/2, 1/2, 1/2))
h_bind("k", positionWindow(1/2, 1/2, 1/2, 1/2))

h_bind("q", positionWindow(0, 0, 2/3, 1))
h_bind("w", positionWindow(2/3, 0, 1/3, 1))

-- grid based window functions
-- cols, rows
hs.grid.setGrid('10x8')
hs.grid.setMargins('0x0')

h_bind("right", function()
    local win = hs.window.focusedWindow()
    if win == nil then return end
    local screen = win:screen()
    local sg = hs.grid.getGrid(screen)
    local g = hs.grid.get(win)
    if g.x + g.w == sg.w then
        g.x = g.x + 1
        g.w = g.w - 1
        hs.grid.set(win, g)
    else
        g.w = g.w + 1
        hs.grid.set(win, g)
    end
end)

h_bind("left", function()
    local win = hs.window.focusedWindow()
    if win == nil then return end
    local screen = win:screen()
    local sg = hs.grid.getGrid(screen)
    local g = hs.grid.get(win)
    if g.x + g.w >= sg.w and g.x ~= 0 then
        g.x = g.x - 1
        g.w = g.w + 1
        hs.grid.set(win, g)
    else
        g.w = g.w - 1
        hs.grid.set(win, g)
    end
end)

h_bind("down", function()
    local win = hs.window.focusedWindow()
    if win == nil then return end
    local screen = win:screen()
    local sg = hs.grid.getGrid(screen)
    local g = hs.grid.get(win)
    if g.y + g.h == sg.h then
        g.y = g.y + 1
        g.h = g.h - 1
        hs.grid.set(win, g)
    else
        g.h = g.h + 1
        hs.grid.set(win, g)
    end
end)

h_bind("up", function()
    local win = hs.window.focusedWindow()
    if win == nil then return end
    local screen = win:screen()
    local sg = hs.grid.getGrid(screen)
    local g = hs.grid.get(win)
    if g.y + g.h >= sg.h and g.y ~= 0 then
        g.y = g.y - 1
        g.h = g.h + 1
        hs.grid.set(win, g)
    else
        g.h = g.h - 1
        hs.grid.set(win, g)
    end
end)


hs_bind("right", hs.grid.pushWindowRight)
hs_bind("left", hs.grid.pushWindowLeft)
hs_bind("down", hs.grid.pushWindowDown)
hs_bind("up", hs.grid.pushWindowUp)

h_bind('n', function()
    -- get the focused window
    local win = hs.window.focusedWindow()
    local isFullScreen = win:isFullScreen()

    if isFullScreen then
        win:setFullScreen(false)
    end
    
    -- get the screen where the focused window is displayed, a.k.a. current screen
    local screen = win:screen()
    win:move(win:frame():toUnitRect(screen:frame()), screen:next(), true, 0)

    if isFullScreen then
        hs.timer.doAfter(0.6, function()
        win:setFullScreen(true)
        end)
    end
end)

h_bind('f', function()
    local win = hs.window.focusedWindow()
    win:toggleFullscreen()
end)

hs.alert.show("Hammerspoon started")
