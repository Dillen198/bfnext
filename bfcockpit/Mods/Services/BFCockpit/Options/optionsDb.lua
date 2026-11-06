-- Settings for Options -> Special -> Vector Strike. DCS stores them in
-- Config\options.lua as options.plugins["BFCockpit"], where the Hooks script
-- (Scripts\Hooks\bfcockpit.lua) reads them. Each key here is bound to the
-- widget named <key><Control> in options.dlg (toggleKeyComboList, ...).
local DbOption = require("Options.DbOption")
local i18n     = require("i18n")
local _        = i18n.ptranslate

-- Key names exactly as dxgui spells them (DCS World\dxgui\bind\KeyNames.txt),
-- because the value goes straight into Window:addHotKeyCallback. Names with
-- a space in them ("page up") are left out: the hotkey parser splits on
-- whitespace.
local keys = {}
local function key(label, value) keys[#keys + 1] = DbOption.Item(label):Value(value) end
for c in ("abcdefghijklmnopqrstuvwxyz"):gmatch(".") do key(c:upper(), c) end
for d = 0, 9 do key(tostring(d), tostring(d)) end
for f = 1, 12 do key("F" .. f, "f" .. f) end
for d = 0, 9 do key("Num " .. d, "[" .. d .. "]") end
key("Insert", "insert")
key("Delete", "delete")
key("Home", "home")
key("End", "end")
key("Pause", "pause")
key("`", "`")

return {
	toggleKey   = DbOption.new():setValue("c"):combo(keys),
	toggleCtrl  = DbOption.new():setValue(true):checkbox(),
	toggleAlt   = DbOption.new():setValue(true):checkbox(),
	toggleShift = DbOption.new():setValue(false):checkbox(),
	openOnComms = DbOption.new():setValue(true):checkbox(),
	autoUpdate  = DbOption.new():setValue(true):checkbox(),
	wheelKey    = DbOption.new():setValue("w"):combo(keys),
	wheelCtrl   = DbOption.new():setValue(true):checkbox(),
	wheelAlt    = DbOption.new():setValue(true):checkbox(),
	wheelShift  = DbOption.new():setValue(false):checkbox(),
	wheelHead   = DbOption.new():setValue(true):checkbox(),
	wheelReach  = DbOption.new():setValue(10):combo({
		DbOption.Item(_("Short -- 6 degrees")):Value(6),
		DbOption.Item(_("Medium -- 10 degrees")):Value(10),
		DbOption.Item(_("Long -- 15 degrees")):Value(15),
		DbOption.Item(_("Very long -- 22 degrees")):Value(22),
	}),
	wheelDwell  = DbOption.new():setValue(0):combo({
		DbOption.Item(_("Off -- click or press the key")):Value(0),
		DbOption.Item(_("0.8 seconds")):Value(0.8),
		DbOption.Item(_("1.2 seconds")):Value(1.2),
		DbOption.Item(_("2 seconds")):Value(2.0),
	}),
	wheelSize   = DbOption.new():setValue(1.0):combo({
		DbOption.Item("60%"):Value(0.6),
		DbOption.Item("70%"):Value(0.7),
		DbOption.Item("80%"):Value(0.8),
		DbOption.Item("100%"):Value(1.0),
		DbOption.Item("120%"):Value(1.2),
		DbOption.Item("150%"):Value(1.5),
	}),
	wheelTheme  = DbOption.new():setValue("green"):combo({
		DbOption.Item(_("Vector Strike green")):Value("green"),
		DbOption.Item(_("Blue")):Value("blue"),
		DbOption.Item(_("Amber")):Value("amber"),
		DbOption.Item(_("Red")):Value("red"),
		DbOption.Item(_("White")):Value("white"),
	}),
	wheelOpacity = DbOption.new():setValue(0.9):combo({
		DbOption.Item("100%"):Value(1.0),
		DbOption.Item("90%"):Value(0.9),
		DbOption.Item("75%"):Value(0.75),
		DbOption.Item("60%"):Value(0.6),
	}),
	wheelNav    = DbOption.new():setValue("arrows"):combo({
		DbOption.Item(_("Arrow keys, Enter, Backspace")):Value("arrows"),
		DbOption.Item(_("Numpad 4 6 8 2, 5, 0")):Value("numpad"),
		DbOption.Item(_("Ctrl+Alt + arrows, Enter, Backspace")):Value("ctrlalt"),
		DbOption.Item(_("Off")):Value("off"),
	}),
	wheelOpenAt = DbOption.new():setValue("center"):combo({
		DbOption.Item(_("Centre of the screen")):Value("center"),
		DbOption.Item(_("Where the mouse is")):Value("cursor"),
	}),
	uiScale     = DbOption.new():setValue(0):combo({
		DbOption.Item(_("Auto (fits your screen)")):Value(0),
		DbOption.Item("60%"):Value(0.6),
		DbOption.Item("70%"):Value(0.7),
		DbOption.Item("80%"):Value(0.8),
		DbOption.Item("90%"):Value(0.9),
		DbOption.Item("100%"):Value(1.0),
		DbOption.Item("115%"):Value(1.15),
		DbOption.Item("130%"):Value(1.3),
		DbOption.Item("150%"):Value(1.5),
		DbOption.Item("175%"):Value(1.75),
		DbOption.Item("200%"):Value(2.0),
	}),
	textScale   = DbOption.new():setValue(1.0):combo({
		DbOption.Item("70%"):Value(0.7),
		DbOption.Item("80%"):Value(0.8),
		DbOption.Item("90%"):Value(0.9),
		DbOption.Item("100%"):Value(1.0),
		DbOption.Item("115%"):Value(1.15),
		DbOption.Item("130%"):Value(1.3),
	}),
}
