-- Vector Strike: registers the plugin with DCS so its settings appear under
-- Options -> Special -> Vector Strike, with the Vector Strike icon. The panel
-- and wheel are the Hooks script (Scripts\Hooks\bfcockpit.lua), which reads
-- these settings back out of Config\options.lua (options.plugins["BFCockpit"]).
--
-- The id stays "BFCockpit": DCS stores the settings under it, so renaming the
-- id would throw away every player's choices. Only what players see is named
-- Vector Strike.
declare_plugin("BFCockpit", {
	installed     = true,
	dirName       = current_mod_path,
	developerName = _("Vector Strike"),
	developerLink = _("https://vectorstrike.org"),
	displayName   = _("Vector Strike"),
	version       = "3.3.0",
	state         = "installed",
	info          = _("Vector Strike in-game panel and wheel: the F10 menu, cargo with crate counts, status, EWR and every message the server sends you.\n\nSet the panel and wheel keys, head-tracking and sizes under Options -> Special -> Vector Strike."),
	Skins = {
		{ name = "BFCockpit", dir = "Theme" },
	},
	Options = {
		{ name = "Vector Strike", nameId = "BFCockpit", dir = "Options", allow_in_simulation = true },
	},
})

plugin_done()
