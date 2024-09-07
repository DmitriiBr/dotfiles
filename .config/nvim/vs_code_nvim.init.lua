local Config = require("vscode_settings.Config")
local Keybinds = require("vscode_settings.Keybinds")

if vim.g.vscode then
	local config = Config()
	local keybinds = Keybinds()

	config.setup()

	keybinds.disable_arrows()
	keybinds.navigation()
	keybinds.splitting()
	keybinds.diagnostics()
	keybinds.file_management()
else
	-- Main Neovim config in ./init.lua
end
