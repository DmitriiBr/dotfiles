local Config = require('./vscode/Config')
local Keybinds = require('./vscode/Keybinds')

if vim.g.vscode then
	local config = Config();
	local keybinds = Keybinds();

	config.create()

	keybinds.disable_arrows()
	keybinds.navigation()
	keybinds.splitting()
	keybinds.diagnostics()
else
	-- Main Neovim config in ./init.lua
end
