local Config = require('./Config')

local function get_error(message)
	local NOT_DEFINED = "NOT DEFINED: "
	return NOT_DEFINED .. message
end

if vim.g.vscode then
	Config.create()

	local vscode = require("vscode")
	local function set_key(mode, keybind, vscode_action_id, desc)
		if mode == nil then error(get_error("Argument [mode] is not defined")) end
		if keybind == nil then error(get_error("Argument [keybind] is not defined")) end
		if vscode_action_id == nil then error(get_error("Argument [vscode_action_id] is not defined")) end
		if desc == nil then error(get_error("Argument [desc] is not defined")) end

		vim.keymap.set(mode, keybind, function() vscode.call(vscode_action_id) end, { desc = desc })
	end

	-- Go to Files
	set_key("n", "<leader>f", "workbench.action.quickOpen", "Go to [F]ile")

	-- Search in files
	set_key("n", "<leader>t", "workbench.action.findInFiles", "Search in files")

	-- Keybinds to make split navigation easier.
	set_key("n", "<leader>sr", "workbench.action.splitEditorRight", "[S]plit [R]ight")
	set_key("n", "<leader>sl", "workbench.action.splitEditorLeft", "[S]plit [L]eft")

	-- Keybinds to make split navigation easier.
	set_key("n", "<C-k>", "workbench.action.focusAboveGroup", "Move focus to the upper window")
	set_key("n", "<C-l>", "workbench.action.focusRightGroup", "Move focus to the right window")
	set_key("n", "<C-j>", "workbench.action.focusBelowGroup", "Move focus to the lower window")
	set_key("n", "<C-h>", "workbench.action.focusLeftGroup", "Move focus to the left window")

	-- Diagnostic keymaps
	set_key("n", "[d", "editor.action.marker.prev", "Go to previous [D]iagnostic message")
	set_key("n", "]d", "editor.action.marker.next", "Go to next [D]iagnostic message")
	set_key("n", "<leader>e", "workbench.action.showErrorsWarnings", "Show diagnostic [E]rror messages")
	-- vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostic [Q]uickfix list" })
else
	Config.create()

	vim.opt.shiftwidth = 4
	vim.opt.tabstop = 4

	-- Diagnostics float window
	vim.diagnostic.config({
		float = { border = "rounded" },
	})

	-- Keybinds to make split navigation easier.
	-- vim.keymap.set("n", "<C-h>", "<C-w><C-h>", { desc = "Move focus to the left window" })
	-- vim.keymap.set("n", "<C-l>", "<C-w><C-l>", { desc = "Move focus to the right window" })
	-- vim.keymap.set("n", "<C-j>", "<C-w><C-j>", { desc = "Move focus to the lower window" })
	-- vim.keymap.set("n", "<C-k>", "<C-w><C-k>", { desc = "Move focus to the upper window" })


	-- Diagnostic keymaps
	-- vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Go to previous [D]iagnostic message" })
	-- vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Go to next [D]iagnostic message" })
	-- vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, { desc = "Show diagnostic [E]rror messages" })
	-- vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostic [Q]uickfix list" })
end
