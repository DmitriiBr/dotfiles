return {
	"stevearc/oil.nvim",
	---@module 'oil'
	---@type oil.SetupOpts
	opts = {},
	config = function()
		local KEYBIND = "-"
		local OPEN_OIL_EXPLORER = "<CMD>Oil<CR>"

		require("oil").setup({
			keymaps = {
				["C-h"] = false,
				["M-h"] = "actions.select_split",
			},
			default_file_explorer = true,
			view_options = {
				show_hidden = true,
				is_always_hidden = function(name, _)
					return name == ".." or name == ".git"
				end,
			},
		})

		vim.keymap.set("n", KEYBIND, OPEN_OIL_EXPLORER, { desc = "Open parent directory" })
	end,
	-- Optional dependencies
	dependencies = { { "echasnovski/mini.icons", opts = {} } },
}
