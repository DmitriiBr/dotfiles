return {
	"stevearc/oil.nvim",
	---@module 'oil'
	---@type oil.SetupOpts
	opts = {
		default_file_explorer = true,
	},
	config = function()
		require("oil").setup()

		vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })
	end,
	-- Optional dependencies
	dependencies = { { "echasnovski/mini.icons", opts = {} } },
}
