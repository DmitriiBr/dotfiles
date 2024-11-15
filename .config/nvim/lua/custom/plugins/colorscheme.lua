-- If you want to see what colorschemes are already installed, you can use `:Telescope colorscheme`.
local ROSE_PINE = {
	"rose-pine/neovim",
	priority = 1000, -- Make sure to load this before all the other start plugins.
	name = "rose-pine",
	init = function()
		-- Load the colorscheme here.
		-- Like many other themes, this one has different styles, and you could load
		-- any other, such as 'tokyonight-storm', 'tokyonight-moon', or 'tokyonight-day'.
		vim.cmd.colorscheme("rose-pine")

		-- You can configure highlights by doing something like:
		vim.cmd.hi("Comment gui=none")
	end,
	config = function()
		require("rose-pine").setup({
			variant = "main",
			styles = {
				transparency = true,
				italic = false,
			},
			highlight_groups = {
				CursorLine = { bg = "none" },
			},
		})
	end,
}

local GITHUB = {
	"projekt0n/github-nvim-theme",
	name = "github-theme",
	lazy = false, -- make sure we load this during startup if it is your main colorscheme
	priority = 1000, -- make sure to load this before all the other start plugins
	config = function()
		require("github-theme").setup({
			options = {
				styles = {
					comments = "NONE", -- Value is any valid attr-list value `:help attr-list`
					functions = "bold",
					keywords = "NONE",
					variables = "bold",
					conditionals = "NONE",
					constants = "bold",
					numbers = "NONE",
					operators = "NONE",
					strings = "NONE",
					types = "bold",
				},
			},
		})

		vim.cmd("colorscheme github_light_default")
	end,
}

return GITHUB
