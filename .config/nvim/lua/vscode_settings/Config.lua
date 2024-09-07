-- Helps to define basic setup for nvim
local function ConfigConstructor()
	return {
		-- Setup basic config settings
		setup = function()
			vim.g.mapleader = " "
			vim.g.maplocalleader = " "

			vim.opt.winblend = 0

			-- Set to true if you have a Nerd Font installed and selected in the terminal
			vim.g.have_nerd_font = true

			-- Make cursor block-style
			vim.opt.guicursor = ""

			-- Decrease update time
			vim.opt.updatetime = 250

			-- Decrease mapped sequence wait time
			-- Displays which-key popup sooner
			vim.opt.timeoutlen = 300

			-- [[ Setting options ]]
			-- See `:help vim.opt`
			-- Make line numbers default
			vim.opt.number = true
			vim.opt.relativenumber = true

			-- Enable mouse mode
			vim.opt.mouse = "a"

			-- Don't show the mode, since it's already in the status line
			vim.opt.showmode = false

			-- Sync clipboard between OS and Neovim.
			vim.opt.clipboard = "unnamedplus"

			-- Enable break indent
			vim.opt.breakindent = true

			-- Save undo history
			vim.opt.undofile = true

			-- Case-insensitive searching UNLESS \C or one or more capital letters in the search term
			vim.opt.ignorecase = true
			vim.opt.smartcase = true

			-- Keep signcolumn on by default
			vim.opt.signcolumn = "yes"

			-- Decrease mapped sequence wait time
			-- Displays which-key popup sooner
			-- vim.opt.timeoutlen = 300

			-- Configure how new splits should be opened
			vim.opt.splitright = true
			vim.opt.splitbelow = true

			-- Preview substitutions live, as you type!
			vim.opt.inccommand = "split"

			-- Show which line your cursor is on
			vim.opt.cursorline = false

			-- Minimal number of screen lines to keep above and below the cursor.
			vim.opt.scrolloff = 10

			-- [[ Basic Keymaps ]]
			--  See `:help vim.keymap.set()`

			-- Set highlight on search, but clear on pressing <Esc> in normal mode
			-- This is SUPER USEFUL!!
			vim.opt.hlsearch = true
			vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

			-- Setting custom escape
			vim.keymap.set("i", "jj", "<Esc>")

			-- Highlight when yanking (copying) text
			--  Try it with `yap` in normal mode
			--  See `:help vim.highlight.on_yank()`
			vim.api.nvim_create_autocmd("TextYankPost", {
				desc = "Highlight when yanking (copying) text",
				group = vim.api.nvim_create_augroup("highlight-yank", { clear = true }),
				callback = function()
					vim.highlight.on_yank()
				end,
			})
		end,
	}
end

return ConfigConstructor
