local function create_default_config()
	vim.g.mapleader = " "
	vim.g.maplocalleader = " "

	vim.opt.winblend = 0

	-- Set to true if you have a Nerd Font installed and selected in the terminal
	vim.g.have_nerd_font = true

	-- vim.opt.guicursor = ""

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

	-- Sets how neovim will display certain whitespace characters in the editor.
	--  See `:help 'list'`
	--  and `:help 'listchars'`
	-- vim.opt.list = true
	-- vim.opt.listchars = { tab = " ", trail = "·", nbsp = "␣" }

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

	-- TIP: Disable arrow keys in normal mode
	-- 'n' stays form Noraml Mode
	vim.keymap.set("n", "<left>", '<cmd>echo "Use h to move!!"<CR>')
	vim.keymap.set("n", "<right>", '<cmd>echo "Use l to move!!"<CR>')
	vim.keymap.set("n", "<up>", '<cmd>echo "Use k to move!!"<CR>')
	vim.keymap.set("n", "<down>", '<cmd>echo "Use j to move!!"<CR>')

	-- [[ Basic Autocommands ]]
	--  See `:help lua-guide-autocommands`

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
end

if vim.g.vscode then
	create_default_config()

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
else
	create_default_config()

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
