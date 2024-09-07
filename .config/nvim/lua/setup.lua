--  note: must happen before plugins are loaded (otherwise wrong leader will be used)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.opt.winblend = 0

-- set to true if you have a nerd font installed and selected in the terminal
vim.g.have_nerd_font = true

vim.opt.guicursor = ""

-- decrease update time
vim.opt.updatetime = 250

-- decrease mapped sequence wait time
-- displays which-key popup sooner
vim.opt.timeoutlen = 300

-- [[ setting options ]]
-- see `:help vim.opt`
-- make line numbers default
vim.opt.number = true
vim.opt.relativenumber = true

vim.opt.shiftwidth = 4
vim.opt.tabstop = 4

-- netrw, if you use it
-- vim.g.netrw_browse_split = 0
-- vim.g.netrw_banner = 0
-- vim.g.netrw_winsize = 25
-- vim.keymap.set("n", "<leader>se", "<cmd>Explore<cr>", { desc = "[s]how [e]xplorer" })

-- enable mouse mode
vim.opt.mouse = "a"

-- don't show the mode, since it's already in the status line
vim.opt.showmode = false

-- sync clipboard between os and neovim.
vim.opt.clipboard = "unnamedplus"

-- enable break indent
vim.opt.breakindent = true

-- save undo history
vim.opt.undofile = true

-- case-insensitive searching unless \c or one or more capital letters in the search term
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- keep signcolumn on by default
vim.opt.signcolumn = "yes"

-- decrease mapped sequence wait time
-- displays which-key popup sooner
-- vim.opt.timeoutlen = 300

-- configure how new splits should be opened
vim.opt.splitright = true
vim.opt.splitbelow = true

-- sets how neovim will display certain whitespace characters in the editor.
--  see `:help 'list'`
--  and `:help 'listchars'`
-- vim.opt.list = true
-- vim.opt.listchars = { tab = " ", trail = "·", nbsp = "␣" }

-- preview substitutions live, as you type!
vim.opt.inccommand = "split"

-- show which line your cursor is on
vim.opt.cursorline = false

-- minimal number of screen lines to keep above and below the cursor.
vim.opt.scrolloff = 10

-- [[ basic keymaps ]]
--  see `:help vim.keymap.set()`

-- set highlight on search, but clear on pressing <esc> in normal mode
-- this is super useful!!
vim.opt.hlsearch = true
vim.keymap.set("n", "<esc>", "<cmd>nohlsearch<cr>")

-- setting custom escape
vim.keymap.set("i", "jj", "<esc>")

-- diagnostics float window
vim.diagnostic.config({ float = { border = "rounded" } })

-- diagnostic keymaps
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "go to previous [d]iagnostic message" })
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "go to next [d]iagnostic message" })
vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, { desc = "show diagnostic [e]rror messages" })
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "open diagnostic [q]uickfix list" })

-- exit terminal mode in the builtin terminal with a shortcut that is a bit easier
-- for people to discover. otherwise, you normally need to press <c-\><c-n>, which
-- is not what someone will guess without a bit more experience.
--
-- note: this won't work in all terminal emulators/tmux/etc. try your own mapping
-- or just use <c-\><c-n> to exit terminal mode
vim.keymap.set("t", "<esc><esc>", "<c-\\><c-n>", { desc = "exit terminal mode" })

-- tip: disable arrow keys in normal mode
-- 'n' stays form noraml mode
vim.keymap.set("n", "<left>", '<cmd>echo "use h to move!!"<cr>')
vim.keymap.set("n", "<right>", '<cmd>echo "use l to move!!"<cr>')
vim.keymap.set("n", "<up>", '<cmd>echo "use k to move!!"<cr>')
vim.keymap.set("n", "<down>", '<cmd>echo "use j to move!!"<cr>')

-- keybinds to make split navigation easier.
--  use ctrl+<hjkl> to switch between windows
--
--  see `:help wincmd` for a list of all window commands
vim.keymap.set("n", "<c-h>", "<c-w><c-h>", { desc = "move focus to the left window" })
vim.keymap.set("n", "<c-l>", "<c-w><c-l>", { desc = "move focus to the right window" })
vim.keymap.set("n", "<c-j>", "<c-w><c-j>", { desc = "move focus to the lower window" })
vim.keymap.set("n", "<c-k>", "<c-w><c-k>", { desc = "move focus to the upper window" })

-- [[ basic autocommands ]]
--  see `:help lua-guide-autocommands`

-- highlight when yanking (copying) text
--  try it with `yap` in normal mode
--  see `:help vim.highlight.on_yank()`
vim.api.nvim_create_autocmd("textyankpost", {
	desc = "highlight when yanking (copying) text",
	group = vim.api.nvim_create_augroup("highlight-yank", { clear = true }),
	callback = function()
		vim.highlight.on_yank()
	end,
})
