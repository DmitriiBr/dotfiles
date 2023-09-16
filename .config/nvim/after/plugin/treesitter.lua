local status, ts = pcall(require, "nvim-treesitter.configs")
if (not status) then return end

ts.setup {
    highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
        disable = {},
    },
    indent = {
        enable = true,
        disable = {},
    },
    ensure_installed = {
        "markdown",
  	    "javascript",
	    "typescript",
        "tsx",
        "json",
        "html",
        "css",
    	"cpp",
     	"c",
    	"lua",
    	"vim",
    	"vimdoc",
    	"query",
    },
    autotag = {
        enable = true,
    },
    sync_install = false,
    auto_install = true,
}

local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.tsx.filetype_to_parsername = { "javascript", "typescript.tsx" }
