vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
    -- packer plugin
    use 'wbthomason/packer.nvim'

    -- telescope plugin
    
    use {
        'nvim-telescope/telescope.nvim', tag = '0.1.3',
        requires = { { 'nvim-lua/plenary.nvim' } }
    }
    use 'nvim-telescope/telescope-file-browser.nvim'

    use({ 'rose-pine/neovim', as = 'rose-pine',
		config = function ()
 	        vim.cmd("colorscheme rose-pine")
		end
    })

--    use({ 'ellisonleao/gruvbox.nvim', as = 'gruvbox',
--        config = function ()
--   		    vim.cmd("colorscheme gruvbox")
--		end
--    })

    use {
        'nvim-treesitter/nvim-treesitter',
        run = function() require('nvim-treesitter.install').update({ with_sync = true }) end,
    }
    use('nvim-treesitter/nvim-treesitter-context');

    -- LSP
    --    use {
    --       'williamboman/mason.nvim',
    --        run = ":MasonUpdate"
    --    }    use('L3MON4D3/LuaSnip')
    --    use('williamboman/mason-lspconfig.nvim')
    --    use('onsails/lspkind-nvim')            -- vscode-like pictograms
    --    use('hrsh7th/cmp-buffer')              -- nvim-cmp source for buffer words
    --    use('hrsh7th/cmp-nvim-lsp')            -- nvim-cmp source for neovim's built-in LSP
    --    use('hrsh7th/nvim-cmp')                -- Completion
    --    use('neovim/nvim-lspconfig')           -- LSP
    --    use('jose-elias-alvarez/null-ls.nvim') -- Use Neovim as a language server to inject LSP diagnostics, code actions, and more via Lua

    --    use('glepnir/lspsaga.nvim')            -- LSP UIs

    use {
        'VonHeikemen/lsp-zero.nvim',
        branch = 'v1.x',
        requires = {
            -- LSP Support
            { 'neovim/nvim-lspconfig' },
            { 'williamboman/mason.nvim' },
            { 'williamboman/mason-lspconfig.nvim' },

            -- Autocompletion
            { 'hrsh7th/nvim-cmp' },
            { 'hrsh7th/cmp-buffer' },
            { 'hrsh7th/cmp-path' },
            { 'saadparwaiz1/cmp_luasnip' },
            { 'hrsh7th/cmp-nvim-lsp' },
            { 'hrsh7th/cmp-nvim-lua' },

            -- Snippets
            { 'L3MON4D3/LuaSnip' },
            { 'rafamadriz/friendly-snippets' },
        }
    }
    use('jose-elias-alvarez/null-ls.nvim') -- Use Neovim as a language server to inject LSP diagnostics, code actions, and more via Lua

    -- Git signs
    use('lewis6991/gitsigns.nvim')
end)

