-- require('gruvbox').setup({
    --transparent_mode = true,
    --italic = {
        --strings = false,
        --comments = true,
       --operators = false,
        --folds = false,
    --},
    --invert_selection = false,
-- })

require('rose-pine').setup({})

vim.o.background = "dark"

function ColorMe(color)
    color = color or "rose-pine"
    vim.cmd.colorscheme(color)
end

ColorMe()
