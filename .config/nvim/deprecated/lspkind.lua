local status, lspkind = pcall(require, "lspkind")
if (not status) then return end

lspkind.init({
    -- enables text annotations

    mode = 'symbol_text',

    -- default symbol map
    -- can be either 'default' (requires nerd-fonts font) or
    -- 'codicons' for codicon preset (requires vscode-codicons font)
    --
    -- default: 'default'
    preset = 'codicons',

    -- override preset symbols
    -- default: {}

    symbol_map = {
        Text = "T - Text",
        Method = "M - Method",
        Function = "F - Function",
        Constructor = "C - Constructor",
        Field = "Fi - Field",
        Variable = "V - Variable",
        Class = "Cl - Class",
        Interface = "I - Interface",
        Module = "M - Module",
        Property = "P - Property",
        Unit = "U - Unit",
        Value = "Val - Value",
        Enum = "E - Enum",
        Keyword = "K - Keyword",
        Snippet = "S - Snippet",
        Color = "Col - Color",
        File = "F - File",
        Reference = "R - Reference",
        Folder = "Fol - Folder",
        EnumMember = "E - EnumMember",
        Constant = "C - Contant",
        Struct = "Str - Struct",
        Event = "Ev - Event",
        Operator = "Op - Operator",
        TypeParameter = "TPar - TypeParameter",
    },
})
