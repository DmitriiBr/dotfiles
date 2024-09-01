local function KeybindsConstructor()
    local vscode = require("vscode")

    local self = {
        map_vscode_call = function(mode, keybind, vscode_action_id, desc)
            local function get_error(message) return "NOT DEFINED: " .. message end

            if mode == nil then error(get_error("Argument [mode] is not defined")) end
            if keybind == nil then error(get_error("Argument [keybind] is not defined")) end
            if vscode_action_id == nil then error(get_error("Argument [vscode_action_id] is not defined")) end
            if desc == nil then error(get_error("Argument [desc] is not defined")) end

            vim.keymap.set(mode, keybind, function() vscode.call(vscode_action_id) end, { desc = desc })
        end,
    }

    return {
        disable_arrows = function()
            vim.keymap.set("n", "<left>", '<cmd>echo "Use h to move!!"<CR>')
            vim.keymap.set("n", "<right>", '<cmd>echo "Use l to move!!"<CR>')
            vim.keymap.set("n", "<up>", '<cmd>echo "Use k to move!!"<CR>')
            vim.keymap.set("n", "<down>", '<cmd>echo "Use j to move!!"<CR>')
        end,
        navigation = function()
            self.map_vscode_call("n", "<leader>f", "workbench.action.quickOpen", "Go to [F]ile")
            self.map_vscode_call("n", "<leader>t", "workbench.action.findInFiles", "Search in files")
        end,
        splitting = function()
            self.map_vscode_call("n", "<leader>sr", "workbench.action.splitEditorRight", "[S]plit [R]ight")
            self.map_vscode_call("n", "<leader>sl", "workbench.action.splitEditorLeft", "[S]plit [L]eft")

            self.map_vscode_call("n", "<C-k>", "workbench.action.focusAboveGroup", "Move focus to the upper window")
            self.map_vscode_call("n", "<C-l>", "workbench.action.focusRightGroup", "Move focus to the right window")
            self.map_vscode_call("n", "<C-j>", "workbench.action.focusBelowGroup", "Move focus to the lower window")
            self.map_vscode_call("n", "<C-h>", "workbench.action.focusLeftGroup", "Move focus to the left window")
        end,
        diagnostics = function()
            self.map_vscode_call("n", "[d", "editor.action.marker.prev", "Go to previous [D]iagnostic message")
            self.map_vscode_call("n", "]d", "editor.action.marker.next", "Go to next [D]iagnostic message")
            self.map_vscode_call("n", "<leader>e", "workbench.action.showErrorsWarnings",
                "Show diagnostic [E]rror messages")
            -- vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostic [Q]uickfix list" })
        end,
        file_management = function()
            self.map_vscode_call("n", "<leader>se", "workbench.view.explorer", "[S]how [E]xplorer")
            self.map_vscode_call("n", "<leader>sb", "workbench.action.toggleSidebarVisibility", "Toggle sidebar")
        end
    }
end

return KeybindsConstructor
