vim.o.background = "dark"

require("gruvbox").setup({
    italic = false
})

vim.cmd("colorscheme gruvbox")

function ColorMyPencils(color)
    color = color or "gruvbox"
    vim.cmd.colorscheme(color)

    -- Transparency (as long as terminal emulator supports it)
    -- vim.api.nvim_set_hl(0, "Normal", { bg = "None" })
    -- vim.api.nvim_set_hl(0, "NormalFloat", { bg = "None" })
end

ColorMyPencils()
