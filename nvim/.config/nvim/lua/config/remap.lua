local nnoremap = require("config.keymap").nnoremap

vim.g.mapleader = " "
vim.keymap.set("n", "<leader>o-", vim.cmd.Ex)
vim.keymap.set("n", "<leader>fs", vim.cmd.write)
vim.keymap.set("n", "<leader>k", vim.cmd.quit)
vim.keymap.set("n", "<leader>wv", vim.cmd.vnew)
vim.keymap.set("n", "<leader>ws", vim.cmd.split)


nnoremap("<leader>wl", "<C-w>l")
nnoremap("<leader>wh", "<C-w>h")
nnoremap("<leader>wj", "<C-w>j")
nnoremap("<leader>wk", "<C-w>k")

