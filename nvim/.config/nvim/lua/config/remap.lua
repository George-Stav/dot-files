vim.g.mapleader = " "

vim.keymap.set("n", "<leader>o-", vim.cmd.Ex)

-- move highlighted text up or down while in Visual mode
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")

-- keep cursor in same place when appending lines with J
vim.keymap.set("n", "J", "mzJ`z")

-- keep cursor in the middle when going up or down
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

-- keep cursor in the middle when moving through search terms
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

-- paste whatever is in paste buffer without replacing it
vim.keymap.set("x", "gp", "\"_dP")

-- yank into system clipboard
vim.keymap.set("n", "gy", "\"+y")
vim.keymap.set("v", "gy", "\"+y")
vim.keymap.set("n", "gY", "\"+Y")

-- delete without placing deleted text into paste buffer (i.e. delete to void register
vim.keymap.set("n", "gd", "\"_d")
vim.keymap.set("v", "gd", "\"_d")

-- vim.keymap.set("n", "Q", "<nop>")
vim.keymap.set("n", "G", "Gzz")

vim.keymap.set("n", "<C-k>", "<cmd>cnext<CR>zz")
vim.keymap.set("n", "<C-j>", "<cmd>cprev<CR>zz")
vim.keymap.set("n", "<leader>k", "<cmd>lnext<CR>zz")
vim.keymap.set("n", "<leader>j", "<cmd>lprev<CR>zz")

vim.keymap.set("n", "<leader>wv", "<cmd>vnew<CR>")
vim.keymap.set("n", "<leader>ws", "<cmd>split<CR>")
vim.keymap.set("n", "<leader>wl", "<C-w>l")
vim.keymap.set("n", "<leader>wh", "<C-w>h")
vim.keymap.set("n", "<leader>wj", "<C-w>j")
vim.keymap.set("n", "<leader>wk", "<C-w>k")
vim.keymap.set("n", "<leader>wc", "<cmd>q<CR>")

vim.keymap.set("n", "<leader>fs", "<cmd>w<CR>")

