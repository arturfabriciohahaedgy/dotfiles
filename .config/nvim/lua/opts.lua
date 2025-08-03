local o = vim.o
local opt = vim.opt
local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

o.expandtab = true
o.shiftwidth = 4
o.termguicolors = true
o.undofile = true
o.relativenumber = true
o.mouse = "a"
o.smarttab = true
o.cursorline = true
opt.clipboard = "unnamedplus"

-- After save formatting hook
augroup("__formatter__", {clear = true})
autocmd("BufWritePost", {group = "__formatter__", command = ":FormatWrite"})
