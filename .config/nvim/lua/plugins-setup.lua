-- Load plugin
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git", "clone", "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git", "--branch=stable", -- latest stable release
        lazypath
    })
end
vim.opt.rtp:prepend(lazypath)
require("lazy").setup("plugins")

require("formatter").setup({
    filetype = {
        lua = {require("formatter.filetypes.lua").luaformat},
        typescript = {require("formatter.filetypes.typescript").prettier},
        html = {require("formatter.filetypes.html").prettier},
        css = {require("formatter.filetypes.css").prettier},
        haskell = {require("formatter.filetypes.haskell").stylish_haskell},
        zig = {require("formatter.filetypes.zig").zigfmt},
        go = {require("formatter.filetypes.go").gofmt},
        ocaml = {require("formatter.filetypes.ocaml").ocamlformat}
    }
})

-- LuaLine

-- LuaSnip
-- require("luasnip.loaders.from_vscode").lazy_load()

-- LSP
-- -- Automatic server installation

-- Colorschemes

-- vim.cmd.colorscheme "catppuccin-mocha"
-- vim.cmd.colorscheme "dracula"
-- vim.cmd.colorscheme "sonokai"

-- Formatter


