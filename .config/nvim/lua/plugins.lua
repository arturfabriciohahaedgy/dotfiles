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

require("lazy").setup({
    {
        -- Which key 
        "folke/which-key.nvim",
        event = "VeryLazy",
        init = function()
            vim.o.timeout = true
            vim.otimeoutlen = 300
        end,
        opts = {}
    }, -- Colorschemes
    -- -- Dracula
    {"Mofiqul/dracula.nvim", lazy = false}, -- -- Catppuccin
    {"catppuccin/nvim", lazy = false, name = "catppuccin"}, -- -- Sonokai
    {"sainnhe/sonokai", lazy = false}, -- LuaLine
    {
        "nvim-lualine/lualine.nvim",
        dependencies = {"nvim-tree/nvim-web-devicons"}
    }, -- Treesitter
    {
        "nvim-treesitter/nvim-treesitter",
        config = function() vim.cmd([[TSUpdate]]) end
    }, -- Formatter
    {"mhartington/formatter.nvim", lazy = true}, -- Emmet
    -- -- LuaSnip
    {
        "L3MON4D3/LuaSnip",
        version = "v2.3", -- Replace <CurrentMajor> by the latest released major (first number of latest release)
        build = "make install_jsregexp",
        dependencies = {"rafamadriz/friendly-snippets"}
    }, -- LSP
    {"neovim/nvim-lspconfig", lazy = false}, {
        "williamboman/mason.nvim",
        dependencies = {"williamboman/mason-lspconfig.nvim"}
    }, -- Autocompletion
    {
        "hrsh7th/nvim-cmp",
        dependencies = {
            "hrsh7th/cmp-nvim-lsp", "hrsh7th/cmp-buffer", "hrsh7th/cmp-path",
            "hrsh7th/cmp-cmdline", "L3MON4D3/LuaSnip",
            "saadparwaiz1/cmp_luasnip"
        }
    }, {"VonHeikemen/lsp-zero.nvim"}, -- Telescope
    {
        'nvim-telescope/telescope.nvim',
        tag = '0.1.8',
        dependencies = {'nvim-lua/plenary.nvim'}
    }, {
        'mrcjkb/haskell-tools.nvim',
        version = '^4' -- Recommended 
        -- ft = { 'haskell', 'lhaskell', 'cabal', 'cabalproject' },
    }, -- Go
    {
        "ray-x/go.nvim",
        dependencies = { -- optional packages
            "ray-x/guihua.lua"
        },
        opts = {
            -- lsp_keymaps = false,
            -- other options
        },
        config = function(lp, opts)
            require("go").setup(opts)
            local format_sync_grp = vim.api.nvim_create_augroup("GoFormat", {})
            vim.api.nvim_create_autocmd("BufWritePre", {
                pattern = "*.go",
                callback = function()
                    require('go.format').goimports()
                end,
                group = format_sync_grp
            })
        end,
        event = {"CmdlineEnter"},
        ft = {"go", 'gomod'},
        build = ':lua require("go.install").update_all_sync()' -- if you need to install/update all binaries
    }
})

-- LuaLine
require("lualine").setup({options = {theme = "dracula"}})

-- Treesitter
require("nvim-treesitter.configs").setup({
    ensure_installed = {
        "c", "lua", "rust", "go", "haskell", "python", "html", "css", "typescript",
        "javascript", "zig"
    },
    highlight = {enable = true}
})

-- Telescope
tb = require('telescope.builtin')

-- LuaSnip
require("luasnip.loaders.from_vscode").lazy_load()

-- LSP
-- -- Automatic server installation
require("mason").setup({})

-- Colorschemes

-- vim.cmd.colorscheme "catppuccin-mocha"
vim.cmd.colorscheme "dracula"
-- vim.cmd.colorscheme "sonokai"

-- Formatter
local futil = require("formatter.util")

require("formatter").setup {
    filetype = {
        lua = {require("formatter.filetypes.lua").luaformat},
        typescript = {require("formatter.filetypes.typescript").prettier},
        html = {require("formatter.filetypes.html").prettier},
        css = {require("formatter.filetypes.css").prettier},
        haskell = {require("formatter.filetypes.haskell").stylish_haskell},
        zig = {require("formatter.filetypes.zig").zigfmt}
    }
}

-- -- Autocompletion
local cmp = require 'cmp'

cmp.setup({
    snippet = {
        expand = function(args) require('luasnip').lsp_expand(args.body) end
    },
    window = {documentation = cmp.config.window.bordered()},
    mapping = cmp.mapping.preset.insert({
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.abort(),
        ['<CR>'] = cmp.mapping.confirm({select = true})
    }),
    sources = cmp.config.sources({{name = 'nvim_lsp'}, {name = 'luasnip'}},
                                 {{name = 'buffer'}})
})

cmp.setup.cmdline({'/', '?'}, {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {{name = 'buffer'}}
})

cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({{name = 'path'}}, {{name = 'cmdline'}})
})

-- -- Servers setup
require("lsp-zero")
local lspconfig = require('lspconfig')
local capabilities = require('cmp_nvim_lsp').default_capabilities()
local servers = {'html', 'css_variables', 'ts_ls', 'angularls', "zls", "gopls"}
for _, lsp in ipairs(servers) do
    lspconfig[lsp].setup {capabilities = capabilities}
end
