return {
        "nvim-treesitter/nvim-treesitter",
        config = function() 
            vim.cmd([[TSUpdate]]) 
            require("nvim-treesitter.configs").setup({
                ensure_installed = {
                    "c", "lua", "rust", "go", "haskell", "python", "html", "css", "typescript",
                    "javascript", "zig"
                },
                highlight = {enable = true}
            })
        end
}
