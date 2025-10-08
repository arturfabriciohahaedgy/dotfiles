return {
        -- Which key 
        "folke/which-key.nvim",
        event = "VeryLazy",
        init = function()
            vim.o.timeout = true
            vim.otimeoutlen = 300
        end,
        opts = {}
}
