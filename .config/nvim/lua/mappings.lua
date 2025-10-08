local lsp = vim.lsp
local setk = vim.keymap.set

-- kj = pressing ESC
setk('i', 'kj', '<Esc>')

-- Clipboard
-- Yank to system's clipboard
setk('v', '<leader>y', '"+y', {desc = "Yank region to system's clipboard"})
setk('n', '<leader>y', '"+y', {desc = "Yank text to system's clipboard"})
setk('n', '<leader>Y', '"+yg_',
     {desc = "Yank rest of line to system's clipboard"})
setk('n', '<leader>yy', '"+yy', {desc = "Yank line to keyboard"})
-- Paste from system's clipboard
setk('v', '<leader>p', '"+p', {desc = "Paste system's clipboard over region"})
setk('n', '<leader>p', '"+p', {desc = "Paste system's clipboard before region"})

-- LSP stuff
-- setk('n', '<space>e', vim.diagnostic.open_float,
--      {desc = "Opens diagonsis in float window"})
setk('n', '[d', vim.diagnostic.goto_prev,
     {desc = "Go to the previous diagnosis"})
setk('n', ']d', vim.diagnostic.goto_next, {desc = "Go to the next diagnosis"})
setk('n', '<space>q', vim.diagnostic.setloclist,
     {desc = "Open list with all diagnosis"})
setk('n', 'gD', lsp.buf.declaration, {desc = "Go to the declaration"})
setk('n', 'gi', lsp.buf.implementation, {desc = "Go to the implementation"})
-- setk('n', 'gd', lsp.buf.definition, {desc = "Go to the definition"})
setk('n', 'K', lsp.buf.hover, {desc = "Show info in floating window"})
-- setk('n', '<space>D', lsp.buf.type_definition, {desc = "Go to type definition"})
-- setk('n', '<space>rn', lsp.buf.rename, {desc = "Rename element"})
-- setk('n', 'gr', lsp.buf.references, {desc = "Go to references"})

-- Telescope
local tb = require('telescope.builtin')

setk('n', '<leader>f', '<leader>f', {desc = "Telescope's find function"})
setk('n', '<leader>fr', tb.oldfiles, {desc = "Search for recent files"})
setk('n', '<leader>ff', tb.find_files, {desc = "Uses find in files"})
setk('n', '<leader>fg', tb.live_grep, {desc = "Uses grep in files"})
setk('n', '<leader>fb', tb.buffers, {desc = "Search and select buffer"})
setk('n', '<leader>fh', tb.help_tags, {desc = "Help"})
setk('n', '<leader>en', function() 
    tb.find_files {
        cwd = vim.fn.stdpath("config")
    }
end, {desc = "Help"})

-- oil
setk('n', '-', '<cmd>Oil<CR>', {desc = "Open Oil in current directory"})

-- harpoon
-- local harpoon = require('harpoon')
-- harpoon:setup({})

-- basic telescope configuration
-- local conf = require("telescope.config").values
-- local function toggle_telescope(harpoon_files)
--     local file_paths = {}
--     for _, item in ipairs(harpoon_files.items) do
--         table.insert(file_paths, item.value)
--     end
--
--     require("telescope.pickers").new({}, {
--         prompt_title = "Harpoon",
--         finder = require("telescope.finders").new_table({
--             results = file_paths,
--         }),
--         previewer = conf.file_previewer({}),
--         sorter = conf.generic_sorter({}),
--     }):find()
-- end
--
-- setk("n", "<leader>a", function() harpoon:list():add() end)
-- setk("n", "<C-e>", function() toggle_telescope(harpoon:list()) end,
--     { desc = "Open harpoon window" })
-- setk("n", "<leader>n", function() harpoon:list():next() end, {desc = "Next buffer stored"})
-- setk("n", "<leader>b", function() harpoon:list():next() end, {desc = "Previous buffer stored"})

