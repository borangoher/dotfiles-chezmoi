-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua

-- LazyVim choices
vim.g.lazyvim_picker = "snacks"
vim.g.lazyvim_cmp = "blink.cmp"
vim.g.lazyvim_python_lsp = "pyright"
vim.g.lazyvim_python_ruff = "ruff"

-- Org directory (shared by orgmode, snacks dashboard and keymaps)
vim.g.org_directory = vim.fn.expand("~/Library/CloudStorage/ProtonDrive-borangoher@proton.me-folder/org/")

-- No swap file
vim.opt.swapfile = false

-- No backup files
vim.opt.backup = false
vim.opt.writebackup = false

-- Use 2 spaces instead of tab
vim.opt.softtabstop = 2

-- Number of undo saved
vim.opt.undoreload = 10000

-- More space for displaying messages
vim.opt.cmdheight = 2

-- Stable update time
vim.opt.updatetime = 300

-- Show the signcolumn in the number column
vim.opt.signcolumn = "number"

-- disable perl provider
vim.g.loaded_perl_provider = 0
