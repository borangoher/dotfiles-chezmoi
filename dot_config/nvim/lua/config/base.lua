-- Use system clipboard
vim.opt.clipboard = "unnamedplus"

-- No swap file
vim.opt.swapfile = false

-- Save undo history in files
vim.opt.undofile = true

-- Number of undo saved
vim.opt.undolevels = 10000
vim.opt.undoreload = 10000

-- Relative line numbers
vim.opt.relativenumber = true

-- Use 2 spaces instead of tab
vim.opt.autoindent = true
vim.opt.expandtab = true
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2

-- Show substitution incrementally
vim.opt.inccommand = "nosplit"

-- Enable hidden buffers
vim.opt.hidden = true

-- No backup files
vim.opt.backup = false
vim.opt.writebackup = false

-- More space for displaying messages
vim.opt.cmdheight = 2

-- Stable update time
vim.opt.updatetime = 300

-- Don't pass messages to ins-completion-menu
vim.opt.shortmess:append("c")

-- Always show the signcolumn
vim.opt.signcolumn = "number"

-- Enable gui colors
vim.opt.termguicolors = true

-- disable perl provider
vim.g.loaded_perl_provider = 0

-- Space as leader key (must be loaded before lazy)
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
vim.keymap.set("n", "<Space>", "<Nop>", { noremap = true, silent = true })
