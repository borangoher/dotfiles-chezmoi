-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua

-- Custom remap
local function CRemap(mode, kbind, cmd, desc)
	vim.keymap.set(mode, kbind, cmd, {
		noremap = true,
		silent = true,
		desc = desc or "",
	})
end

-- Remove LazyVim window resize maps
for _, key in ipairs({ "<C-Up>", "<C-Down>", "<C-Left>", "<C-Right>" }) do
	pcall(vim.keymap.del, "n", key)
end

-- other.nvim
CRemap("n", "<leader>jj", "<cmd>Other<CR>", "Open Other")
CRemap("n", "<leader>js", "<cmd>OtherSplit<CR>", "Open Other in Split")
CRemap("n", "<leader>jv", "<cmd>OtherVSplit<CR>", "Open Other in Vertical Split")
CRemap("n", "<leader>jt", "<cmd>OtherTabNew<CR>", "Open Other in New Tab")
CRemap("n", "<leader>jT", "<cmd>Other test<CR>", "Open Test File")
CRemap("n", "<leader>jS", "<cmd>Other scss<CR>", "Open SCSS File")
CRemap("n", "<leader>jc", "<cmd>OtherClear<CR>", "Clear Other")

-- markdown (peek.nvim, glow.nvim)
CRemap("n", "<leader>mm", "<cmd>PeekOpen<CR>", "Peek open")
CRemap("n", "<leader>mc", "<cmd>PeekClose<CR>", "Peek close")
CRemap("n", "<leader>mg", "<cmd>Glow<CR>", "Glow preview")

-- sniprun
CRemap("n", "<leader>rr", "<cmd>SnipRun<CR>", "Run line")
CRemap("v", "<leader>rr", ":SnipRun<CR>", "Run selection")
CRemap("n", "<leader>ri", "<cmd>SnipInfo<CR>", "SnipRun info")
CRemap("n", "<leader>rR", "<cmd>SnipReset<CR>", "SnipRun reset")
CRemap("n", "<leader>rc", "<cmd>SnipClose<CR>", "SnipRun close")

-- goto-preview
CRemap("n", "<leader>cpd", "<cmd>lua require('goto-preview').goto_preview_definition()<CR>", "Open definition preview")
CRemap("n", "<leader>cpi", "<cmd>lua require('goto-preview').goto_preview_implementation()<CR>", "Open impl preview")
CRemap("n", "<leader>cpq", "<cmd>lua require('goto-preview').close_all_win()<CR>", "Close preview windows")

-- oil.nvim
CRemap("n", "-", "<cmd>Oil<CR>", "Open parent directory")

-- mini.trailspace
CRemap("n", "<leader>cw", function()
	require("mini.trailspace").trim()
end, "Trim trailing whitespace")

-- nvim-emmet
CRemap({ "n", "v" }, "<leader>xe", function()
	require("nvim-emmet").wrap_with_abbreviation()
end, "Emmet wrap")

-- harpoon
local harpoon = require("harpoon")

CRemap("n", "<leader>H", function()
	harpoon:list():add()
end, "Harpoon add")

CRemap("n", "<C-e>", function()
	harpoon.ui:toggle_quick_menu(harpoon:list())
end, "Harpoon quick menu")

CRemap("n", "<C-y>", function()
	harpoon:list():select(1)
end, "Harpoon select (1)")

CRemap("n", "<C-t>", function()
	harpoon:list():select(2)
end, "Harpoon select (2)")

CRemap("n", "<C-n>", function()
	harpoon:list():select(3)
end, "Harpoon select (3)")

CRemap("n", "<C-s>", function()
	harpoon:list():select(4)
end, "Harpoon select (4)")

CRemap("n", "<C-S-P>", function()
	harpoon:list():prev()
end, "Harpoon list previous")

CRemap("n", "<C-S-N>", function()
	harpoon:list():next()
end, "Harpoon list next")

-- orgmode
CRemap("n", "<leader>fO", function()
	vim.cmd.edit(vim.g.org_directory)
end, "Open org directory")
CRemap("n", "<leader>oSs", "<cmd>OrgSuperAgenda<cr>", "Org super agenda")
CRemap("n", "<leader>oSf", "<cmd>OrgSuperAgenda!<cr>", "Org super agenda (fullscreen)")
CRemap("n", "<leader>oSv", "<cmd>OrgSuperAgenda views<cr>", "Org super agenda views")
CRemap("n", "<leader>oh", function()
	require("telescope-orgmode").search_headings()
end, "Org search headlines")
CRemap("n", "<leader>og", function()
	require("telescope-orgmode").search_tags()
end, "Org search tags")
CRemap("n", "<leader>oR", function()
	require("telescope-orgmode").refile_heading()
end, "Org refile (picker)")
CRemap("n", "<leader>oL", function()
	require("telescope-orgmode").insert_link()
end, "Org insert link (picker)")

-- bufferline.nvim
CRemap("n", "<leader>bp", "<Cmd>BufferLineTogglePin<CR>", "Toggle Pin")
CRemap("n", "<leader>bP", "<Cmd>BufferLineGroupClose ungrouped<CR>", "Delete Non-Pinned Buffers")
CRemap("n", "<leader>br", "<Cmd>BufferLineCloseRight<CR>", "Delete Buffers to the Right")
CRemap("n", "<leader>bl", "<Cmd>BufferLineCloseLeft<CR>", "Delete Buffers to the Left")
CRemap("n", "<S-h>", "<cmd>BufferLineCyclePrev<cr>", "Prev Buffer")
CRemap("n", "<S-l>", "<cmd>BufferLineCycleNext<cr>", "Next Buffer")
CRemap("n", "[b", "<cmd>BufferLineCyclePrev<cr>", "Prev Buffer")
CRemap("n", "]b", "<cmd>BufferLineCycleNext<cr>", "Next Buffer")
CRemap("n", "[B", "<cmd>BufferLineMovePrev<cr>", "Move Buffer Prev")
CRemap("n", "]B", "<cmd>BufferLineMoveNext<cr>", "Move Buffer Next")
