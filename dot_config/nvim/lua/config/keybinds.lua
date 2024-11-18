-- Custom remap
local function CRemap(mode, kbind, cmd, desc)
	vim.keymap.set(mode, kbind, cmd, {
		noremap = true,
		silent = true,
		desc = desc or "",
	})
end

-- window nav
CRemap("n", "<C-left>", "<C-w>h", "Move left")
CRemap("n", "<C-down>", "<C-w>j", "Move down")
CRemap("n", "<C-up>", "<C-w>k", "Move up")
CRemap("n", "<C-right>", "<C-w>l", "Move right")

-- CHADtree
CRemap("n", "<leader>v", "<cmd>CHADopen<CR>", "Open CHADtree")
CRemap("n", "<leader>xx", "<cmd>Trouble diagnostics toggle<cr>", "Diagnostics (Trouble)")
CRemap("n", "<leader>xX", "<cmd>Trouble diagnostics toggle filter.buf=0<CR>", "Buffer Diagnostics (Trouble)")
CRemap("n", "<leader>cs", "<cmd>Trouble symbols toggle focus=false<CR>", "Symbols (Trouble)")
CRemap(
	"n",
	"<leader>cl",
	"<cmd>Trouble lsp toggle focus=false win.position=right<CR>",
	"LSP Definitions / references / ... (Trouble)"
)
CRemap("n", "<leader>xL", "<cmd>Trouble loclist toggle<CR>", "Location List (Trouble)")
CRemap("n", "<leader>xQ", "<cmd>Trouble qflist toggle<CR>", "Quickfix List (Trouble)")

-- other.nvim
CRemap("n", "<leader>ll", "<cmd>:Other<CR>", "Open Other")
CRemap("n", "<leader>ltn", "<cmd>:OtherTabNew<CR>", "Open Other in New Tab")
CRemap("n", "<leader>lp", "<cmd>:OtherSplit<CR>", "Open Other in Split")
CRemap("n", "<leader>lv", "<cmd>:OtherVSplit<CR>", "Open Other in Vertical Split")
CRemap("n", "<leader>lc", "<cmd>:OtherClear<CR>", "Clear Other")
CRemap("n", "<leader>lt", "<cmd>:Other test<CR>", "Open Test File")
CRemap("n", "<leader>ls", "<cmd>:Other scss<CR>", "Open SCSS File")

-- nvim-emmet
CRemap({ "n", "v" }, "<leader>xe", require("nvim-emmet").wrap_with_abbreviation, "Emmet wrap")

-- lazygit.nvim
CRemap("n", "<leader>lg", "<cmd>LazyGit<cr>", "Open LazyGit")

-- harpoon
local harpoon = require("harpoon")

local conf = require("telescope.config").values
local function toggle_telescope(harpoon_files)
	local file_paths = {}
	for _, item in ipairs(harpoon_files.items) do
		table.insert(file_paths, item.value)
	end

	require("telescope.pickers")
		.new({}, {
			prompt_title = "Harpoon",
			finder = require("telescope.finders").new_table({
				results = file_paths,
			}),
			previewer = conf.file_previewer({}),
			sorter = conf.generic_sorter({}),
		})
		:find()
end

CRemap("n", "<C-e>", function()
	toggle_telescope(harpoon:list())
end, "Open harpoon window")

CRemap("n", "<leader>a", function()
	harpoon:list():add()
end)

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

-- telescope
local builtin = require("telescope.builtin")
CRemap("n", "<leader>ff", builtin.find_files, "Telescope find files")
CRemap("n", "<leader>fg", builtin.live_grep, "Telescope live grep")
CRemap("n", "<leader>fb", builtin.buffers, "Telescope buffers")
CRemap("n", "<leader>fh", builtin.help_tags, "Telescope help tags")

-- leap.nvim
CRemap({ "n", "x", "o" }, "<Leader>s", "<Plug>(leap-forward)", "Leaf forward")
CRemap({ "n", "x", "o" }, "<Leader>S", "<Plug>(leap-backward)", "Leap backward")
CRemap({ "n", "x", "o" }, "<Leader>gs", "<Plug>(leap-from-window)", "Leap from window")
