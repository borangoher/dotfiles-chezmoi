return {
	"nvim-neo-tree/neo-tree.nvim",
	keys = {
		-- Explorer keys belong to the snacks explorer
		{ "<leader>e", false },
		{ "<leader>E", false },
		{ "<leader>fe", false },
		{ "<leader>fE", false },
		{ "<leader>n", "<cmd>Neotree<CR>", desc = "Open neotree" },
	},
	opts = {
		close_if_last_window = true,
		window = {
			width = 30,
		},
		buffers = {
			follow_current_file = { enabled = true },
		},
		filesystem = {
			follow_current_file = { enabled = true },
			filtered_items = {
				hide_dotfiles = false,
				hide_gitignored = false,
				hide_by_name = {
					"node_modules",
				},
				never_show = {
					".DS_Store",
					"thumbs.db",
				},
			},
		},
	},
}
