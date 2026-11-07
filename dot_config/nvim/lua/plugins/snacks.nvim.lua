return {
	"folke/snacks.nvim",
	keys = {
		-- Snacks explorer keys (LazyVim only enables one explorer extra, and neo-tree's is on)
		{
			"<leader>fe",
			function()
				Snacks.explorer({ cwd = LazyVim.root() })
			end,
			desc = "Explorer Snacks (root dir)",
		},
		{
			"<leader>fE",
			function()
				Snacks.explorer()
			end,
			desc = "Explorer Snacks (cwd)",
		},
		{ "<leader>e", "<leader>fe", desc = "Explorer Snacks (root dir)", remap = true },
		{ "<leader>E", "<leader>fE", desc = "Explorer Snacks (cwd)", remap = true },
		{ "<leader>n", false },
		{
			"<leader>snn",
			function()
				Snacks.picker.notifications()
			end,
			desc = "Notification History (Snacks)",
		},
	},
	opts = function(_, opts)
		opts.explorer = opts.explorer or {}

		local keys = opts.dashboard.preset.keys
		-- Insert org entries before the Quit button
		table.insert(keys, #keys, {
			icon = " ",
			key = "o",
			desc = "Org Directory",
			action = function()
				vim.cmd.edit(vim.g.org_directory)
			end,
		})
		table.insert(keys, #keys, {
			icon = " ",
			key = "a",
			desc = "Org Agenda",
			action = function()
				require("orgmode").instance().agenda:open_by_key("o")
			end,
		})
	end,
}
