return {
	"folke/snacks.nvim",
	keys = {
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
