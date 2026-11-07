return {
	"rgroli/other.nvim",
	config = function()
		require("other-nvim").setup({
			mappings = {
				"angular",
				"rails",
				"golang",
				"python",
				"rust",
			},
			style = {
				border = "solid",
				seperator = "|",
				width = 0.7,
				minHeight = 2,
			},
		})
	end,
}
