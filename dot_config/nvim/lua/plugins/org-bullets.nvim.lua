return {
	"nvim-orgmode/org-bullets.nvim",
	ft = { "org" },
	dependencies = { "nvim-orgmode/orgmode" },
	opts = {
		symbols = {
			headlines = { "◉", "○", "✸", "✿" },
			list = "•",
			checkboxes = {
				done = { "✓", "@org.keyword.done" },
				todo = { "˟", "@org.keyword.todo" },
			},
		},
	},
}
