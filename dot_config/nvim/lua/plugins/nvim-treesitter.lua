return {
	"nvim-treesitter/nvim-treesitter",
	lazy = false,
	build = ":TSUpdate",
	dependencies = {
		{ "nushell/tree-sitter-nu", build = ":TSUpdate nu" },
	},
	config = function()
		local configs = require("nvim-treesitter.configs")

		configs.setup({
			ensure_installed = {
				"angular",
				"c",
				"cpp",
				"css",
				"html",
				"javascript",
				"json",
				"typescript",
				"lua",
				"vim",
				"vimdoc",
				"query",
				"markdown",
				"markdown_inline",
				"bash",
				"regex",
			},
			sync_install = false,
			auto_install = true,
			ignore_install = {},
			modules = {},
			highlight = {
				enable = true,
				additional_vim_regex_highlighting = false,
			},
		})
	end,
}
