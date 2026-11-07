return {
	"mfussenegger/nvim-lint",
	opts = {
		linters_by_ft = {
			markdown = { "markdownlint" },
			javascript = { "eslint" },
			typescript = { "eslint" },
			javascriptreact = { "eslint" },
			typescriptreact = { "eslint" },
			json = { "eslint" },
			html = { "tidy" },
			css = { "stylelint" },
			cpp = { "cppcheck" },
			yaml = { "yamllint" },
			lua = { "luac" },
		},
	},
}
