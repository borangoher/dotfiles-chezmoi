return {
	"neovim/nvim-lspconfig",
	opts = {
		servers = {
			-- nvim-orgmode's built-in language server
			org = {},
			-- Installed via mise, not mason
			ruff = { mason = false },
			oxlint = { mason = false },
		},
	},
}
