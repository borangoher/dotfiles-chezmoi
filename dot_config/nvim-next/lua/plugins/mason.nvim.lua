-- Mason only installs Neovim-only tools; formatters and linters come from mise/brew
return {
	"mason-org/mason.nvim",
	opts = function(_, opts)
		opts.ensure_installed = {
			-- language servers
			"lua-language-server",
			"vtsls",
			"json-lsp",
			"yaml-language-server",
			"pyright",
			"clangd",
			"tailwindcss-language-server",
			"taplo",
			"gopls",
			"dockerfile-language-server",
			"docker-compose-language-service",
			"nil",
			-- debug adapters
			"js-debug-adapter",
			"debugpy",
			"codelldb",
			"delve",
			-- go editing helpers
			"gomodifytags",
			"impl",
			"goimports",
		}
	end,
}
