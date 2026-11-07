return {
	"stevearc/conform.nvim",
	opts = function(_, opts)
		opts.formatters_by_ft = opts.formatters_by_ft or {}

		-- Remove oxfmt added by the typescript.oxc extra (prettier handles these)
		for ft, formatters in pairs(opts.formatters_by_ft) do
			if type(formatters) == "table" then
				opts.formatters_by_ft[ft] = vim.tbl_filter(function(f)
					return f ~= "oxfmt"
				end, formatters)
			end
		end

		local formatters_by_ft = {
			lua = { "stylua" },
			python = { "ruff_organize_imports", "ruff_format" },
			javascript = { "prettier", "rustywind", "eslint" },
			typescript = { "prettier", "rustywind", "eslint" },
			javascriptreact = { "prettier", "rustywind", "eslint" },
			typescriptreact = { "prettier", "rustywind", "eslint" },
			html = { "prettier" },
			cpp = { "clang-format" },
			css = { "stylelint" },
			markdown = { "prettier" },
			yaml = { "prettier" },
			json = { "prettier" },
		}
		for ft, formatters in pairs(formatters_by_ft) do
			opts.formatters_by_ft[ft] = formatters
		end

		opts.formatters = vim.tbl_deep_extend("force", opts.formatters or {}, {
			eslint = {
				command = "eslint",
				args = { "--fix", "$FILENAME" },
				stdin = false,
				exit_codes = { 0, 1 },
			},
		})

		-- Format on save is handled by LazyVim; only the defaults are set here
		opts.default_format_opts = vim.tbl_deep_extend("force", opts.default_format_opts or {}, {
			timeout_ms = 2000,
			lsp_format = "fallback",
		})
	end,
}
