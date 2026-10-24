return {
  "stevearc/conform.nvim",
  opts = {
    formatters_by_ft = {
      lua = { "stylua" },
      python = { "black" },
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
    },
    formatters = {
      eslint = {
        command = "eslint",
        args = { "--fix", "$FILENAME" },
        stdin = false,
        exit_codes = { 0, 1 },
      },
    },
    format_on_save = {
      timeout_ms = 2000,
      lsp_format = "fallback",
    },
  },
}
