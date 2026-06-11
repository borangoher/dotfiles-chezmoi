return {
  "stevearc/conform.nvim",
  opts = {
    formatters_by_ft = {
      lua = { "stylua" },
      python = { "black" },
      javascript = { "prettierd", "rustywind", "eslint_d" },
      typescript = { "prettierd", "rustywind", "eslint_d" },
      javascriptreact = { "prettierd", "rustywind", "eslint_d" },
      typescriptreact = { "prettierd", "rustywind", "eslint_d" },
      html = { "prettierd" },
      cpp = { "clang-format" },
      css = { "stylelint" },
      markdown = { "mdformat" },
      yaml = { "prettierd" },
      json = { "prettierd" },
    },
    format_on_save = {
      timeout_ms = 2000,
      lsp_format = "fallback",
    },
  },
}
