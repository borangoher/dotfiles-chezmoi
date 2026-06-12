return {
  "y3owk1n/base16-pro-max.nvim",
  lazy = false,
  priority = 1000,
  config = function()
    local yaml_parser = require("base16-pro-max.parser")
    require("base16-pro-max").setup ({
      colors = yaml_parser.get_base16_colors("~/.config/theme/bg-palette.yaml"),
    })
    vim.cmd.colorscheme("base16-pro-max")
  end,
}
