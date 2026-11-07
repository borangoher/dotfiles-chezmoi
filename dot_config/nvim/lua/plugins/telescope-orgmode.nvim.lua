return {
  "nvim-orgmode/telescope-orgmode.nvim",
  event = "VeryLazy",
  dependencies = {
    "nvim-orgmode/orgmode",
    "folke/snacks.nvim",
  },
  config = function()
    require("telescope-orgmode").setup({ adapter = "snacks" })
  end,
}
