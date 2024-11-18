return {
	"echasnovski/mini.nvim",
	version = false,
	config = function()
		require("mini.ai").setup()
		require("mini.align").setup()
		require("mini.animate").setup()
		require("mini.base16").setup({
			palette = {
				base00 = "#24283b",
				base01 = "#1f2335",
				base02 = "#292e42",
				base03 = "#565f89",
				base04 = "#a9b1d6",
				base05 = "#c0caf5",
				base06 = "#c0caf5",
				base07 = "#c0caf5",
				base08 = "#f7768e",
				base09 = "#ff9e64",
				base0A = "#e0af68",
				base0B = "#9ece6a",
				base0C = "#1abc9c",
				base0D = "#41a6b5",
				base0E = "#bb9af7",
				base0F = "#ff007c",
			},
		})
		require("mini.basics").setup()
		require("mini.comment").setup()
		require("mini.cursorword").setup()
		require("mini.extra").setup()
		require("mini.indentscope").setup()
		require("mini.move").setup()
		require("mini.sessions").setup()
		require("mini.splitjoin").setup()
		require("mini.starter").setup()
		require("mini.surround").setup()
		require("mini.trailspace").setup()
	end,
}
