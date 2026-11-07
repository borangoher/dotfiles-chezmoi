local function load_base16_file(path)
	local expanded = vim.fn.expand(path)
	if vim.fn.filereadable(expanded) == 0 then
		vim.notify("base16: theme file not found: " .. expanded, vim.log.levels.WARN)
		return {}
	end
	local palette = {}
	for line in io.lines(expanded) do
		local key, value = line:match('(base%x%x):%s*"(%x+)"')
		if key then
			palette[key] = "#" .. value
		end
	end
	return palette
end

return {
	"LazyVim/LazyVim",
	opts = {
		colorscheme = function()
			require("mini.base16").setup({
				palette = load_base16_file("~/.config/theme/eight-hundred.yaml"),
				use_cterm = true,
			})
			vim.g.colors_name = "eight-hundred"
		end,
	},
}
