local function load_base16_file(path)
  local expanded = vim.fn.expand(path)
  if vim.fn.filereadable(expanded) == 0 then
    vim.notify("base16: theme file not found: " .. expanded, vim.log.levels.WARN)
    return {}
  end
  local palette = {}
  for line in io.lines(expanded) do
    local key, value = line:match('(base%x%x):%s*"(%x+)"')
    if key then palette[key] = "#" .. value end
  end
  return palette
end

return {
	"nvim-mini/mini.nvim",
	version = false,
  lazy = false,
  priority = 1000,
	config = function()
    require("mini.base16").setup({
        palette = load_base16_file("~/.config/theme/eight-hundred.yaml"),
        use_cterm = true,
    })
    vim.g.colors_name = "eight-hundred"

		require("mini.ai").setup()
		require("mini.align").setup()
		require("mini.animate").setup()
		require("mini.basics").setup()
		require("mini.comment").setup()
		require("mini.cursorword").setup()
		require("mini.extra").setup()
		require("mini.indentscope").setup()
		require("mini.move").setup()
		require("mini.sessions").setup()
		require("mini.splitjoin").setup()
		local starter = require("mini.starter")
		starter.setup({
			items = {
				starter.sections.sessions(5, true),
				starter.sections.recent_files(5, false, false),
				{
					name = "Open org directory",
					action = function()
						vim.cmd.edit(vim.g.org_directory)
					end,
					section = "Org",
				},
				{
					name = "Open org agenda (Overview)",
					action = function()
						require("orgmode").instance().agenda:open_by_key("o")
					end,
					section = "Org",
				},
				starter.sections.builtin_actions(),
			},
		})
		require("mini.surround").setup()
		require("mini.trailspace").setup()
	end,
}
