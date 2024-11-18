return {
	"neovim/nvim-lspconfig",
	lazy = false,
	dependencies = {
		{ "ms-jpq/coq_nvim", branch = "coq" },
		{ "ms-jpq/coq.artifacts", branch = "artifacts" },
		{ "ms-jpq/coq.thirdparty", branch = "3p" },
		{
			"SmiteshP/nvim-navbuddy",
			dependencies = {
				{ "SmiteshP/nvim-navic", opts = { lsp = { auto_attach = true } } },
				"MunifTanjim/nui.nvim",
			},
			opts = { lsp = { auto_attach = true } },
		},
	},
	init = function()
		vim.g.coq_settings = {
			auto_start = true,
			keymap = {
				bigger_preview = "<c-i>",
				jump_to_mark = "<c-u>",
			},
		}
	end,
	config = function()
		local lsp = require("lspconfig")
		local coq_lsp = require("coq").lsp_ensure_capabilities

		lsp.pyright.setup({ coq_lsp() })
		lsp.angularls.setup({ coq_lsp() })
		lsp.clangd.setup({ coq_lsp() })
		lsp.cssls.setup({ coq_lsp() })
		lsp.emmet_language_server.setup({ coq_lsp() })
		lsp.yamlls.setup({ coq_lsp() })
		lsp.html.setup({ coq_lsp() })
		lsp.nushell.setup({ coq_lsp() })
		lsp.lua_ls.setup({
			coq_lsp({
				on_init = function(client)
					if client.workspace_folders then
						local path = client.workspace_folders[1].name
						if vim.uv.fs_stat(path .. "/.luarc.json") or vim.uv.fs_stat(path .. "/.luarc.jsonc") then
							return
						end
					end

					client.config.settings.Lua = vim.tbl_deep_extend("force", client.config.settings.Lua, {
						runtime = {
							version = "LuaJIT",
						},
						workspace = {
							checkThirdParty = false,
							library = {
								vim.env.VIMRUNTIME,
							},
						},
					})
				end,
				settings = {
					Lua = {},
				},
			}),
		})
		lsp.nushell.setup({ coq_lsp() })
		lsp.sqlls.setup({ coq_lsp() })
		lsp.ts_ls.setup({ coq_lsp() })
	end,
}
