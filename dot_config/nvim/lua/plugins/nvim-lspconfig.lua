return {
  "neovim/nvim-lspconfig",
  lazy = false,
  dependencies = {
    { "ms-jpq/coq_nvim",       branch = "coq" },
    { "ms-jpq/coq.artifacts",  branch = "artifacts" },
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
      keymap = {
        bigger_preview = "<c-i>",
      },
    }

    vim.keymap.set({ "i", "s" }, "<c-u>", function()
      vim.snippet.jump(1)
    end)
  end,
  config = function()
    local coq = require("coq")

    local servers = {
      "pyright",
      "angularls",
      "clangd",
      "cssls",
      "emmet_language_server",
      "yamlls",
      "html",
      "nushell",
      "sqlls",
      "ts_ls",
    }

    for _, server in ipairs(servers) do
      vim.lsp.config(server, coq.lsp_ensure_capabilities({}))
    end

    vim.lsp.config("lua_ls", coq.lsp_ensure_capabilities({
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
    }))

    vim.lsp.enable(vim.list_extend(servers, { "lua_ls" }))
  end,
}
