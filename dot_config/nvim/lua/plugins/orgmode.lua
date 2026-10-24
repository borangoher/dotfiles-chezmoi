return {
  "nvim-orgmode/orgmode",
  event = "VeryLazy",
  ft = { "org" },
  config = function()
    local orgdir = vim.g.org_directory

    require("orgmode").setup({
      org_agenda_files = orgdir .. "**/*",
      org_default_notes_file = orgdir .. "inbox.org",
      org_archive_location = orgdir .. "archive.org::",

      org_log_done = "time",
      org_log_into_drawer = "LOGBOOK",
      org_hide_emphasis_markers = true,
      org_startup_indented = true,

      org_todo_keywords = { "HOLD(h)", "TODO(t)", "PROG(p)", "|", "DONE(d)" },
      org_todo_keyword_faces = {
        HOLD = ":foreground #6B8294",
        TODO = ":foreground #8BB9EA :weight bold",
        PROG = ":foreground #8ED1A3 :weight bold",
        DONE = ":foreground #6B8294",
      },

      org_capture_templates = {
        i = {
          description = "Inbox",
          template = "** TODO %^{Task description}",
          target = orgdir .. "inbox.org",
          headline = "Inbox Items",
        },
        n = {
          description = "Note",
          template = "** %^{Heading}\n%^{Explanation}",
          target = orgdir .. "inbox.org",
          headline = "Inbox Items",
          properties = { empty_lines = 1 },
        },
      },

      org_agenda_span = 10,
      org_agenda_block_separator = "―",
      org_agenda_current_time_string = "ᐊ┈┈┈┈┈┈┈ Now",
      org_agenda_time_grid = {
        type = { "today", "require-timed", "remove-match" },
        times = { 900, 1200, 1400, 1700, 2100 },
        time_separator = "      ",
        time_label = "┈┈┈┈┈┈┈┈┈┈┈┈┈",
      },
      org_agenda_custom_commands = {
        o = {
          description = "Overview",
          types = {
            { type = "agenda" },
            { type = "tags_todo", match = "inbox", org_agenda_overriding_header = "Inbox Items" },
            { type = "tags_todo", match = "/PROG", org_agenda_overriding_header = "In Progress" },
            { type = "tags_todo", match = "/TODO|HOLD", org_agenda_overriding_header = "Other Tasks" },
          },
        },
      },
    })

    -- Soft wrap and concealed markup in org buffers
    vim.api.nvim_create_autocmd("FileType", {
      pattern = "org",
      callback = function()
        vim.opt_local.wrap = true
        vim.opt_local.linebreak = true
        vim.opt_local.conceallevel = 2
        vim.opt_local.concealcursor = ""
      end,
    })

    -- Tangle on save for files with tangle header args
    vim.api.nvim_create_autocmd("BufWritePost", {
      pattern = "*.org",
      callback = function(args)
        if vim.fn.search(":tangle", "nw") > 0 then
          require("orgmode").action("org_mappings.org_babel_tangle")
        end
      end,
    })
  end,
}
