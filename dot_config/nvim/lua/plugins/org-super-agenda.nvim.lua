return {
  "hamidi-dev/org-super-agenda.nvim",
  cmd = { "OrgSuperAgenda" },
  dependencies = { "nvim-orgmode/orgmode" },
  opts = {
    todo_states = {
      { name = "HOLD", keymap = "oh", color = "#6B8294" },
      { name = "TODO", keymap = "ot", color = "#8BB9EA" },
      { name = "PROG", keymap = "op", color = "#8ED1A3" },
      { name = "DONE", keymap = "od", color = "#6B8294", strike_through = true },
    },
    groups = {
      {
        name = "Today",
        matcher = function(i)
          return i.scheduled and i.scheduled:is_today()
        end,
        sort = { by = "scheduled_time", order = "asc" },
      },
      {
        name = "Overdue",
        matcher = function(i)
          return i.todo_state ~= "DONE"
            and ((i.deadline and i.deadline:is_past()) or (i.scheduled and i.scheduled:is_past()))
        end,
        sort = { by = "date_nearest", order = "asc" },
      },
      {
        name = "Inbox Items",
        matcher = function(i)
          return i:has_tag("inbox")
        end,
      },
      {
        name = "In Progress",
        matcher = function(i)
          return i.todo_state == "PROG"
        end,
      },
      {
        name = "Other Tasks",
        matcher = function(i)
          return i.todo_state == "TODO" or i.todo_state == "HOLD"
        end,
      },
    },
  },
}
