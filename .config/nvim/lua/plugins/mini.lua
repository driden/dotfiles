return {
  { -- Collection of various small independent plugins/modules
    'echasnovski/mini.statusline',
    event = 'VimEnter',
    priority = 1000, -- Make sure to load this before all the other start plugins.
    config = function()
      local statusline = require 'mini.statusline'
      statusline.setup { use_icons = vim.g.have_nerd_font }

      -- You can configure sections in the statusline by overriding their
      -- default behavior. For example, here we set the section for
      -- cursor location to LINE:COLUMN
      ---@diagnostic disable-next-line: duplicate-set-field
      statusline.section_location = function()
        return '%2l:%-2v'
      end
    end,
  },
}
