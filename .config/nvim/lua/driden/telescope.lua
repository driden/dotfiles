-- I tested this locally, it works :)
local M = {}
function M.del_buffers(opts)
    opts = opts or {}
    opts.previewer = true
    opts.sort_lastused = true
    opts.show_all_buffers = true
    -- opts.shorten_path = false
    opts.attach_mappings = function(prompt_bufnr, map)
        local delete_buf = function()
            local current_picker = action_state.get_current_picker(prompt_bufnr)
            local multi_selections = current_picker:get_multi_selection()

            if next(multi_selections) == nil then
                local selection = action_state.get_selected_entry()
                actions.close(prompt_bufnr)
                vim.api.nvim_buf_delete(selection.bufnr, { force = true })
            else
                actions.close(prompt_bufnr)
                for _, selection in ipairs(multi_selections) do
                    vim.api.nvim_buf_delete(selection.bufnr, { force = true })
                end
            end
        end

        map("i", "<C-x>", delete_buf)
        return true
    end
    require("telescope.builtin").buffers(require("telescope.themes").get_dropdown(opts))
end

return M
