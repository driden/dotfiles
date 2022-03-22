local M = {}

local function get_lua_opts()
    return {
        settings = {
            Lua = {
                runtime = {
                    version = 'LuaJIT',
                    path = vim.split(package.path, ';')
                },
                diagnostics = {globals = {'vim'}},
                workspace = {
                    library = vim.api.nvim_get_runtime_file("", true),
                    checkThirdParty = false
                },
                telemetry = {enable = false}
            }
        }
    }
end

local function setup_server(server)
    local opts = {}
    if server.name == "sumneko_lua" then
        opts = vim.tbl_deep_extend("force", get_lua_opts(), opts)
    end
    server:setup(opts)
end

function M.setup()
    vim.o.completeopt = "menu,menuone,noselect"
    local installer = require 'nvim-lsp-installer'
    installer.on_server_ready(setup_server)
    -- This setup() function will take the provided server configuration and decorate it with the necessary properties
    -- before passing it onwards to lspconfig.
    -- Refer to https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md

    -- nullls
    local null_ls = require("null-ls")
    local formatting = null_ls.builtins.formatting
    null_ls.setup({
        sources = {
            formatting.clang_format, formatting.cmake_format,
            formatting.dart_format, formatting.gofmt, formatting.lua_format,
            formatting.prettier, formatting.shfmt, formatting.stylua,
            null_ls.builtins.completion.spell,
            null_ls.builtins.diagnostics.eslint
        },
        on_attach = function(client)
            if client.resolved_capabilities.document_formatting then
                print("formatting")
                vim.cmd(
                    "autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_seq_sync()")
                if client.resolved_capabilities.document_highlight then
                    print("highlight")
                    vim.api.nvim_exec([[
          augroup document_highlight
            autocmd! * <buffer>
            autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
            autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
          augroup END
    ]], false)
                end
            end
        end
    })

    local no_really = {
        method = null_ls.methods.DIAGNOSTICS,
        filetypes = {"markdown", "text"},
        generator = {
            fn = function(params)
                local diagnostics = {}
                -- sources have access to a params object
                -- containing info about the current file and editor state
                for i, line in ipairs(params.content) do
                    local col, end_col = line:find("really")
                    if col and end_col then
                        -- null-ls fills in undefined positions
                        -- and converts source diagnostics into the required format
                        table.insert(diagnostics, {
                            row = i,
                            col = col,
                            end_col = end_col,
                            source = "no-really",
                            message = "Don't use 'really!'",
                            severity = 2
                        })
                    end
                end
                return diagnostics
            end
        }
    }

    null_ls.register(no_really)
end

return M
