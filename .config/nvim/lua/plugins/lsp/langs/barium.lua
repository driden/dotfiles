local setup = function()
	local lspconfig = require("lspconfig")
	local configs = require("lspconfig.configs")

  if not configs.barium then
    configs.barium = {
        default_config = {
            cmd = {'barium'};
            filetypes = {'brazil-config'};
            root_dir = function(fname)
                return lspconfig.util.find_git_ancestor(fname)
            end;
            settings = {};
        };
    }
  end

  lspconfig.barium.setup {}
end

setup();
