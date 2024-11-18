# xdg dirs
set -x XDG_CONFIG_HOME "$HOME/.config"
set -x XDG_CACHE_HOME "$HOME/.cache"
set -x XDG_DATA_HOME "$HOME/.local/share"

# path
set -x PATH $PATH /opt/google-cloud-cli/bin

# default editor
set -x EDITOR nvim
set -x VISUAL nvim

# custom dirs
set -x REPOS "$HOME/repos"

# fzf and ripgrep
set -x FZF_DEFAULT_COMMAND "rg --files --hidden --glob '!.git'"
set -x FZF_CTRL_T_COMMAND "$FZF_DEFAULT_COMMAND"

# hledger
set -x LEDGER_FILE "$REPOS/hledger/.hledger.journal"

# starship
set -x STARSHIP_CONFIG "$XDG_CONFIG_HOME/starship/starship.toml"
mkdir -p ~/.cache/starship
starship init fish | source

# mise
mise activate fish | source

function daycommit
    set formatted_date (date "+%Y-%m-%d")
    git commit -am "$formatted_date"
    git push
end

function hcommit
    cd "$REPOS/hledger"
    daycommit
end

if status is-interactive
    eval (zellij setup --generate-auto-start fish | string collect)
end
