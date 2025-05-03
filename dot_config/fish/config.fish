# xdg dirs
set -x XDG_CONFIG_HOME "$HOME/.config"
set -x XDG_CACHE_HOME "$HOME/.cache"
set -x XDG_DATA_HOME "$HOME/.local/share"

# brew
eval "$(/opt/homebrew/bin/brew shellenv)"

# path
set -x PATH $PATH /opt/google-cloud-cli/bin ~/.local/bin /Library/TeX/texbin/

# default editor
set -x EDITOR nvim
set -x VISUAL nvim

# custom dirs
set -x REPOS "$HOME/repos"

# fzf and ripgrep
set -x FZF_DEFAULT_COMMAND "rg --files --hidden --glob '!.git'"
set -x FZF_CTRL_T_COMMAND "$FZF_DEFAULT_COMMAND"

# mise
mise activate fish | source

# starship
set -x STARSHIP_CONFIG "$XDG_CONFIG_HOME/starship/starship.toml"
mkdir -p ~/.cache/starship

if status is-interactive; and not set -q INSIDE_EMACS
    starship init fish | source
    eval (zellij setup --generate-auto-start fish | string collect)
end
