# xdg dirs
set -x XDG_CONFIG_HOME "$HOME/.config"
set -x XDG_CACHE_HOME "$HOME/.cache"
set -x XDG_DATA_HOME "$HOME/.local/share"

# theme
set -g fish_color_autosuggestion brblack
set -g fish_color_cancel -r
set -g fish_color_command blue
set -g fish_color_comment red
set -g fish_color_cwd green
set -g fish_color_cwd_root red
set -g fish_color_end green
set -g fish_color_error brred
set -g fish_color_escape brcyan
set -g fish_color_history_current --bold
set -g fish_color_host normal
set -g fish_color_host_remote yellow
set -g fish_color_normal normal
set -g fish_color_operator brcyan
set -g fish_color_param cyan
set -g fish_color_quote yellow
set -g fish_color_redirection cyan --bold
set -g fish_color_search_match bryellow --background=brblack
set -g fish_color_selection white --bold --background=brblack
set -g fish_color_status red
set -g fish_color_user brgreen
set -g fish_color_valid_path --underline
set -g fish_pager_color_completion normal
set -g fish_pager_color_description yellow -i
set -g fish_pager_color_prefix normal --bold --underline
set -g fish_pager_color_progress brwhite --background=cyan
set -g fish_pager_color_selected_background -r

# doom path
set -x PATH $PATH $HOME/.config/emacs/bin

# brew
eval "$(/opt/homebrew/bin/brew shellenv)"

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
