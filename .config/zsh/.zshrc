# Load environment variables
source ~/.config/shell/profile.sh

# Colors and prompt
autoload -U colors && colors	# Load colors
PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "
setopt autocd		# Automatically cd into typed directory.

# History stuff
HISTSIZE=10000000
SAVEHIST=10000000
HISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/history"

# Load completion
autoload -U compinit
compinit
_comp_options+=(globdots) # Hidden files

bindkey -e

# Load aliases if they exist
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"

# Load fzf-tab
FZF_TAB_PLUGIN="$HOME"/opt/fzf-tab/fzf-tab.plugin.zsh
[ -f "$FZF_TAB_PLUGIN" ] && source "$FZF_TAB_PLUGIN"

# Load zsh-autosuggestions
ZSH_AUTOSUG=/usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
[ -f "$ZSH_AUTOSUG" ] && source "$ZSH_AUTOSUG"

# Load zsh syntax highlighting
ZSH_SYNTAX=/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
[ -f "$ZSH_SYNTAX" ] && source "$ZSH_SYNTAX"

