#!/bin/sh

# Default programs
export EDITOR="nvim"
export TERMINAL="alacritty"
# export TERMINAL="st"
# export BROWSER="firefox"

# XDG
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_STATE_HOME="$HOME/.local/state"

# Programs
export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME"/java
export GOPATH="$XDG_DATA_HOME"/go
export GOBIN="$XDG_DATA_HOME"/bin
export GHCUP_USE_XDG_DIRS=true
export CABAL_DIR="$XDG_CONFIG_HOME/cabal"
export STACK_XDG=1

# Plan 9 stuff
export PLAN9="$XDG_DATA_HOME"/plan9
export FONTSRV_DIR="$HOME/mnt/fontsrv"

# Load env
# [ -f "$HOME"/.ghcup/env ] && source "$HOME"/.ghcup/env
[ -f "$HOME"/.cargo/env ] && source "$HOME"/.cargo/env
[ -f "$XDG_DATA_HOME"/ghcup/env ] && source "$XDG_DATA_HOME"/ghcup/env

export PATH="$PATH:$PLAN9/bin:$GOBIN:$(find ~/.local/bin -type d | paste -sd ':' -)"
