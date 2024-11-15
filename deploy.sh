#!/bin/sh

makeCfg() {
    # mkdir -p "$HOME/.config/$1"
    mkdir -p "./tests/$1"
}

makeLink() {
    ln -s "$1" "$2"
}

# makecfg alacritty
# makecfg lf
# makecfg nvim
# makecfg shell
# makecfg sx
# makecfg sxhkd
# makecfg vis
# makecfg xmobar
# makecfg xmonad
# makecfg zsh

ls -A .config/ | sed 's/ /\n/g'
