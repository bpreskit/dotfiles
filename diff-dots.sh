#!/bin/bash

SRC_DIR="$(dirname ${BASH_SOURCE[0]})"
declare -A dotfiles
dotfiles=(
  ["$SRC_DIR/zshrc"]="$HOME/.zshrc"
  ["$SRC_DIR/zsh_aliases"]="$HOME/.zsh_aliases"
  ["$SRC_DIR/zshrc_pure"]="$HOME/.zshrc_pure"
  ["$SRC_DIR/emacs.d/init.el"]="$HOME/.emacs.d/init.el"
  ["$SRC_DIR/emacs.d/lisp/mycode.el"]="$HOME/.emacs.d/lisp/mycode.el"
  ["$SRC_DIR/emacs.d/lisp/keybindings.el"]="$HOME/.emacs.d/lisp/keybindings.el"
  ["$SRC_DIR/emacs.d/lisp/local-org.el"]="$HOME/.emacs.d/lisp/local-org.el"
)

for src in "${!dotfiles[@]}"; do
  diff_command="diff -bs $src ${dotfiles[$src]}"
  echo $diff_command
  eval $diff_command
  echo "========================================================="
  echo ""
done
