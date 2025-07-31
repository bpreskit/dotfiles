export FZF_DEFAULT_OPTS='--bind "alt-v:page-up,ctrl-v:page-down,alt-<:last,alt->:first"'
export MY_FZF_EMACS_WINDOW=${MY_FZF_EMACS_WINDOW:-"GNU Emacs"}

function fzg {
    local rg_options=$(printf "%q" "$@")
    local reload="reload:rg ${rg_options} --line-number --column --color=always --smart-case {q} || :"
    fzf --disabled --ansi \
              --bind "start:$reload" \
              --bind "change:$reload" \
              --delimiter : \
              --bind "alt-l:become(batcat --pager='less +{2}' --style=numbers --color=always --highlight-line {2} {1})" \
              --bind "alt-e:become(emacsclient -n +{2} {1}; wmctrl -a '${MY_FZF_EMACS_WINDOW}')" \
              --preview 'batcat --style=numbers --color=always --highlight-line {2} {1}' \
              --preview-window '+{2}/2'
}

function gbg {
    git branch | fzf --preview 'git show --color=always {-1}' \
                     --height 40% --layout reverse
}

function gcog {
  git branch | fzf --preview 'git show --color=always {-1}' \
                   --bind 'enter:become(git checkout {-1})' \
                   --height 40% --layout reverse
}

function fzfd {
    local directory="$1"
    if [[ -z "${directory}" ]]; then
        directory="."
    fi
    find "${directory}" | fzf
}
