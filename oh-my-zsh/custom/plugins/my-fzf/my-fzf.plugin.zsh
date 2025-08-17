FZF_COPY='if [[ -n $TMUX ]]; then tmux set-buffer {}; fi; echo {} | xclip -i -selection clipboard'
export FZF_DEFAULT_OPTS="--multi --bind \"alt-v:page-up,ctrl-v:page-down,alt-<:last,alt->:first,alt-c:select-all+accept,alt-w:become($FZF_COPY)\""

function fzg {
    local rg_dir=""
    if [[ -d "${@[$#]}" ]]; then
        local rg_dir="${@[$#]}"
        argv=($argv[1,-2])
    fi
    local rg_options=$(printf "%q" "$@")
    local reload="reload:rg ${rg_options} --line-number --column --color=always --smart-case {q} ${rg_dir} || :"
    fzf --disabled --ansi \
        --no-multi \
        --bind "start:$reload" \
        --bind "change:$reload" \
        --delimiter : \
        --bind "alt-l:become(batcat --pager='less +{2}' --style=numbers --color=always --highlight-line {2} {1})" \
        --bind "alt-e:become(emacsclient -n +{2} {1}; wmctrl -a '${MY_EMACS_WINDOW}')" \
        --preview 'batcat --style=numbers --color=always --highlight-line {2} {1}' \
        --preview-window '+{2}/2'
}

function fzgb {
    for branch in $(git branch | cut -c 3-); do
        echo "branch: $branch"
        git show --decorate --no-patch $branch
        echo "\0"
    done | \
        fzf \
            --no-multi \
            --read0 \
            --gap \
            --highlight-line \
            --color "header:italic" \
            --header "M-c to checkout, RET to print" \
            --bind 'alt-c:become(echo {} | grep -P "^branch: " | cut -d " " -f 2- | xargs git checkout)'
}

function fzwm {
    wmctrl -l | fzf --with-nth "4.." --bind 'enter:become(wmctrl -ia {1})'
}

function gbg {
    git branch | fzf --preview 'git show --color=always {-1}' \
                     --height 40% --layout reverse
}

function gcog {
    git branch | fzf --no-multi \
                     --preview 'git show --color=always {-1}' \
                     --bind 'enter:become(git checkout {-1})' \
                     --height 40% \
                     --layout reverse
}

function fzgl {
    git log "$@" --decorate |
        awk '/^commit/ { print "\0" } { print }' |
        fzf --read0 --gap --ansi --highlight-line
}

function fzfd {
    local directory="$1"
    if [[ -z "${directory}" ]]; then
        directory="."
    fi
    find "${directory}" | \
        fzf --bind "alt-e:become(emacsclient -n {1}; wmctrl -a '${MY_EMACS_WINDOW}')" \
            --bind "alt-l:become(batcat --style=numbers --color=always {1})"
}

# Add the widgets
export FZF_CTRL_T_COMMAND=""
export FZF_ALT_C_COMMAND=""
source <(fzf --zsh | rg -v "(zle|bindkey).*fzf-history-widget")
# Add C-M-r for history
zle     -N                  fzf-history-widget
bindkey -M emacs "\e\C-r"   fzf-history-widget
zle     -N                  fzf-file-widget
bindkey -M emacs "\C-x\C-f" fzf-file-widget
zle     -N                  fzf-cd-widget
bindkey -M emacs "\C-xd"    fzf-cd-widget
