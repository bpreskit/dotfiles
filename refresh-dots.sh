#!/bin/bash


usage() {
  cat <<EOF
refresh-dots.sh [--help] [--no-ansible] [--no-emacs] [--ansible-copy]
  --no-other
    Skip non-emacs dot files.
  --no-emacs
    Skip files in ~/.emacs.d
  -h, --help
    Show this help.
EOF
  exit 1
}

# For focusing on ediff
export MY_EMACS_WINDOW=${MY_EMACS_WINDOW:-"GNU Emacs"}

# Keep this in sync with copy_items in unpack_dots/vars/main.yml
OTHER_DOTS=(
  gitignore_global
  rgignore
  tmux.conf
  zsh_aliases
  zshrc
)
OTHER_DOT_DIRS=(
  oh-my-zsh
  config
)

no_other=""
no_emacs=""

parse_args() {
  while [[ -n "$1" ]]; do
    case "$1" in
      "--no-emacs")
        no_emacs=1
        shift
        ;;
      "--no-other")
        no_other=1
        shift
        ;;
      *)
        usage
        ;;
    esac
  done
}

maybe_ediff_files () {
    local local_file=$1
    local repo_file=$2
    if [[ -f "${local_file}" ]]; then
        if ! diff -q "${repo_file}" "${local_file}" &>/dev/null; then
            read -p "${repo_file} differs between repo and local. Ediff them? (y/N) " -N 1 resp
            echo ""
            if grep -qPi "^y$" <<<$resp; then
                wmctrl -a "${MY_EMACS_WINDOW}"
                ediff_cmd="(ediff \"${repo_file}\" \"${local_file}\")"
                ALTERNATE_EDITOR="emacs" emacsclient --eval "$ediff_cmd"
                wmctrl -a "ediff"
            else
                echo "Skipping ediff of ${repo_file} and ${local_file}."
            fi
        else
            echo "No difference between ${repo_file} and ${local_file}. Skipping."
        fi
    else
        echo "local didn't exist"
        read -p "${repo_file} doesn't exist locally. Copy it in? (y/N) " -N 1 resp
        echo ""
        if grep -qPi "^y$" <<<$resp; then
            cp "${repo_file}" "${local_file}"
        fi
    fi
}

do_emacs () {
    pushd "${repo_dir}/emacs.d"

    local file_array=()
    local file
    local repo_file
    local local_file
    local base_emacs_dir
    while read file; do
        file_array=("${file_array[@]}" "$file")
    done < <(find . -type f -name "*.el")

    base_emacs_dir="${HOME}/.emacs.d"
    for repo_file in "${file_array[@]}"; do
        local_file="${base_emacs_dir}/${repo_file#./}"
        maybe_ediff_files ${local_file} ${repo_file}
    done

    # Special handling for weirdly-named files.
    maybe_ediff_files ~/.emacs.d/.projectile projectile
    maybe_ediff_files ~/.emacs.d/.dir-locals.el dir-locals.el.suffix

    popd
}

do_other () {
    pushd "${repo_dir}"
    local repo_file
    local local_file
    for repo_file in ${OTHER_DOTS[@]}; do
        local_file="${HOME}/.${repo_file}"
        maybe_ediff_files "${local_file}" "${repo_file}"
    done
    local file_array=()
    local file

    for repo_file_dir in ${OTHER_DOT_DIRS}; do
        file_array=()
        while read file; do
            file_array=("${file_array[@]}" "$file")
        done < <(fd --type file . "${repo_file_dir}")
    done
    for repo_file in "${file_array[@]}"; do
        local_file="${HOME}/.${repo_file}"
        maybe_ediff_files ${local_file} ${repo_file}
    done
    popd
}

parse_args "$@"

script_dir=$(dirname "${BASH_SOURCE[0]}")
repo_dir=$(git rev-parse --show-toplevel)

if [[ -z "${no_other}" ]]; then
    do_other
fi

if [[ -z "${no_emacs}" ]]; then
    do_emacs
fi
