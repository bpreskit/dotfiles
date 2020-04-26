#!/bin/bash


usage() {
  cat <<EOF
refresh-dots.sh [--help] [--no-ansible] [--no-emacs] [--ansible-copy]
  --no-ansible
    Completely skip the ansible playbook, which would usually tell you which
    zsh files differ between your machine and the repo (plus some git config
    settings).
  --no-emacs
    Skip files in ~/.emacs.d
  --ansible-copy
    Live run of the ansible -- don't worry, your .zsh*'s will be backed up.
  -h, --help
    Show this help.
EOF
  exit 1
}

no_ansible=""
ansible_args="--check"
no_emacs=""

parse_args() {
  while [[ -n "$1" ]]; do
    case "$1" in
      "--no-ansible")
        no_ansible=1
        shift
        ;;
      "--no-emacs")
        no_emacs=1
        shift
        ;;
      "--ansible-copy")
        ansible_args=""
        shift
        ;;
      *)
        usage
        ;;
    esac
  done
}

parse_args "$@"

script_dir=$(dirname "${BASH_SOURCE[0]}")
repo_dir=$(git rev-parse --show-toplevel)

if [[ -z "${no_ansible}" ]]; then
  pushd "${script_dir}/ansible-setup"
  ansible-playbook ${ansible_args} -i local-machine refresh-dots.yml
  popd
fi

if [[ -z "${no_emacs}" ]]; then
  pushd "${repo_dir}/emacs.d"

  file_array=()
  while read file; do
    file_array=("${file_array[@]}" "$file")
  done < <(find . -type f -name "*.el")

  base_emacs_dir="${HOME}/.emacs.d"
  for repo_file in "${file_array[@]}"; do
    local_file="${base_emacs_dir}/${repo_file#./}"
    if [[ -f "${local_file}" ]]; then
      if ! diff -q "${repo_file}" "${local_file}" &>/dev/null; then
        read -p "${repo_file} differs between repo and local. Ediff them? (y/N) " -N 1 resp
        if grep -qPi "^y$" <<<$resp; then
          ediff_cmd="(ediff \"${repo_file}\" \"${local_file}\")"
          ALTERNATE_EDITOR="emacs -nw" emacsclient -nw --eval "$ediff_cmd"
        else
          echo "Skipping ediff of ${repo_file} and ${local_file}."
        fi
      else
        echo "No difference between ${repo_file} and ${local_file}. Skipping."
      fi
    else
      echo "local didn't exist"
      read -p "${repo_file} doesn't exist locally. Copy it in? (y/N) " -N 1 resp
      if grep -qPi "^y$" <<<$resp; then
        cp "${repo_file}" "${local_file}"
      fi
    fi
  done

  popd
fi
