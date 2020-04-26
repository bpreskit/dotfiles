#!/bin/bash

script_dir=$(dirname "${BASH_SOURCE[0]}")
repo_dir=$(git rev-parse --show-toplevel)

pushd "${script_dir}/ansible-setup"
ansible-playbook --check -i local-machine refresh-dots.yml
popd

pushd "${repo_dir}/emacs.d"

file_array=()
while read file; do
  file_array=("${file_array[@]}" "$file")
done < <(find . -type f)

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
