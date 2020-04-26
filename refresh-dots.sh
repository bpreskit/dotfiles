#!/bin/bash

script_dir=$(dirname "${BASH_SOURCE[0]}")

pushd "${script_dir}/ansible-setup"
ansible-playbook --check -i local-machine refresh-dots.yml
popd
