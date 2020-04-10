#!/bin/bash

pushd ansible-setup
ansible-playbook -i local-machine refresh-dots.yml
popd
