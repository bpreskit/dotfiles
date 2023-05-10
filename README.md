How to use this repo\'s contents
================================

Refresh emacs dotfiles
----------------------

    ./refresh-dots.sh --no-ansible

Refresh all dotfiles
--------------------

    ./refresh-dots.sh --ansible-copy

Setup new system
----------------

1.  You will need
    -   This repo.
    -   Ansible (gotten with `sudo apt install ansible`).
2.  Go to the `ansible-setup` directory.
3.  Use `sudo add-apt-repository` to add the PPAs listed in
    `roles/common_packages/vars/main.yml`.
4.  Copy `ansible.cfg` to `/etc/ansible`.
5.  Run `ansible-playbook -K -i local-machine setup.yml` and give it
    your `sudo` password.
