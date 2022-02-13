#!/bin/bash

if [[ -z "${DAV_DIR}" ]]; then
    DAV_DIR="/tmp/webdav"
fi

mount_dav_dir() {
  local user

  mkdir "${DAV_DIR}"
  sudo mount "${DAV_DIR}"
  # With tmux, it's possible that users will return a repeated list
  # e.g., "bpreskitt bpreskitt" rather than "bpreskitt".
  user=$(users | cut -d ' ' -f 1)
  sudo chown -R ${user}:${user} "${DAV_DIR}"
}

mount_dav_dir
