#!/bin/bash

mkdir /tmp/webdav
sudo mount /tmp/webdav
# With tmux, it's possible that users will return a repeated list
# e.g., "bpreskitt bpreskitt" rather than "bpreskitt".
user=$(users | cut -d ' ' -f 1)
sudo chown -R $(users):$(users) /tmp/webdav
