#!/bin/bash

rsync --filter="merge,- $HOME/.gitignore_global" -avz /tmp/webdav $HOME/Backup
