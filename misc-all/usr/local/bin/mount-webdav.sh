#!/bin/bash

mkdir /tmp/webdav
sudo mount /tmp/webdav
sudo chown -R $(users):$(users) /tmp/webdav
