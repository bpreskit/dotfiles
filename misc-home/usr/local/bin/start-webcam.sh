#!/bin/bash

set -e

REMOTE_PORT=8080
LOCAL_PORT=8075
phone_model="SM_G950U"

if ! adb devices -l | grep -q "model:$phone_model"; then
    echo "Phone isn't there. Exiting."
    exit
else
    device=$(adb devices -l | grep "model:$phone_model" | cut -d ' ' -f 1)
fi

if adb forward --list | grep -Pq "$device.*tcp:${REMOTE_PORT}$"; then
    echo "port already forwarded."
else
    adb forward tcp:$LOCAL_PORT tcp:$REMOTE_PORT
fi

if ! nc -zw2 localhost $LOCAL_PORT; then
    echo "Can't hit port $LOCAL_PORT.  check if webcam is streaming on $REMOTE_PORT"
    exit
fi

# Set it to use the front camera.
curl -X POST http://localhost:${LOCAL_PORT}/settings/ffc\?set\=on

ROTATE=0

ffmpeg -r 20 -i http://localhost:$LOCAL_PORT/video \
       -vf rotate=$ROTATE \
       -f v4l2 \
       -pix_fmt yuv420p \
       -r 20 \
       /dev/video0
