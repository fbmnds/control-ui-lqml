#!/bin/bash

sudo docker run --rm -ti -v $(pwd):/control-ui  -w /control-ui --entrypoint ./build.sh qt5.12-android:1.1
