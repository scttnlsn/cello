#!/usr/bin/env bash

if [ ! -d build/classes ]; then
    mkdir -p build/classes
fi

scalac -sourcepath src/ -d build/classes `find src/ -name '*.scala'`
