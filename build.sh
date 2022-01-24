#!/usr/bin/env sh

if [ $1 = "build" ]; then
    echo "Building"
    stack install
elif [ $1 = "run" ]; then
    echo "Running"
    stack run -- build bar.tl -r
elif [ $1 = "test" ]; then
    echo "Running tests"
    echo "Not implemented yet."
    exit 1
else
    echo "Unsupported command: $1"
    exit 1
fi
