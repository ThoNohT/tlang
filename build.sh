#!/bin/sh

if [ $1 = "build" ]; then
    dotnet build
elif [ $1 = "run" ]; then
    dotnet run
elif [ $1 = "test" ]; then
    dotnet run test
else
    echo "Unsupported command: $1"
    exit 1
fi
