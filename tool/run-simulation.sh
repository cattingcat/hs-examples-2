#!/bin/sh

if [ -f "./package.yaml" ]; then
    stack run simulation --profile -- +RTS -p
    less ./simulation.prof 
    rm ./simulation.prof 
else 
    echo "Error: cd to project root"
fi
