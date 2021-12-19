#!/bin/bash

version=$(git log --decorate=short | head -n 1 | grep -E -o "tag: v[^,\)]+" | cut -c 7-)

if [ -z "$version" ]; then
    previous=$(git log --decorate=short | grep -E -o "tag: v[^,\)]+" | head -n 1 | cut -c 7-)
    version="$(echo "$previous" | awk -F. -v OFS=. '{$NF++;print}')-SNAPSHOT"
fi

echo "$version"
