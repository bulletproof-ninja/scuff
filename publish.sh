#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "Need Scala version!"
    exit 1
fi

./gradlew \
    -Pversion="$(./version.sh)" \
    -DossrhUsername="$OSSRH_USER" \
    -DossrhPassword="$OSSRH_PASS" \
    -DscalaVersion="$1" \
    -DcompilerArgs="-Xdisable-assertions -opt:l:method -opt:l:inline" \
    --info \
    clean :test publish
