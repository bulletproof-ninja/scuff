#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "Need Scala version!"
    exit 1
fi

PUBLISH_VERSION="$(./version.sh)"
echo "Publishing version $PUBLISH_VERSION"

./gradlew \
    -Pversion="$PUBLISH_VERSION" \
    -DossrhUsername="$OSSRH_USER" \
    -DossrhPassword="$OSSRH_PASS" \
    -DscalaVersion="$1" \
    -DcompilerArgs="-Xdisable-assertions -opt:l:method -opt:l:inline" \
    --info \
    clean :test publish
