#!/bin/sh

mkdir -p out
elm make src/Main.elm --output=out/app.js
cp src/index.html out

