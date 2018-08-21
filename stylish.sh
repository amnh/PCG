#!/bin/bash

find lib src test app -type f -name "*.hs" | while read fname; do
    stylish-haskell -i "$fname"
done
