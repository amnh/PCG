#!/bin/bash

find lib src test app utils -type f -name "*.hs" | while read fname; do
    stylish-haskell -i "$fname"
done
