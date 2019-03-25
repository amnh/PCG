#!/bin/bash

for file in *.dot;
do
  file_png=${file%.dot}.png
  dot ${file} -Tpng > ${file_png}
done
