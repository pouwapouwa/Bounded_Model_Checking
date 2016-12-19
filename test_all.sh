#!/bin/sh

foo= "Test result for"

echo "##############################"

for filetest in examples/[^.]*; do
    ./bmc.d.byte $filetest;
    echo -e "\n##############################\n"
done
