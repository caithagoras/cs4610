#!/bin/bash

tests=(rosetta)
input="t.in"
dir="."

for test in ${tests[@]}
do
    cool --out "$dir/reference" "$dir/$test.cl" < "$input" >& "$dir/reference.out"
    cool --type "$dir/$test.cl"
    python main.py "$dir/$test.cl-type" <"$input" >& "$dir/$test.out"

    diff -y -b -B -E -w -s "$dir/reference.out" "$dir/$test.out"
    rm -f "$dir/reference.out" "$dir/$test.out"
done
