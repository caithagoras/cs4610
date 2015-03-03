#!/bin/bash

tests=(good bad)
dir="."

for test in ${tests[@]}
do
    cool --out "$dir/reference" --parse "$dir/$test.cl" >& "$dir/reference.cl-ast"
    cool --lex "$dir/$test.cl" >& "$dir/$test.cl-lex"
    jison parser.jison
    nodejs main.js "$dir/$test.cl-lex" >& "$dir/$test.cl-ast"

    diff -b -B -E -w -s "$dir/reference.cl-ast" "$dir/$test.cl-ast"
    rm -f "$dir/reference.cl-ast" "$dir/$test.cl-lex" "$dir/$test.cl-ast"
done
