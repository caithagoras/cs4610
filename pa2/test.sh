#!/bin/bash

tests=(good)

for test in ${tests[@]}
do
    ocamllex main.mll
    ocamlc unix.cma str.cma *.ml
    ./a.out "$test.cl" >& "$test.cl-lex"
    cool --out reference --lex "$test.cl" >& "reference.cl-lex"
    diff -b -B -E -w -s "reference.cl-lex" "$test.cl-lex"
    #rm -f "reference.cl-lex" "$test.cl-lex"
done

rm *.cmi *.cmo a.out
