#!/bin/bash

tests=(100words ambig austen century cycle testcase)

echo "ml"
for test in ${tests[@]}
do
    ocaml unix.cma str.cma rosetta.ml < "testcases/$test.list" > "$test.out"
    diff -b -B -E -w -s "$test.out" "testcases/$test.answer"
    rm "$test.out"
done
echo ""

echo "hs"
ghc --make -o rosetta *.hs ;
for test in ${tests[@]}
do
    ./rosetta < "testcases/$test.list" > "$test.out"
    diff -b -B -E -w -s "$test.out" "testcases/$test.answer"
    rm "$test.out"
done
echo ""

echo "py"
for test in ${tests[@]}
do
    python rosetta.py < "testcases/$test.list" > "$test.out"
    diff -b -B -E -w -s "$test.out" "testcases/$test.answer"
    rm "$test.out"
done
echo ""

echo "rb"
for test in ${tests[@]}
do
    ruby rosetta.rb < "testcases/$test.list" > "$test.out"
    diff -b -B -E -w -s "$test.out" "testcases/$test.answer"
    rm "$test.out"
done
echo ""

echo "c"
gcc -o rosetta rosetta.c
for test in ${tests[@]}
do
    ./rosetta < "testcases/$test.list" > "$test.out"
    diff -b -B -E -w -s "$test.out" "testcases/$test.answer"
    rm "$test.out"
done
echo ""

echo "js"
for test in ${tests[@]}
do
    nodejs rosetta.js < "testcases/$test.list" > "$test.out"
    diff -b -B -E -w -s "$test.out" "testcases/$test.answer"
    rm "$test.out"
done
echo ""

echo "cl"
for test in ${tests[@]}
do
    cool rosetta.cl < "testcases/$test.list" > "$test.out"
    diff -b -B -E -w -s "$test.out" "testcases/$test.answer"
    rm "$test.out"
done
echo ""

rm -f ./rosetta *.o *.hi
