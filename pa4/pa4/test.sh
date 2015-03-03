#!/bin/bash

<<COMMENT
tests=(
    class_named_self_type
    #inheritance_cycle
    inherited_attribute_conflict
    inherit_int
    inherit_string
    no_main_class
    no_main_fn
    non_existed_class
    redefine_class
    redefine_feature
    redefine_feature_in_child_class
    redefine_self
    unknown_type
    valid_attribute_method_same_name
)
COMMENT
tests=(t)
dir="."

ghc main.hs

for test in ${tests[@]}
do
    cool --out "$dir/reference" --class-map "$dir/$test.cl" >& "$dir/reference.cl-type"
    cool --parse "$dir/$test.cl"
    ./main "$dir/$test.cl-ast" >& "$dir/$test.cl-type"

    diff -y -b -B -E -w -s "$dir/reference.cl-type" "$dir/$test.cl-type"
    #rm -f "$dir/reference.cl-type" "$dir/$test.cl-type" "$dir/$test.cl-ast"
done
