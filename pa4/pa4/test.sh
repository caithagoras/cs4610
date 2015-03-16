#!/bin/bash

<<COMMENT
tests=(
    assign_to_self
    attribute_self
    binding_self_in_let
    case_bounded_twice
    case_self_type
    class_named_self_type
    #inheritance_cycle
    inherited_attribute_conflict
    inherited_method_conflict
    inherit_int
    inherit_string
    method_return_unconformed
    mismatched_arg_num
    mismatched_arg_type
    no_main_class
    no_main_fn
    non_bool_if_condition
    non_bool_loop_condition
    non_existed_attribute
    non_existed_class
    redefine_class
    redefine_feature
    redefine_feature_in_child_class
    redefine_self
    rosetta
    static_dispatch_self_type
    unconformed_arithmetic
    unconformed_assignment
    unconformed_init
    unconformed_let
    unconformed_negate
    unconformed_not
    unconformed_relational
    unconformed_static_dispatch
    unknown_id
    unknown_type
    unsort-cool
    valid_attribute_method_same_name
)
COMMENT
tests=(good)
dir="."

ghc main.hs

for test in ${tests[@]}
do
    cool --out "$dir/reference" --type "$dir/$test.cl" >& "$dir/reference.cl-type"
    cool --parse "$dir/$test.cl"
    ./main "$dir/$test.cl-ast" >& "$dir/$test.cl-type"

    diff -y -b -B -E -w -s "$dir/reference.cl-type" "$dir/$test.cl-type"
    rm -f "$dir/reference.cl-type" "$dir/$test.cl-type" "$dir/$test.cl-ast"
done
