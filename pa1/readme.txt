Programming Assignment 1 - The Rosetta Stone
CS4610
Leiqing Cai

This zip file contains 7 programs, written in Ruby, OCaml, Haskell, JavaScript, Python, C and Cool, respectively. All programs solve the topological sort problem.

General Algorithm Description:
    1. Read in inputs. Each task is a node. For task B that is dependent on task A, create an edge pointing from A to B.
    2. Create a mapping from each node (task) to its in-degree.
    3. order = a list that represent the topological order. Initialized to empty.
    4. S = The set of all nodes that have an in-degree of 0. Loop until S is empty.
        a. next_node = node with the least lexicographic order in S.
        b. append next_node to the list order.
        c. Change the in-degree of next_node to -1, so that it will never be in S thereafter.
        d. For each edge (next_node, k), decrement the in-degree of node k by 1.
    5. If the length of the list order is the same as the number of node, then the required topological order is found. Otherwise, a cycle exists.

Language Comparison of Implementation:
    Python, Ruby, JavaScript:
        The three languages are all dynamically-typed and object-oriented, with abundant built-in data structures, such as lists and maps.
        Therefore, implementing with those three languages are realtive simple and similar.
        The in-degrees are stored using a dictionary in Python, a Hash in Ruby, and an associtive array in JavaScript.
        The graph is constructed using the inputs and is stored using a String -> String map.
        
    C:
        Since the size of the inputs is unknown in advance, a dynamically-sized data structure is necessary. Since C is not object oriented and does not have very good library support, I created a Vector struct that mimics the behavior of a vector in C++. When storing the indegree, instead of having a map from tasks to integers, I have a list of tasks and a list of integers that correspond to each other. Due to the difficulty of constructing the graph, the original array of edges are used directly to update the in-degree in each iteration of the loop.
    
    Cool:
        Cool is not cool! Since it does not even have array, I built a linked list class to simulate a linear structure. The Implementation is similar to C though the code is much longer due to its poor express power.
    
    Hashkell and OCaml:
        Both languages are functional. For Implementation, I broke down the algorithm into several small and managable pieces, including:
            1. Store the inputs as a list of string, with each line being an element.
            2. tuplelize: Takes in the input list and returns a list of tuples, each containing the two related tasks, that is, a list of edges.
            3. remove_dups: Takes in the input list and returns a list without duplication, that is, a list of all tasks.
            4. build_indeg: Build the in-degree mapping from the task list, with all in-degree set to 0.
            5. init_indeg: Initialize the in-degree mapping with their initial values.
            6. Loop:
                6a. pick_next_order: Given in-degree, find the next node in the topological order.
                6b. adjust_indegree: Adjust the in-degree mapping according to the edge list and the next node.
            7. Output accordingly.

Test Case:
    The test case is designed to have 26 tasks, labeled A to Z. The inputs contains rules for each pair of different tasks, with the smaller one (lexicographically) dependent on the larger one.
    Therefore, the output should be a 26 lines, from Z to A.
