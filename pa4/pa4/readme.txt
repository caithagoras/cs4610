Programming Assignment 4 - The Semantic Analyzer
CS4610
Leiqing Cai

The semantic analyzer is written in Haskell.

General Semantic Analysis Stages
    a. Read in the serialized inputs and build the AST.
    b. Build a list of class names.
    c. Build the parent map.
    d. Build the class map.
    e. Build the implementation map.
    f. Annotate the AST by type checking expressions.
    g. Re-build the class map and the implementation map using the annotated AST.
    h. Output the class map, the implementation map, the parent map, and the annotated AST.

    Notes:
        * Step a, g, h is guaranteed to be error-free by problem specification.
        * If any semantic errors were found in stage b-f, the program outputs an error message and exits at the end of that stage.
        * Stage a-e fullfills the requirements of pa4c.
        * Initialization expressions of attributes and bodies of methodies are not annotated at the end of stage e. After stage f, if there is no type-error, we simply need to rebuild the class map and the implementation map (call the appropriate function again) to have the initialization expressions and method body annotated.

Examples of Expression Type Checking:
    1. Case
            a. To type-check a case expression, first type check <expr0>. (See page Case of CRM for reference of expr0)
            b. If <expr0> contains type error, return the error.
            c. Fold left the list of the case element lists, maintain a list of bound types.
            d. For each case element
                - If the bound variable is "self", return an error.
                - If the bound type is "SELF_TYPE", return an error.
                - If the bound type is an undefined, return an error.
                - If the bound type is already in the list of bound types, return an error
                - Otherwise, the case element is valid, return the case element with its expression annotated, along with the static type of its expression.
                - Update the list of bound types.
            f. Returns the case expression with annotated <expr0>, annotated case elements, and its static type, which is the least upper bound of all the static types of the case elements.

    2. Dynamic Dispatch
            a. Type check the receiver object.
            b. Type check each actual parameter.
            c. From the implementation map, lookup the method name from class t0. t0 is the type of the receiver object if it is not "SELF_TYPE", otherwise it is the class environment. Report an error if none found.
            d. Match the actual parameters with the formal parameters
                - Check if number of parameters are the same.
                - Check if each actual parameter type conforms to the formal parameter type in the corresponding position.
            e. The type of the dispatch is the declared type of the method if it is not"SELF_TYPE", otherwise it is the class environment.

Test Cases:
    good.cl
        - Contain most Cool syntactical structures. See comments in good.cl.

    bad1.cl
        - Assign to self is not allowed.
    
    bad2.cl
        - Attribute cannot be named self.
    
    bad3.cl
        - Self cannot be bound in a let expression.
