class A inherits B {
    feature_no_init: Int;                             -- uninitiliazed attribute
    feature_init: String <- "string";                 -- initiliazed attribute

    f1(): String{                                     -- method with no formal parameter
        if x < y then                                 -- if
            {                                         -- block
                a <- 3;                               -- assign
                a + ~b * (c - d) / e;                 -- arithmetic
                a < b;                                -- relational
                a <= b;                               -- relational
                a = b;                                -- relational
                not b;                                -- logical
                true;                                 -- logical
                false;                                -- logical
                "35" + 95 + a;                        -- string, integer, identifier
                (a + b)@A.f2(c, d);                   -- static dispatch
                (a + b).f2(c, d);                     -- dynamic dispatch
                f1();                                 -- self dispatch
                isvoid feature_no_init;               -- isvoid
                a <- new B;                           -- new
            }
        else
            while g <= h loop                         -- while
                let x:X, y:Y <- new Y in              -- let
                    let a:Int <- 3 in                 -- let
                        case x of                     -- case
                            id1: A => isvoid id1;
                            id2: B => isvoid id2;
                        esac
            pool
        fi
    };
    
    f2(a: Int, b: Some_Type):Int{                     -- method with formal parameters
        5
    };
};


class B {
                                                      -- class with no feature
};
