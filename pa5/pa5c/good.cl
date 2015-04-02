Class Main inherits IO {
    a: Int <- 3;                                        -- attribute, initialized, Int literal
    b: Int <- a;                                        -- attribute, initialized, identifier
    f: Bool <- true;                                    -- attribute, initialized, Bool literal
    g: Bool;                                            -- attribute, uninitialized
    s: String <- "3";                                   -- attribute, initialized, String literal
    t: String <- "abc";
    
    x: X <- new X;                                      -- attribute, initialized, new
    y: Y <- new Y;
    z: Z <- new X;                                      -- attribute, initialized, new, conformity

    main(): Object {
        {                                               -- block
            a <- a+b;                                   -- assign, plus
            a <- a-b;                                   -- assign, minus
            a <- a*b;                                   -- assign, times,
            a <- a/b;                                   -- assign, divide
            a <- ~a;                                    -- assign, negate

            f <- a = b;                                 -- assign, equal (Int)
            f <- a < b;                                 -- assign, lt (Int)
            f <- a <= b;                                -- assign, le (Int)

            f <- f = g;                                -- assign, equal (Bool)
            f <- f < g;                                 -- assign, lt (Bool)
            f <- f <= g;                                -- assign, le (Bool)

            f <- s = t;                                 -- assign, equal (String)
            f <- s < t;                                 -- assign, lt (String)
            f <- s <= t;                                -- assign, le (String)
            
            f <- x = y;                                 -- assign, equal (Object)
            f <- x < y;                                 -- assign, lt (Object)
            f <- x <= y;                                -- assign, le (Object)
            
            f <- not g;                                 -- assign, not
            f <- isvoid self;                           -- assign, isvoid, self
        }
    };
    
    m1(): Object {
        {
            z <-                                        -- lub conformity
            if (not (a < 5)) then                       -- if, parenthesis
                x
            else
                y
            fi;
            
            while (a < 5) loop                          -- while
                new Z
            pool;
            
            let i: String in                            -- let with no initialization
                let j: String <- "3", i: Int <- 3 in    -- let with initialization, Previous binding gets hidden
                    {
                        m1();                           -- self dispatch
                        x.m2(i);                        -- dynamic dispatch
                        y.m3(i,j);                      -- dynamic dispatch
                        y@Z.m3(i,j);                    -- static dispatch
                        
                    };
            
            
            z <-                                        -- lub conformity
            case x of                                   -- case expression
                id0: Int => new X;
                id1: String => new Y;
                id2: Z => id2;
            esac;
        }
    };
    
    mx1(): Int {                                        -- tests for built-in functions
        s.length()
    };
    
    mx2(): String {
        s.concat(t)
    };
    
    mx3(): Object {
        t <- s.substr(0, 0)
    };
    
    mx4(): Object {
        s@Object.abort()
    };
    
    mx5(): String {
        t <- s.type_name()
    };
    
    mx6(): String {
        {
            f <- g.copy();
            z <- y.copy();
            t <- s.copy();
        }
    };
    
    mx(): Main {
        {
            a <- self.in_int();
            s <- (new SELF_TYPE).in_string();
            out_int(a);
            out_string(s);
        }
    };
};

class Z {
    m2(x:Int):Int {
        x
    };
    
    m3(x: Int, y: String): SELF_TYPE {                  -- SELF_TYPE as return type
        new SELF_TYPE                                   -- SELF_TYPE in new
    };
};

Class X inherits Z {                                    -- inheritance
};

Class Y inherits Z {
    m3(x:Int, y:String): SELF_TYPE {                    -- method override
        self                                            -- return type conformity
    };
};
