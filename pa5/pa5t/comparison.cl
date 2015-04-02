class Main inherits IO {
    a: A <- new A;
    b: B <- new B;
    c: A <- new A;
    main(): Object {
        {
            if a < b then
                out_string("a\n")
            else
                out_string("b\n")
            fi;
            
            if a < c then
                out_string("a\n")
            else
                out_string("b\n")
            fi;
            
            if b < a then
                out_string("a\n")
            else
                out_string("b\n")
            fi;
            
            if c < a then
                out_string("a\n")
            else
                out_string("b\n")
            fi;
            
            if a <= b then
                out_string("a\n")
            else
                out_string("b\n")
            fi;
            
            if a <= c then
                out_string("a\n")
            else
                out_string("b\n")
            fi;
            
            if b <= a then
                out_string("a\n")
            else
                out_string("b\n")
            fi;
            
            if c <= a then
                out_string("a\n")
            else
                out_string("b\n")
            fi;
            
            if a = b then
                out_string("a\n")
            else
                out_string("b\n")
            fi;
            
            if a = c then
                out_string("a\n")
            else
                out_string("b\n")
            fi;
        }
    };
};

class A{
};

class B{
};

