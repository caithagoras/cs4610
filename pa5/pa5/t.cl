class Main inherits IO{
    a: Int<-3;
    b: Object;
    
    main():Object {
        {    
            
            b<-
            while a < 100 loop
            {
                a <- a + 1;
                out_int(a);
                out_string("\n");
            }
            pool;

            --let x:B <- y in
            --   if (x = y) then
            --        out_string("eq\n")
            --    else
            --        out_string("not eq\n")
            --    fi;
        }
    };
};


class A{
    a: Int;
    
    setA(t:Int):Object{
        a <- t
    };
    
    getA():Int{
        a
    };
};

class B inherits A{
};
