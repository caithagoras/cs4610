class Main inherits IO{
    main():Object {
        let x:A <- new A, y:A in
        {
            x.seta(3);
            x.setb("asdf");
            
            y <- x.copy();
            y.seta(7);
            y.setb("weori");
            
            out_int(x.geta());
            out_string(x.getb().concat("\n"));
            out_int(y.geta());
            out_string(y.getb().concat("\n"));
        }
    };
};

class A{
    a:Int;
    b:String;
    
    seta(t:Int):SELF_TYPE{
        {
            a <- t;
            self;
        }
    };
    
    setb(t:String):SELF_TYPE{
        {
            b <- t;
            self;
        }
    };
    
    geta():Int{
        a
    };
    
    getb():String{
        b
    };  
};