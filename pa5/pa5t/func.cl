class Main inherits IO {
    x:Int <- 0;
    y:String <- "init";

    main(): Object {
        {
            setX(5, y);
            out_int(x);
            out_string(y);
            setX(7, y);
            out_int(x);
            out_string(y);
            out_int(y.length());
        }
    };
    
    setX(xx:Int, y:String): Int {
        {
            y <- "gg";
            x <- xx;
        }
    };
};
