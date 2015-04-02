class Main inherits IO {
    b:Bool;
    c:String;
    main(): Object {
        {
            if b then
                out_string("true")
            else
                out_string("false")
            fi;
            out_string(c);
        }
    };
};
