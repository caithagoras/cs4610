class Main inherits IO {
    a: Main;
    main(): Object {
        {
            a <- self.copy();
            if a = self then
                out_string("1")
            else
                out_string("2")
            fi;
        }
    };
};
