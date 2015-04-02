class Main inherits IO {
    a1:Object;
    a2:Object;
    a3:Object <- new A;
    main(): Object {
        {

            if a1 < a2 then
                out_string("true\n")
            else
                out_string("false\n")
            fi;
            
            
            if a1 < a3 then
                out_string("true\n")
            else
                out_string("false\n")
            fi;

        }
    };
};

class A{
};
