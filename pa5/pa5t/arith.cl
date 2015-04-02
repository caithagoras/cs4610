class Main inherits IO {
    main(): Object {
        {
            out_int(3+5);
            out_int(3-5);
            out_int(3*5);
            out_int(3/5);
            out_int(~5);
            
            if false < true then
                out_int(0)
            else
                out_int(1)
            fi;
            
            if false <= true then
                out_int(0)
            else
                out_int(1)
            fi;
            
            if false = true then
                out_int(0)
            else
                out_int(1)
            fi;
            
            if true < false then
                out_int(0)
            else
                out_int(1)
            fi;
            
            if true <= false then
                out_int(0)
            else
                out_int(1)
            fi;
            
            if true = true then
                out_int(0)
            else
                out_int(1)
            fi;
            
            if false = false then
                out_int(0)
            else
                out_int(1)
            fi;
        }
    };
};
