class Main inherits IO{
    main():Object {
        let line : String <- "0"
        in {
            while (line <- in_string()) = "" loop
            {
                out_string("l=");
                out_string(line);
                out_string("\n");
            }
            pool;
        }
    };
};


