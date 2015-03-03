class ListNode inherits IO {
    vs : String;
    vi : Int;
    next : ListNode;
    prev : ListNode;
    
    getString() : String {
        vs
    };
    
    getInt() : Int {
        vi
    };
    
    getNext() : ListNode {
        next
    };
    
    getPrev() : ListNode {
        prev
    };
    
    setString(s : String) : ListNode {
        {
            vs <- s;
            self;
        }
    };
    
    setInt(i : Int) : ListNode {
        {
            vi <- i;
            self;
        }
    };
    
    setNext(n : ListNode) : ListNode {
        {
            next <- n;
            self;
        }
    };
    
    setPrev(p : ListNode) : ListNode {
        {
            prev <- p;
            self;
        }
    };
};

class List inherits IO {
    head : ListNode;
    tail : ListNode;
    current : ListNode;
    
    init() : List {
        {
            head <- new ListNode;
            tail <- new ListNode;
            head.setNext(tail);
            tail.setPrev(head);
            current <- head;
            self;
        }
    };
    
    append(node : ListNode) : List {
        {
            node.setNext(tail);
            node.setPrev(tail.getPrev());
            tail.getPrev().setNext(node);
            tail.setPrev(node);
            self;
        }
    };
    
    appends(s : String) : List {
        let
            node : ListNode <- (new ListNode).setString(s)
        in
            append(node)
    };
    
    appendi(i : Int) : List {
        let
            node : ListNode <- (new ListNode).setInt(i)
        in
            append(node)
    };
    
    getCurrentString() : String {
        current.getString()
    };
    
    getCurrentInt() : Int {
        current.getInt()
    };
    
    ended() : Bool {
        current = tail
    };
    
    reset() : List {
        {
            current <- head.getNext();
            self;
        }
    };
    
    forward() : List {
        {
            if not ended() then
                current <- current.getNext()
            else
                self
            fi;
            self;
        }
    };
    
    printString() : Object {
        let
            node : ListNode <- head.getNext()
        in
            while not node = tail loop {
                    out_string(node.getString());
                    out_string("\n");
                    node <- node.getNext();
            }
            pool
    };
    
    printInt() : Object {
        let
            node : ListNode <- head.getNext()
        in {
            while not node = tail loop {
                    out_int(node.getInt());
                    out_string(" ");
                    node <- node.getNext();
            }
            pool;
            out_string("\n");
        }
    };
    
    findString(s : String) : Int {
        let
            node : ListNode <- head.getNext(),
            finished : Bool <- false,
            n : Int <- 0
        in {
            while not finished loop
                if node = tail then
                    finished <- true
                else {
                    if node.getString() = s then
                        finished <- true
                    else {
                        node <- node.getNext();
                        n <- n + 1;
                    }
                    fi;
                }
                fi
            pool;
            
            if node = tail then
                ~1
            else
                n
            fi;
        }
    };
    
    get(n : Int) : ListNode {
        let
            node : ListNode <- head.getNext(),
            finished : Bool <- false,
            i : Int <- 0
        in {
            while not finished loop
                if i = n then
                    finished <- true
                else
                    if node = tail then
                        finished <- true
                    else {
                        i <- i + 1;
                        node <- node.getNext();
                    }
                    fi
                fi
            pool;
            
            if node = tail then
                new ListNode
            else
                node
            fi;
        }
    };
    
    size() : Int {
        let
            node : ListNode <- head.getNext(),
            n : Int <- 0
        in {
            while not node = tail loop {
                node <- node.getNext();
                n <- n + 1;
            }
            pool;
            n;
        }
    };
};

class Main inherits IO { 
    content : List;
    tasks : List;
    indeg : List;
    order : List;
    
    init() : Object {
        {
            content <- (new List).init();
            tasks <- (new List).init();
            indeg <- (new List).init();
            order <- (new List).init();
        }
    };

    read_input() : Object {
        let line : String <- "0"
        in {
            while not (line <- in_string()) = "" loop
                content.appends(line)
            pool;
        }
    };
    
    init_tasks() : Object {
        {
            content.reset();
            while not content.ended() loop {
                let
                    i : Int <- tasks.findString(content.getCurrentString())
                in
                    if i = ~1 then {
                        tasks.appends(content.getCurrentString());
                        indeg.appendi(0);
                    }
                    else
                        "pass"
                    fi;
                content.forward();
            }
            pool;
        }
    };
    
    init_indeg() : Object {
        {
            content.reset();
            while not content.ended() loop {
                let
                    i : Int <- tasks.findString(content.getCurrentString()),
                    node : ListNode <- indeg.get(i)
                in
                    node.setInt(node.getInt() + 1);
                content.forward().forward();
            }
            pool;
        }
    };
    
    topological_sort() : Object {
        {
            let
                finished : Bool <- false,
                count : Int <- 0
            in
                while not finished loop {
                    indeg.reset();
                    tasks.reset();
                    
                    let
                        next_node : String <- ""
                    in {
                        while not indeg.ended() loop {
                            if indeg.getCurrentInt() = 0 then
                                if next_node = "" then
                                    next_node <- tasks.getCurrentString()
                                else
                                    if tasks.getCurrentString() < next_node then
                                        next_node <- tasks.getCurrentString()
                                    else
                                        "pass"
                                    fi
                                fi
                            else
                                "pass"
                            fi;
                            
                            indeg.forward();
                            tasks.forward();
                        }
                        pool;

                        if next_node = "" then
                            finished <- true
                        else {
                            order.appends(next_node);
                            indeg.get(tasks.findString(next_node)).setInt(~1);
                            
                            content.reset();
                            while not content.ended() loop
                                let
                                    s1 : String <- content.getCurrentString(),
                                    s2 : String <- content.forward().getCurrentString()
                                in {
                                    if next_node = s2 then
                                        let
                                            is1 : Int <- tasks.findString(s1),
                                            node : ListNode <- indeg.get(is1)
                                        in
                                            node.setInt(node.getInt()-1)
                                    else
                                        "pass"
                                    fi;
                                    content.forward();
                                }
                            pool;
                        }
                        fi;
                    };
                    
                }
                pool;
                
                if order.size() = indeg.size() then
                    order.printString()
                else
                    out_string("cycle\n")
                fi;
        }
    };
    
    main() : Object {
        {
            init();
            read_input();
            init_tasks();
            init_indeg();
            topological_sort();
        }
    };
};

