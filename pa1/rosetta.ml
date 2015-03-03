open Printf;;
module MapS = Map.Make(String);;

let rec tuplelize (x: 'a list) : ('a * 'a) list = 
    if List.length x >= 2 then
        (List.nth x 0, List.nth x 1) :: tuplelize (List.tl (List.tl x))
    else [];;
    
let remove_dups (x: 'a list) : 'a list =
    let check_dups current p =
        if List.mem p current then
            current
        else
            current @ [p]
    in
        List.fold_left check_dups [] x;;
        
let rec build_indeg indeg =
    if List.length indeg = 0 then
        MapS.empty
    else
        MapS.add (List.hd indeg) 0 (build_indeg (List.tl indeg));;

let rec init_indeg edges indeg =
    if List.length edges = 0 then
        indeg
    else
        let x, y = List.hd edges in
        let indeg' = MapS.add x ((MapS.find x indeg)+1) indeg in
            init_indeg (List.tl edges) indeg';;
                
let pick_next_order indeg = 
    try
        let filtered = MapS.filter (fun key value -> value = 0) indeg in
            let next_node, _ = MapS.min_binding filtered in
                Some next_node
    with Not_found ->
        None;;

let adjust_indeg edges indeg next_node =
    let indeg' = MapS.add next_node (-1) indeg in
    let affected_edges = List.filter (fun e -> let x, y = e in y = next_node) edges in
    let affected_nodes = List.map (fun e -> let x, y = e in x) affected_edges in
        MapS.mapi (fun key value -> if List.mem key affected_nodes then value-1 else value) indeg'

let rec topological_sort_rec edges indeg =
    let next_node' = pick_next_order indeg in
        match next_node' with
            | None -> []
            | Some next_node ->
                let indeg' = adjust_indeg edges indeg next_node in
                    next_node :: topological_sort_rec edges indeg';;

let topological_sort edges indeg =
    let order = topological_sort_rec edges indeg in
        if List.length order = MapS.cardinal indeg then
            Some order
        else
            None;;

let lines = ref [] in 
try
    while true do
        lines :=  (!lines) @ [read_line ()]
    done
with _ -> begin
    let edges = tuplelize !lines in
    let tasks = remove_dups !lines in
    let indeg = init_indeg edges (build_indeg tasks) in
    let order = topological_sort edges indeg in
        match order with
            | None -> printf "cycle\n"
            | Some order' -> List.iter print_endline order'
end

