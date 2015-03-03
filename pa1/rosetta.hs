import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

tuplelize :: [s] -> [(s,s)]
tuplelize [] = []
tuplelize (s1:s2:xr) = (s1,s2) : tuplelize(xr)

removeDups :: Eq s => [s] -> [s]
removeDups  = foldl (\current x -> if x `elem` current
                                    then current
                                    else current ++ [x]) []

buildIndeg :: Ord s => [s] -> Map s Int
buildIndeg [] = Map.empty
buildIndeg (s:xr) = Map.insert s 0 (buildIndeg xr)

initIndeg :: Ord s => [(s,s)] -> Map s Int -> Map s Int
initIndeg [] m = m
initIndeg ((s1,s2):xr) m = Map.adjust ((+) 1) s1 (initIndeg xr m)

pickNextOrder :: Ord s => Map s Int -> Maybe s
pickNextOrder m =
    let filtered = Map.filter (== 0) m in
        let candidates = Map.toAscList filtered in
            if length candidates == 0
               then Nothing
               else Just (fst (candidates !! 0))

adjustIndeg :: Ord s => [(s,s)] -> Map s Int -> s -> Map s Int
adjustIndeg edges indeg next_node =
    let indeg' = Map.adjust (\x -> -1) next_node indeg in
    let affected_edges = filter (\x -> snd x == next_node) edges in
    let affected_nodes = map (\x -> fst x) affected_edges in
        Map.mapWithKey (\key deg -> if key `elem` affected_nodes then deg-1 else deg) indeg'
               
topologicalSortRec :: Ord s => [(s,s)] -> Map s Int -> [s]
topologicalSortRec edges indeg =
    let next_node_mb = pickNextOrder indeg in
        if Maybe.isNothing next_node_mb
           then []
           else
                let next_node = Maybe.fromJust next_node_mb in
                    let indeg' = adjustIndeg edges indeg next_node in
                        next_node : topologicalSortRec edges indeg'

topologicalSort :: Ord s => [(s,s)] -> Map s Int -> Maybe[s]
topologicalSort edges indeg =
    let order = topologicalSortRec edges indeg in
        if length order == length (Map.toAscList indeg)
           then Just order
           else Nothing

main = do
    inputs <- getContents
    let content = lines inputs
    let edges = tuplelize content
    let tasks = removeDups content
    let indeg = initIndeg edges (buildIndeg tasks)
    let order = topologicalSort edges indeg
    
    if Maybe.isNothing order
       then do
           mapM putStrLn ["cycle"]
       else do
           mapM putStrLn (Maybe.fromJust order)