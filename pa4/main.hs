import System.Environment
import System.Exit
import System.IO
import Text.Printf
import Debug.Trace

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

data Node = Class Node (Maybe Node) [Node]              -- class_name, super_class, feature_list
          | Id String Int                               -- id, line_num
          | Attribute Node Node (Maybe Node)            -- id, type, [init]
          | Method Node [Node] Node Node                -- id, formal_list, type, body
          | Formal Node Node                            -- id, type
                                                        -- Expressions
          | Assign Int Node Node                        -- line_num, lhs, rhs
          | DynamicDispatch Int Node Node [Node]        -- line_num, e, method, args
          | StaticDispatch Int Node Node Node [Node]    -- line_num, e, type, method, args
          | SelfDispatch Int Node [Node]                -- line_num, method, args
          | If Int Node Node Node                       -- line_num, predicate, then_expr, else_expr
          | While Int Node Node                         -- line_num, predicate, body
          | Block Int [Node]                            -- line_num, body
          | New Int Node                                -- line_num, type_id
          | Isvoid Int Node                             -- line_num, x
          | Plus Int Node Node                          -- line_num, x, y
          | Minus Int Node Node                         -- line_num, x, y
          | Times Int Node Node                         -- line_num, x, y
          | Divide Int Node Node                        -- line_num, x, y
          | LessThan Int Node Node                      -- line_num, x, y
          | LessEqual Int Node Node                     -- line_num, x, y
          | Equal Int Node Node                         -- line_num, x, y
          | Not Int Node                                -- line_num, x
          | Negate Int Node                             -- line_num, x
          | IntL Int Int                                -- line_num, value
          | StringL Int String                          -- line_num, value
          | BoolL Int Bool                              -- line_num, truth
          | Identifier Int Node                         -- line_num, id
          | Let Int [Node] Node                         -- line_num, bindings, body
          | Case Int Node [Node]                        -- line_num, expr, case_elements
          | LetBinding Node Node (Maybe Node)           -- var, type, [init]
          | CaseElement Node Node Node                  -- var, type, expr

data Err = Err Int String                               -- line_num, error_message
data Attr = Attr String String (Maybe Node)             -- name, type, init

isRight :: Either x y -> Bool
isRight (Right _) = True
isRight _ = False

getLeft :: Either x y -> x
getLeft (Left p) = p

getRight :: Either x y -> y
getRight (Right p) = p

build_list :: ([String] -> (Node, [String])) -> [String] -> ([Node], [String])
build_list build_fn (num:content) =
    build_list_rec build_fn content (read num :: Int)

build_list_rec :: ([String] -> (Node, [String])) -> [String] -> Int -> ([Node], [String])
build_list_rec build_fn content num =
    if num == 0
       then ([], content)
       else let (element, content') = build_fn content
            in let (elements, content'') = build_list_rec build_fn content' (num - 1)
               in (element:elements, content'')

build_id :: [String] -> (Node, [String])
build_id (line:name:content) = (Id name (read line :: Int), content)
               
build_ast :: [String] -> [Node]
build_ast content =
    let (class_list, _) = build_list build_class content
    in class_list

build_class :: [String] -> (Node, [String])
build_class (class_line:class_name:"no_inherits":content) =
    (Class (Id class_name (read class_line :: Int)) Nothing feature_list, content')
    where (feature_list, content') = build_list build_feature content
build_class (class_line:class_name:"inherits":super_line:super_name:content) =
    (Class (Id class_name (read class_line :: Int)) (Just (Id super_name (read super_line :: Int))) feature_list, content')
    where (feature_list, content') = build_list build_feature content

build_feature :: [String] -> (Node, [String])
build_feature ("attribute_no_init":name_line:name:type_line:type_name:content) =
    (Attribute (Id name (read name_line :: Int)) (Id type_name (read type_line :: Int)) Nothing, content)
build_feature ("attribute_init":name_line:name:type_line:type_name:content) =
    (Attribute (Id name (read name_line :: Int)) (Id type_name (read type_line :: Int)) (Just initial), content')
    where (initial, content') = build_expr content
build_feature ("method":name_line:name:content) =
    (Method (Id name (read name_line :: Int)) formal_list (Id type_name (read type_line :: Int)) body, content''')
    where (formal_list, content') = build_list build_formal content
          (type_line:type_name:content'') = content'
          (body, content''') = build_expr content''

build_formal :: [String] -> (Node, [String])
build_formal (name_line:name:type_line:type_name:content) =
    (Formal (Id name (read name_line :: Int)) (Id type_name (read type_line :: Int)), content)

build_expr :: [String] -> (Node, [String])
build_expr (line:"assign":content) = (Assign line_num lhs rhs, content'')
    where (lhs,content') = build_id content
          (rhs, content'') = build_expr content'
          line_num = read line :: Int

build_expr (line:"dynamic_dispatch":content) = (DynamicDispatch line_num e method args, content''')
    where (e, content') = build_expr content
          (method, content'') = build_id content'
          (args, content''') = build_list build_expr content''
          line_num = read line :: Int

build_expr (line:"static_dispatch":content) = (StaticDispatch line_num e type_id method args, content'''')
    where (e, content') = build_expr content
          (type_id, content'') = build_id content'
          (method, content''') = build_id content''
          (args, content'''') = build_list build_expr content'''
          line_num = read line :: Int

build_expr (line:"self_dispatch":content) = (SelfDispatch line_num method args, content'')
    where (method, content') = build_id content
          (args, content'') = build_list build_expr content'
          line_num = read line :: Int

build_expr (line:"if":content) = (If line_num predicate then_expr else_expr, content''')
    where (predicate, content') = build_expr content
          (then_expr, content'') = build_expr content'
          (else_expr, content''') = build_expr content''
          line_num = read line :: Int

build_expr (line:"while":content) = (While line_num predicate body, content'')
    where (predicate, content') = build_expr content
          (body, content'') = build_expr content'
          line_num = read line :: Int

build_expr (line:"block":content) = (Block line_num body, content')
    where (body, content') = build_list build_expr content
          line_num = read line :: Int

build_expr (line:"new":content) = (New line_num type_id, content')
    where (type_id, content') = build_id content
          line_num = read line :: Int

build_expr (line:command:content)
    | command == "plus" = (Plus line_num x y, content'')
    | command == "minus" = (Minus line_num x y, content'')
    | command == "times" = (Times line_num x y, content'')
    | command == "divide" = (Divide line_num x y, content'')
    | command == "lt" = (LessThan line_num x y, content'')
    | command == "le" = (LessEqual line_num x y, content'')
    | command == "eq" = (Equal line_num x y, content'')
    where (x, content') = build_expr content
          (y, content'') = build_expr content'
          line_num = read line :: Int
    
build_expr (line:command:content)
    | command == "isvoid" = (Isvoid line_num x, content')
    | command == "not" = (Not line_num x, content')
    | command == "negate" = (Negate line_num x, content')
    where (x, content') = build_expr content
          line_num = read line :: Int
    
build_expr (line:command:content)
    | command == "integer" = (IntL line_num (read value :: Int), content')
    | command == "string" = (StringL line_num value, content')
    where (value:content') = content
          line_num = read line :: Int

build_expr (line:"identifier":content) = (Identifier line_num ident, content')
    where (ident, content') = build_id content
          line_num = read line :: Int

build_expr (line:command:content)
    | command == "true" = (BoolL line_num True, content)
    | command == "false" = (BoolL line_num False, content)
    where line_num = read line :: Int

build_expr (line:"let":content) = (Let line_num bindings body, content'')
    where (bindings, content') = build_list build_let_binding content
          (body, content'') = build_expr content'
          line_num = read line :: Int

build_expr (line:"case":content) = (Case line_num expr case_elements, content'')
    where (expr, content') = build_expr content
          (case_elements, content'') = build_list build_case_element content'
          line_num = read line :: Int

build_let_binding :: [String] -> (Node, [String])
build_let_binding ("let_binding_no_init":content) = (LetBinding var type_id Nothing, content'')
    where (var, content') = build_id content
          (type_id, content'') = build_id content'
build_let_binding ("let_binding_init":content) = (LetBinding var type_id (Just initial), content''')
    where (var, content') = build_id content
          (type_id, content'') = build_id content'
          (initial, content''') = build_expr content''

build_case_element :: [String] -> (Node, [String])
build_case_element content = (CaseElement var type_id expr, content''')
    where (var, content') = build_id content
          (type_id, content'') = build_id content'
          (expr, content''') = build_expr content''

create_class_ids :: [Node] -> Either [String] Err
create_class_ids ast = class_ids
    where class_ids = foldl update_class_ids init_class_ids ast
          init_class_ids = Left ["Bool", "IO", "Int", "Object", "String"]

          update_class_ids :: Either [String] Err -> Node -> Either [String] Err
          update_class_ids (Right err) node = Right err
          update_class_ids (Left p) (Class (Id class_name class_line) _ _) =
              if elem class_name p
                 then Right $ Err class_line (printf "class %s redefined" class_name)
                 else Left $ class_name:p

create_parent_map :: [String] -> [Node] -> Either (Map String String) Err
create_parent_map class_ids [] = Left $ Map.fromList [("Bool", "Object"), ("IO", "Object"), ("Int", "Object"), ("String", "Object")]
create_parent_map class_ids ((Class (Id class_name class_line) (Just (Id super_name super_line)) _):xs) =
    if super_name == "Int" || super_name == "Bool" || super_name == "String"
       then Right $ Err super_line $ printf "class %s inherits from %s" class_name super_name
       else if not (elem super_name class_ids)
            then Right $ Err super_line $ printf "class %s inherits from unknown class %s" class_name super_name
            else let parent_map = create_parent_map class_ids xs
                 in if isRight parent_map
                       then parent_map
                       else Left $ Map.insert class_name super_name $ getLeft parent_map
create_parent_map class_ids ((Class (Id class_name class_line) Nothing _):xs) =
    let parent_map = create_parent_map class_ids xs
    in if isRight parent_map
          then parent_map
          else Left $ Map.insert class_name "Object" $ getLeft parent_map

has_cycle :: [String] -> Map String String -> Bool
has_cycle [] parent_map = False
has_cycle (class_id:class_ids) parent_map = found_cycle || has_cycle class_ids parent_map
    where found_cycle = Map.member class_id parent_map && encounter (Maybe.fromJust (Map.lookup class_id parent_map)) class_id parent_map
          encounter :: String -> String -> Map String String -> Bool
          encounter current goal parent_map =
              current == goal || Map.member current parent_map && encounter (Maybe.fromJust (Map.lookup current parent_map)) goal parent_map

get_class :: String -> [Node] -> Node
get_class class_name ast =
    head $ filter (\(Class (Id name _) _ _) -> name == class_name) ast

create_class_map :: [Node] -> [Node] -> Map String String -> Either (Map String [Attr]) Err
create_class_map [] _ _ = Left $ Map.fromList [("Bool", []), ("IO", []), ("Int", []), ("Object", []), ("String", [])]
create_class_map ((Class (Id class_name _) _ _):xs) ast parent_map =
    let class_map = create_class_map xs ast parent_map
    in if isRight class_map || Map.member class_name (getLeft class_map)
          then class_map
          else add_to_class_map class_name ast parent_map $ getLeft class_map

    where add_to_class_map :: String -> [Node] -> Map String String -> Map String [Attr] -> Either (Map String [Attr]) Err
          add_to_class_map class_name ast parent_map class_map =
              if Map.member class_name class_map
                 then Left class_map
                 else let Just parent_name = Map.lookup class_name parent_map
                          parent_node = get_class parent_name ast
                          class_map' = add_to_class_map parent_name ast parent_map class_map
                      in if isRight class_map'
                            then class_map'
                            else let Just parent_attributes = Map.lookup parent_name (getLeft class_map')
                                     (Class _ _ feature_list) = get_class class_name ast
                                     class_attributes = add_to_attribute_list class_name parent_attributes (reverse feature_list)
                                 in if isRight class_attributes
                                       then Right $ getRight class_attributes
                                       else Left $ Map.insert class_name (getLeft class_attributes) (getLeft class_map')

          add_to_attribute_list :: String -> [Attr] -> [Node] -> Either [Attr] Err
          add_to_attribute_list class_name attributes [] = Left attributes
          add_to_attribute_list class_name attributes (feature:features) =
              let attributes' = add_to_attribute_list class_name attributes features
              in if isRight attributes'
                    then attributes'
                    else update_attribute_list class_name (getLeft attributes') feature

          update_attribute_list :: String -> [Attr] -> Node -> Either [Attr] Err
          update_attribute_list class_name attributes (Attribute (Id attr_name attr_line) (Id type_name _) optional_init) =
              if any (\(Attr x _ _) -> x == attr_name) attributes
                 then Right $ Err attr_line $ printf "class %s redefines attribute %s" class_name attr_name
                 else Left $ attributes ++ [Attr attr_name type_name optional_init]
          update_attribute_list class_name attributes (Method _ _ _ _) = Left attributes

output_nodes :: [Node] -> [String]
output_nodes nodes = foldl (\acc node -> acc ++ output_node node) [] nodes

output_node_list :: [Node] -> [String]
output_node_list nodes = show (length nodes) : output_nodes nodes

output_node :: Node -> [String]
output_node (Id var line) = [show line, var]
output_node (Assign line lhs rhs) = show line : "assign" : output_node lhs ++ output_node rhs
output_node (DynamicDispatch line e method args) = show line : "dynamic_dispatch" : output_nodes [e, method] ++ output_node_list args
output_node (StaticDispatch line e type_id method args) = show line : "static_dispatch" : output_nodes [e, type_id, method] ++ output_node_list args
output_node (SelfDispatch line method args) = show line : "self_dispatch" : output_node method ++ output_node_list args
output_node (If line predicate then_expr else_expr) = show line : "if" : output_nodes [predicate, then_expr, else_expr]
output_node (While line predicate body) = show line : "while" : output_nodes [predicate, body]
output_node (Block line body) = show line : "block" : output_node_list body
output_node (New line type_id) = show line : "new" : output_node type_id
output_node (Isvoid line x) = show line : "isvoid" : output_node x
output_node (Plus line x y) = show line : "plus" : output_nodes [x, y]
output_node (Minus line x y) = show line : "minus" : output_nodes [x, y]
output_node (Times line x y) = show line : "times" : output_nodes [x, y]
output_node (Divide line x y) = show line : "divide" : output_nodes [x, y]
output_node (LessThan line x y) = show line : "lt" : output_nodes [x, y]
output_node (LessEqual line x y) = show line : "le" : output_nodes [x, y]
output_node (Equal line x y) = show line : "eq" : output_nodes [x, y]
output_node (Not line x) = show line : "not" : output_node x
output_node (Negate line x) = show line : "negate" : output_node x
output_node (IntL line x) = [show line, "int" , show x]
output_node (StringL line x) = [show line, "string", x]
output_node (BoolL line x) = [show line, "bool", if x then "true" else "false"]
output_node (Identifier line x) = show line : output_node x
output_node (Let line bindings body) = show line : "let" : output_node_list bindings ++ output_node body
output_node (Case line e case_elements) = show line : "case" : output_node e ++ output_node_list case_elements
output_node (LetBinding var type_id Nothing) = "let_binding_no_init" : output_nodes [var, type_id]
output_node (LetBinding var type_id (Just initial)) = "let_binding_no_init" : output_nodes [var, type_id, initial]
output_node (CaseElement var type_id expr) = output_nodes [var, type_id, expr]

output_class_map :: Map String [Attr] -> [String]
output_class_map class_map =
    "class_map" : show (Map.size class_map) : foldl (\acc (class_name, attributes) -> acc ++ [class_name, show (length attributes)] ++ output_attributes attributes) [] (Map.toList class_map)
    where output_attributes :: [Attr] -> [String]
          output_attributes attributes = foldl (\acc attribute -> acc ++ output_attribute attribute) [] attributes
          
          output_attribute :: Attr -> [String]
          output_attribute (Attr attr_name type_name (Just initial)) = "initializer" : attr_name : type_name : output_node initial
          output_attribute (Attr attr_name type_name Nothing) = ["no_initializer", attr_name, type_name]

print_parent_map :: Map String String -> Handle -> IO [()]
print_parent_map parent_map outfp = do
    hPutStrLn outfp "parent_map"
    hPutStrLn outfp (show (Map.size parent_map))
    mapM (\(x,y) -> hPutStrLn outfp (x ++ "\n" ++ y)) (Map.toList parent_map)

report_error :: Err -> IO b
report_error (Err line_num msg) = do
    printf "ERROR: %d: Type-Check: %s\n" line_num msg
    exitFailure

main = do
    args <- getArgs
    let input_filename = head args
    let output_filename = take ((length input_filename) - 4) input_filename ++ "-type"
    infp <- openFile input_filename ReadMode
    inputs <- hGetContents infp

    let content = lines inputs
        ast = build_ast content

    -- Constructs a set of class names
    let class_ids_p = create_class_ids ast
    if isRight class_ids_p
       then report_error $ getRight class_ids_p
       else return ()
    let Left class_ids = class_ids_p

    -- Constructs the parent map
    let parent_map_p = create_parent_map class_ids ast
    if isRight parent_map_p
       then report_error $ getRight parent_map_p
       else return()
    let Left parent_map = parent_map_p
    
    -- Check if parent map contains cycles
    if has_cycle class_ids parent_map
       then report_error $ Err 0 "inheritance cycle"
       else return()

    let class_map_p = create_class_map ast ast parent_map
    if isRight class_map_p
       then report_error $ getRight class_map_p
       else return()
    let Left class_map = class_map_p
        
    -- Emit file
    outfp <- openFile output_filename WriteMode
    let class_map_output = output_class_map class_map
    mapM (\line -> hPutStrLn outfp line) class_map_output
    
    hClose infp
    hClose outfp