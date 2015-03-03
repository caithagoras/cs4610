import System.Environment
import System.Exit
import System.IO
import Text.Printf
import Debug.Trace

import Data.List
import Data.Map (Map)
import qualified Data.List as List
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
data Imp = Imp String [(String, String)] String String (Maybe Node)
                                                        -- method_name, formals (name, type), return_type, owner, body

init_class_map = Map.fromList [("Bool", []), 
                               ("IO", []),
                               ("Int", []),
                               ("Object", []),
                               ("String", [])
                              ]
init_imp_map = Map.fromList [("Object", [Imp "abort"         []                  "Object"    "Object"       Nothing,
                                         Imp "copy"          []                  "SELF_TYPE" "Object"       Nothing,
                                         Imp "type_name"     []                  "String"    "Object"       Nothing
                                        ]),
                             ("Bool",   [Imp "abort"         []                  "Object"    "Object"       Nothing,
                                         Imp "copy"          []                  "SELF_TYPE" "Object"       Nothing,
                                         Imp "type_name"     []                  "String"    "Object"       Nothing
                                        ]),
                             ("Int",    [Imp "abort"         []                  "Object"    "Object"       Nothing,
                                         Imp "copy"          []                  "SELF_TYPE" "Object"       Nothing,
                                         Imp "type_name"     []                  "String"    "Object"       Nothing
                                        ]),
                             ("IO",     [Imp "abort"         []                  "Object"    "Object"       Nothing,
                                         Imp "copy"          []                  "SELF_TYPE" "Object"       Nothing,
                                         Imp "type_name"     []                  "String"    "Object"       Nothing,
                                         Imp "in_int"        []                  "Int"       "IO"           Nothing,
                                         Imp "in_string"     []                  "String"    "IO"           Nothing,
                                         Imp "out_int"       [("x", "Int")]      "SELF_TYPE" "IO"           Nothing,
                                         Imp "out_string"    [("x", "String")]   "SELF_TYPE" "IO"           Nothing
                                        ]),
                             ("String", [Imp "abort"         []                  "Object"    "Object"       Nothing,
                                         Imp "copy"          []                  "SELF_TYPE" "Object"       Nothing,
                                         Imp "type_name"     []                  "String"    "Object"       Nothing,
                                         Imp "concat"        [("s", "String")]   "String"    "String"       Nothing,
                                         Imp "length"        []                  "Int"       "String"       Nothing,
                                         Imp "substr"        [("i", "Int"), ("l", "Int")]
                                                                                 "String"    "String"       Nothing
                                        ])
                            ]
                                                        
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

get_class :: String -> [Node] -> Node
get_class class_name ast =
    head $ filter (\(Class (Id name _) _ _) -> name == class_name) ast

convert_formals :: [Node] -> [(String, String)]
convert_formals formal_list = foldr (\(Formal (Id formal_name _) (Id type_name _)) acc -> (formal_name, type_name):acc) [] formal_list

create_class_ids :: [Node] -> Either [String] Err
create_class_ids ast = class_ids
    where class_ids = foldl update_class_ids init_class_ids ast
          init_class_ids = Left ["Bool", "IO", "Int", "Object", "String"]

          update_class_ids :: Either [String] Err -> Node -> Either [String] Err
          update_class_ids acc@(Right _) _ = acc
          update_class_ids acc@(Left class_ids) (Class (Id class_name class_line) _ _) 
              | elem class_name class_ids = Right $ Err class_line $ printf "class %s redefined" class_name
              | class_name == "SELF_TYPE" = Right $ Err class_line "class named SELF_TYPE"
              | otherwise =Left $ class_name:class_ids

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
has_cycle class_ids parent_map = any (\(class_name) -> found_cycle class_name) class_ids
    where found_cycle :: String -> Bool
          found_cycle class_name = found_cycle_rec class_name []
          
          found_cycle_rec :: String -> [String] -> Bool
          found_cycle_rec current path
              | current == "Object" = False
              | elem current path = True
              | otherwise = found_cycle_rec next (current:path)
              where next = Maybe.fromJust $ Map.lookup current parent_map

create_class_map :: [Node] -> [String] -> Map String String -> Either (Map String [Attr]) Err
create_class_map ast class_ids parent_map = foldl add_to_class_map (Left init_class_map) ast          
    where add_to_class_map :: Either (Map String [Attr]) Err -> Node -> Either (Map String [Attr]) Err
          add_to_class_map acc@(Right _) _ = acc
          add_to_class_map acc@(Left class_map) (Class (Id class_name _) _ feature_list)
              | Map.member class_name class_map = acc
              | otherwise = 
                  let parent_name = Maybe.fromJust $ Map.lookup class_name parent_map
                      parent_added = Map.member parent_name class_map
                      class_map' = (if parent_added then acc else add_to_class_map acc (get_class parent_name ast))
                  in if isRight class_map'
                        then class_map'
                        else let parent_attributes = Maybe.fromJust $ Map.lookup parent_name $ getLeft class_map'
                                 class_attributes = foldl add_to_attribute_list (Left parent_attributes) feature_list
                             in if isRight class_attributes
                                   then Right $ getRight class_attributes
                                   else Left $ Map.insert class_name (getLeft class_attributes) (getLeft class_map')

              where add_to_attribute_list :: Either [Attr] Err -> Node -> Either [Attr] Err
                    add_to_attribute_list acc@(Right _) _ = acc
                    add_to_attribute_list acc (Method _ _ _ _) = acc
                    add_to_attribute_list (Left attributes) (Attribute (Id attr_name attr_line) (Id type_name type_line) optional_init)
                        | any (\(Attr x _ _) -> x == attr_name) attributes = Right $ Err attr_line $ printf "class %s redefines attribute %s" class_name attr_name
                        | attr_name == "self" = Right $ Err attr_line $ printf "class %s has an attribute named self" class_name
                        | not (elem type_name ("SELF_TYPE":class_ids)) = Right $ Err type_line $ printf "class %s has attribute %s with unknown type %s" class_name attr_name type_name
                        | otherwise = Left $ attributes ++ [Attr attr_name type_name optional_init]

create_imp_map :: [Node] -> [String] -> Map String String -> Either (Map String [Imp]) Err
create_imp_map ast class_ids parent_map = foldl add_to_imp_map (Left init_imp_map) ast
    where add_to_imp_map :: Either (Map String [Imp]) Err -> Node -> Either (Map String [Imp]) Err
          add_to_imp_map acc@(Right _) _ = acc
          add_to_imp_map acc@(Left imp_map) (Class (Id class_name _) _ feature_list)
              | Map.member class_name imp_map = acc
              | otherwise =
                  let parent_name = Maybe.fromJust $ Map.lookup class_name parent_map
                      parent_added = Map.member parent_name imp_map
                      imp_map' = (if parent_added then acc else add_to_imp_map acc (get_class parent_name ast))
                  in if isRight imp_map'
                        then imp_map'
                        else let parent_imp = Maybe.fromJust $ Map.lookup parent_name $ getLeft imp_map'
                                 class_imp = foldl add_to_imp_list (Left parent_imp) feature_list
                             in if isRight class_imp
                                   then Right $ getRight class_imp
                                   else Left $ Map.insert class_name (getLeft class_imp) (getLeft imp_map')

              where add_to_imp_list :: Either [Imp] Err -> Node -> Either [Imp] Err
                    add_to_imp_list acc@(Right _) _ = acc
                    add_to_imp_list acc (Attribute _ _ _) = acc
                    add_to_imp_list (Left imp_list) (Method (Id method_name method_line) formal_list (Id type_name type_line) body) =
                        let check_method :: Maybe Err
                            check_method
                                | not (elem type_name ("SELF_TYPE":class_ids)) = Just $ Err type_line $ printf "class %s has method %s with unknown return type %s" class_name method_name type_name
                                | otherwise = check_formal_list formal_list
                                where check_formal_list :: [Node] -> Maybe Err
                                      check_formal_list formal_list = check_formal_list_rec formal_list []
                                        
                                      check_formal_list_rec :: [Node] -> [String] -> Maybe Err
                                      check_formal_list_rec [] _ = Nothing
                                      check_formal_list_rec ((Formal (Id formal_name formal_line) (Id type_name type_line)):xs) defined
                                         | elem formal_name defined = Just $ Err formal_line $ printf "class %s has method %s with duplicate formal parameter named %s" class_name method_name formal_name
                                         | formal_name == "self" = Just $ Err formal_line $ printf "class %s has method %s with formal parameter named self" class_name method_name
                                         | not (elem type_name class_ids) = Just $ Err type_line $ printf "class %s has method %s with formal parameter of unknown type %s" class_name method_name type_name
                                         | otherwise = check_formal_list_rec xs (formal_name:defined)

                            method_error = check_method
                            repeated = any (\(Imp x _ _ _ _) -> x == method_name) imp_list
                        in if repeated
                              then let overriden@(Imp _ _ _ owner _) = head $ filter (\(Imp x _ _ _ _) -> x == method_name) imp_list
                                   in if owner == class_name
                                         then Right $ Err method_line $ printf "class %s redefines method %s" class_name method_name
                                         else if Maybe.isJust method_error
                                            then Right $ Maybe.fromJust method_error 
                                            else let check_override :: Imp -> Maybe Err
                                                     check_override (Imp _ o_formals o_type _ _)
                                                         | o_type /= type_name = Just $ Err type_line $ printf "class %s redefines method %s and changes return type (from %s to %s)" class_name method_name o_type type_name
                                                         | length o_formals /= length formal_list = Just $ Err method_line $ printf "class %s redefines method %s and changes number of formals" class_name method_name
                                                         | otherwise = check_formal_compatibility o_formals formal_list
                                                         where check_formal_compatibility :: [(String, String)] -> [Node] -> Maybe Err
                                                               check_formal_compatibility [] [] = Nothing
                                                               check_formal_compatibility ((_, o_type):oxs) (Formal (Id n_name _) (Id n_type n_type_line):nxs)
                                                                   | o_type /= n_type = Just $ Err n_type_line $ printf "class %s redefines method %s and changes type of formal %s" class_name method_name n_name
                                                                   | otherwise = check_formal_compatibility oxs nxs
                                                     override_error = check_override overriden
                                                 in if Maybe.isJust override_error
                                                       then Right $ Maybe.fromJust override_error
                                                       else let im = Maybe.fromJust $ List.findIndex (\(Imp x _ _ _ _) -> x == method_name) imp_list
                                                            in Left $ take im imp_list ++ [Imp method_name (convert_formals formal_list) type_name class_name (Just body)]
                                                            ++ drop (im + 1) imp_list
                              else if Maybe.isJust method_error
                                 then Right $ Maybe.fromJust method_error
                                 else Left $ imp_list ++ [Imp method_name (convert_formals formal_list) type_name class_name (Just body)]

check_main :: Map String [Imp] -> Maybe Err
check_main imp_map
    | not $ Map.member "Main" imp_map = Just $ Err 0 "class Main not found"
    | not $ any (\(Imp name _ _ _ _) -> name == "main") main_imp = Just $ Err 0 "class Main method main not found"
    | not $ any (\(Imp name formal _ _ _) -> name == "main" && length formal == 0) main_imp = Just $ Err 0 "class Main method main with 0 parameters not found"
    | otherwise = Nothing
    where main_imp = Maybe.fromJust $ Map.lookup "Main" imp_map
                                 
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
output_node (IntL line x) = [show line, "integer" , show x]
output_node (StringL line x) = [show line, "string", x]
output_node (BoolL line x) = [show line, if x then "true" else "false"]
output_node (Identifier line x) = show line : "identifier" : output_node x
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

output_imp_map :: Map String [Imp] -> [String]
output_imp_map imp_map =
    "implementation_map" : show (Map.size imp_map) : foldl (\acc (class_name, imp_list) -> acc ++ [class_name, show (length imp_list)] ++ output_imp_list imp_list) [] (Map.toList imp_map)
    where output_imp_list :: [Imp] -> [String]
          output_imp_list imp_list = foldl (\acc imp -> acc ++ output_imp imp) [] imp_list
          
          output_imp :: Imp -> [String]
          output_imp (Imp method_name formals return_type owner body) = (method_name : show (length formals) : formal_names) ++ (owner : body_expr)
              where formal_names = map (\(name, _) -> name) formals
                    body_expr = if Maybe.isJust body
                                   then output_node $ Maybe.fromJust body
                                   else ["0", return_type, "internal", (owner ++ "." ++ method_name)]

output_parent_map :: Map String String -> [String]
output_parent_map parent_map = "parent_map" : show (Map.size parent_map) : foldr (\(x, y) acc-> x:y:acc) [] (Map.toList parent_map)

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

    -- Constructs the set of class names
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

    -- Constructs the class map
    let class_map_p = create_class_map ast class_ids parent_map
    if isRight class_map_p
       then report_error $ getRight class_map_p
       else return()
    let Left class_map = class_map_p
        
    -- Constructs the implementation map
    let imp_map_p = create_imp_map ast class_ids parent_map
    if isRight imp_map_p
       then report_error $ getRight imp_map_p
       else return ()
    let Left imp_map = imp_map_p
    
    -- Check main() method in Main class
    let no_main_error = check_main imp_map
    if Maybe.isJust no_main_error
       then report_error $ Maybe.fromJust no_main_error
       else return()

    -- Emit file
    outfp <- openFile output_filename WriteMode
    let class_map_output = output_class_map class_map
    --let imp_map_output = output_imp_map imp_map
    mapM (\line -> hPutStrLn outfp line) class_map_output
    
    hClose infp
    hClose outfp
