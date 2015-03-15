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

data Node = Class Node (Maybe Node) [Node]                    -- class_name, super_class, feature_list
          | Id String Int                                     -- id, line_num
          | Attribute Node Node (Maybe Node)                  -- id, type, [init]
          | Method Node [Node] Node Node                      -- id, formal_list, type, body
          | Formal Node Node                                  -- id, type
                                               -- Expressions
          | Assign Int Node Node String                       -- line_num, lhs, rhs
          | DynamicDispatch Int Node Node [Node] String       -- line_num, e, method, args
          | StaticDispatch Int Node Node Node [Node] String   -- line_num, e, type, method, args
          | SelfDispatch Int Node [Node] String               -- line_num, method, args
          | If Int Node Node Node String                      -- line_num, predicate, then_expr, else_expr
          | While Int Node Node String                        -- line_num, predicate, body
          | Block Int [Node] String                           -- line_num, body
          | New Int Node String                               -- line_num, type_id
          | Isvoid Int Node String                            -- line_num, x
          | Plus Int Node Node String                         -- line_num, x, y
          | Minus Int Node Node String                        -- line_num, x, y
          | Times Int Node Node String                        -- line_num, x, y
          | Divide Int Node Node String                       -- line_num, x, y
          | LessThan Int Node Node String                     -- line_num, x, y
          | LessEqual Int Node Node String                    -- line_num, x, y
          | Equal Int Node Node String                        -- line_num, x, y
          | Not Int Node String                               -- line_num, x
          | Negate Int Node String                            -- line_num, x
          | IntL Int Int String                               -- line_num, value
          | StringL Int String String                         -- line_num, value
          | BoolL Int Bool String                             -- line_num, truth
          | Identifier Int Node String                        -- line_num, id
          | Let Int [Node] Node String                        -- line_num, bindings, body
          | Case Int Node [Node] String                       -- line_num, expr, case_elements
          | LetBinding Node Node (Maybe Node)                 -- var, type, [init]
          | CaseElement Node Node Node                        -- var, type, expr
          
data Attr = Attr String String (Maybe Node)                             -- name, type, init
data Err = Err Int String                                               -- line_num, error_message
data Imp = Imp String [(String, String)] String String (Maybe Node)     -- method_name, formals (name, type), return_type, owner, body

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

detuplize :: [(a,b)] -> ([a], [b])
detuplize list = foldl (\(acc_x, acc_y) (x, y) -> (acc_x ++ [x], acc_y ++ [y])) ([], []) list

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
build_expr (line:"assign":content) = (Assign line_num lhs rhs "", content'')
    where (lhs,content') = build_id content
          (rhs, content'') = build_expr content'
          line_num = read line :: Int

build_expr (line:"dynamic_dispatch":content) = (DynamicDispatch line_num e method args "", content''')
    where (e, content') = build_expr content
          (method, content'') = build_id content'
          (args, content''') = build_list build_expr content''
          line_num = read line :: Int

build_expr (line:"static_dispatch":content) = (StaticDispatch line_num e type_id method args "", content'''')
    where (e, content') = build_expr content
          (type_id, content'') = build_id content'
          (method, content''') = build_id content''
          (args, content'''') = build_list build_expr content'''
          line_num = read line :: Int

build_expr (line:"self_dispatch":content) = (SelfDispatch line_num method args "", content'')
    where (method, content') = build_id content
          (args, content'') = build_list build_expr content'
          line_num = read line :: Int

build_expr (line:"if":content) = (If line_num predicate then_expr else_expr "", content''')
    where (predicate, content') = build_expr content
          (then_expr, content'') = build_expr content'
          (else_expr, content''') = build_expr content''
          line_num = read line :: Int

build_expr (line:"while":content) = (While line_num predicate body "", content'')
    where (predicate, content') = build_expr content
          (body, content'') = build_expr content'
          line_num = read line :: Int

build_expr (line:"block":content) = (Block line_num body "", content')
    where (body, content') = build_list build_expr content
          line_num = read line :: Int

build_expr (line:"new":content) = (New line_num type_id "", content')
    where (type_id, content') = build_id content
          line_num = read line :: Int

build_expr (line:command:content)
    | command == "plus" = (Plus line_num x y "", content'')
    | command == "minus" = (Minus line_num x y "", content'')
    | command == "times" = (Times line_num x y "", content'')
    | command == "divide" = (Divide line_num x y "", content'')
    | command == "lt" = (LessThan line_num x y "", content'')
    | command == "le" = (LessEqual line_num x y "", content'')
    | command == "eq" = (Equal line_num x y "", content'')
    where (x, content') = build_expr content
          (y, content'') = build_expr content'
          line_num = read line :: Int
    
build_expr (line:command:content)
    | command == "isvoid" = (Isvoid line_num x "", content')
    | command == "not" = (Not line_num x "", content')
    | command == "negate" = (Negate line_num x "", content')
    where (x, content') = build_expr content
          line_num = read line :: Int
    
build_expr (line:command:content)
    | command == "integer" = (IntL line_num (read value :: Int) "", content')
    | command == "string" = (StringL line_num value "", content')
    where (value:content') = content
          line_num = read line :: Int

build_expr (line:"identifier":content) = (Identifier line_num ident "", content')
    where (ident, content') = build_id content
          line_num = read line :: Int

build_expr (line:command:content)
    | command == "true" = (BoolL line_num True "", content)
    | command == "false" = (BoolL line_num False "", content)
    where line_num = read line :: Int

build_expr (line:"let":content) = (Let line_num bindings body "", content'')
    where (bindings, content') = build_list build_let_binding content
          (body, content'') = build_expr content'
          line_num = read line :: Int

build_expr (line:"case":content) = (Case line_num expr case_elements "", content'')
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

create_type_environment :: [Attr] -> Map String String
create_type_environment attributes = foldl (\acc (Attr name declared_type _) -> Map.insert name declared_type acc) Map.empty attributes

bind :: Map String String -> [Node] -> Map String String
bind o nodes = foldl bind' o nodes

bind' :: Map String String -> Node -> Map String String
bind' acc (Formal (Id name _) (Id type_name _)) = Map.insert name type_name acc

conform :: Map String String -> String -> String -> String -> Bool
conform p c "SELF_TYPE" "SELF_TYPE" = True
conform p c "SELF_TYPE" t = conform' p c t
conform p c t "SELF_TYPE" = False
conform p c t t' = conform' p t t'

conform' :: Map String String -> String -> String -> Bool
conform' p sub "Object" = True
conform' p "Object" super = False
conform' p sub super
    | sub == super = True
    | otherwise = conform' p (Maybe.fromJust (Map.lookup sub p)) super

conform_parameters :: Int -> Map String String -> String -> [String] -> [String] -> Maybe Err
conform_parameters line p c actuals formals = conform_parameters' actuals formals 1
    where conform_parameters' :: [String] -> [String] -> Int -> Maybe Err
          conform_parameters' [] [] _ = Nothing
          conform_parameters' (actual:actuals) (formal:formals) arg_num
              | not $ conform p c actual formal = Just $ Err line $ printf "argument #%d type %s does not conform to formal type %s" arg_num actual formal
              | otherwise = conform_parameters' actuals formals (arg_num + 1)

lub :: Map String String -> String -> String -> String -> String
lub p c "SELF_TYPE" "SELF_TYPE" = "SELF_TYPE"
lub p c "SELF_TYPE" t = lub' p c t
lub p c t "SELF_TYPE" = lub' p c t
lub p c t t' = lub' p t t'

lub':: Map String String -> String -> String -> String
lub' p t1 t2 =
    let get_root_path :: Map String String -> String -> [String]
        get_root_path p "Object" = ["Object"]
        get_root_path p t = t : (get_root_path p $ Maybe.fromJust $ Map.lookup t p)
        
        path1 = get_root_path p t1
        path2 = get_root_path p t2
    in head $ filter (\c -> elem c path1) path2

lub_list :: Map String String -> String -> [String] -> String
lub_list p c [t] = t
lub_list p c (t:xs) = lub p c t $ lub_list p c xs

type_check_list :: Map String String -> Map String [Imp] -> String -> Map String String -> [Node] -> Either ([Node], [String]) Err
type_check_list o m c p nodes = foldl type_check' (Left ([], [])) nodes
    where type_check' :: Either ([Node], [String]) Err -> Node -> Either ([Node], [String]) Err
          type_check' acc@(Right _) _ = acc
          type_check' (Left (node_list, type_list)) node =
              let node' = type_check o m c p node
                  Left (annotated_node, node_type) = node'
              in if isRight node'
                    then Right $ getRight node'
                    else Left (node_list ++ [annotated_node], type_list ++ [node_type])

type_check :: Map String String -> Map String [Imp] -> String -> Map String String -> Node -> Either (Node, String) Err
type_check o m c p (Id name line)
    | name == "self" = Left (Id name line, "SELF_TYPE")
    | Map.member name o = Left (Id name line, Maybe.fromJust (Map.lookup name o))
    | otherwise = Right $ Err line $ printf "unbound identifier %s" name

type_check o m c p (Assign line lhs rhs _)
    | isRight lhs' = lhs'
    | isRight rhs' = rhs'
    | not (conform p c lhs_type rhs_type) = Right $ Err line $ printf "%s does not conform to %s in assignment" (output_type' c rhs_type) (output_type' c lhs_type)
    | otherwise = Left (Assign line lhs_node rhs_node rhs_type, rhs_type)
    where lhs' = type_check o m c p lhs
          rhs' = type_check o m c p rhs
          Left (lhs_node, lhs_type) = lhs'
          Left (rhs_node, rhs_type) = rhs'

type_check o m c p (DynamicDispatch line recv method_node@(Id method_name method_line) args _)
    | isRight recv' = recv'
    | isRight args' = Right $ getRight args'
    | not method_exists = Right $ Err method_line $ printf "unknown method %s in dispatch on %s" method_name t0'
    | length arg_type_list /= length formal_type_list = Right $ Err line $ printf "wrong number of actual arguments (%d vs. %d)" (length arg_type_list) (length formal_type_list)
    | Maybe.isJust parameter_result = Right $ Maybe.fromJust parameter_result
    | otherwise = Left (DynamicDispatch line recv_node method_node arg_node_list return_type', return_type')
    where recv' = type_check o m c p recv
          args' = type_check_list o m c p args
          Left (recv_node, recv_type) = recv'
          Left (arg_node_list, arg_type_list) = args'
          t0' = if recv_type == "SELF_TYPE" then c else recv_type
          method_found = filter (\(Imp x _ _ _ _) -> x == method_name) $ Maybe.fromJust $ Map.lookup t0' m
          method_exists = length method_found > 0
          Imp _ formals return_type _ _ = head $ method_found
          formal_type_list = map (\(formal_name, formal_type) -> formal_type) formals
          parameter_result = conform_parameters line p c arg_type_list formal_type_list
          return_type' = if return_type == "SELF_TYPE" then recv_type else return_type

type_check o m c p (StaticDispatch line recv f3@(Id t _) method_node@(Id method_name method_line) args _)
    | isRight recv' = recv'
    | isRight args' = Right $ getRight args'
    | not $ conform p c recv_type t =Right $ Err line $ printf "%s does not conform to %s in static dispatch" (output_type' c recv_type) t
    | not method_exists = Right $ Err method_line $ printf "unknown method %s in dispatch on %s" method_name t
    | length arg_type_list /= length formal_type_list = Right $ Err line $ printf "wrong number of actual arguments (%d vs. %d)" (length arg_type_list) (length formal_type_list)
    | Maybe.isJust parameter_result = Right $ Maybe.fromJust parameter_result
    | otherwise = Left (StaticDispatch line recv_node f3 method_node arg_node_list return_type', return_type')
    where recv' = type_check o m c p recv
          args' = type_check_list o m c p args
          Left (recv_node, recv_type) = recv'
          Left (arg_node_list, arg_type_list) = args'
          method_found = filter (\(Imp x _ _ _ _) -> x == method_name) $ Maybe.fromJust $ Map.lookup t m
          method_exists = length method_found > 0
          Imp _ formals return_type _ _ = head $ method_found
          formal_type_list = map (\(formal_name, formal_type) -> formal_type) formals
          parameter_result = conform_parameters line p c arg_type_list formal_type_list
          return_type' = if return_type == "SELF_TYPE" then recv_type else return_type

type_check o m c p (SelfDispatch line method_node@(Id method_name method_line) args _)
    | isRight args' = Right $ getRight args'
    | not method_exists = Right $ Err method_line $ printf "unknown method %s in dispatch on %s" method_name c
    | length arg_type_list /= length formal_type_list = Right $ Err line $ printf "wrong number of actual arguments (%d vs. %d)" (length arg_type_list) (length formal_type_list)
    | Maybe.isJust parameter_result = Right $ Maybe.fromJust parameter_result
    | otherwise = Left (SelfDispatch line method_node arg_node_list return_type, return_type)
    where args' = type_check_list o m c p args
          Left (arg_node_list, arg_type_list) = args'
          method_found = filter (\(Imp x _ _ _ _) -> x == method_name) $ Maybe.fromJust $ Map.lookup c m
          method_exists = length method_found > 0
          Imp _ formals return_type _ _ = head $ method_found
          formal_type_list = map (\(formal_name, formal_type) -> formal_type) formals
          parameter_result = conform_parameters line p c arg_type_list formal_type_list
          
type_check o m c p (If line predicate then_expr else_expr _)
    | isRight predicate' = predicate'
    | isRight then_expr' = then_expr'
    | isRight else_expr' = else_expr'
    | predicate_type /= "Bool" = Right $ Err line $ printf "conditional has type %s instead of Bool" predicate_type
    | otherwise = Left (If line predicate_node then_expr_node else_expr_node conditional_type, conditional_type)
    where predicate' = type_check o m c p predicate
          then_expr' = type_check o m c p then_expr
          else_expr' = type_check o m c p else_expr
          Left (predicate_node, predicate_type) = predicate'
          Left (then_expr_node, then_expr_type) = then_expr'
          Left (else_expr_node, else_expr_type) = else_expr'
          conditional_type = lub p c then_expr_type else_expr_type

type_check o m c p (While line predicate body _)
    | isRight predicate' = predicate'
    | isRight body' = body'
    | predicate_type /= "Bool" = Right $ Err line $ printf "predicate has type %s instead of Bool" predicate_type
    | otherwise = Left (While line predicate_node body_node "Object", "Object")
    where predicate' = type_check o m c p predicate
          body' = type_check o m c p body
          Left (predicate_node, predicate_type) = predicate'
          Left (body_node, body_type) = body'

type_check o m c p (Block line nodes _) =
    let block_result = type_check_list o m c p nodes
        Left (node_list, type_list) = block_result
        block_type = last type_list
    in if isRight block_result
          then Right $ getRight block_result
          else Left (Block line node_list block_type, block_type)

type_check o m c p (New line f2@(Id type_name _) _)
    | type_name == "SELF_TYPE" = Left (New line f2 "SELF_TYPE", "SELF_TYPE")
    | Map.member type_name m = Left (New line f2 type_name, type_name)
    | otherwise = Right $ Err line $ printf "unknown type %s" type_name

type_check o m c p (Isvoid line x _)
    | isRight x' = x'
    | otherwise = Left (Isvoid line x_node "Bool", "Bool")
    where x' = type_check o m c p x
          Left (x_node, x_type) = x'

type_check o m c p (Plus line lhs rhs _)
    | isRight lhs' = lhs'
    | isRight rhs' = rhs'
    | lhs_type == "Int" && rhs_type == "Int" = Left (Plus line lhs_node rhs_node "Int", "Int")
    | otherwise = Right $ Err line $ printf "arithmetic on %s %s instead of Ints" (output_type' c lhs_type) (output_type' c rhs_type)
    where lhs' = type_check o m c p lhs
          rhs' = type_check o m c p rhs
          Left (lhs_node, lhs_type) = lhs'
          Left (rhs_node, rhs_type) = rhs'

type_check o m c p (Minus line lhs rhs _)
    | isRight lhs' = lhs'
    | isRight rhs' = rhs'
    | lhs_type == "Int" && rhs_type == "Int" = Left (Minus line lhs_node rhs_node "Int", "Int")
    | otherwise = Right $ Err line $ printf "arithmetic on %s %s instead of Ints" (output_type' c lhs_type) (output_type' c rhs_type)
    where lhs' = type_check o m c p lhs
          rhs' = type_check o m c p rhs
          Left (lhs_node, lhs_type) = lhs'
          Left (rhs_node, rhs_type) = rhs'

type_check o m c p (Times line lhs rhs _)
    | isRight lhs' = lhs'
    | isRight rhs' = rhs'
    | lhs_type == "Int" && rhs_type == "Int" = Left (Times line lhs_node rhs_node "Int", "Int")
    | otherwise = Right $ Err line $ printf "arithmetic on %s %s instead of Ints" (output_type' c lhs_type) (output_type' c rhs_type)
    where lhs' = type_check o m c p lhs
          rhs' = type_check o m c p rhs
          Left (lhs_node, lhs_type) = lhs'
          Left (rhs_node, rhs_type) = rhs'

type_check o m c p (Divide line lhs rhs _)
    | isRight lhs' = lhs'
    | isRight rhs' = rhs'
    | lhs_type == "Int" && rhs_type == "Int" = Left (Divide line lhs_node rhs_node "Int", "Int")
    | otherwise = Right $ Err line $ printf "arithmetic on %s %s instead of Ints" (output_type' c lhs_type) (output_type' c rhs_type)
    where lhs' = type_check o m c p lhs
          rhs' = type_check o m c p rhs
          Left (lhs_node, lhs_type) = lhs'
          Left (rhs_node, rhs_type) = rhs'

type_check o m c p (LessThan line lhs rhs _)
    | isRight lhs' = lhs'
    | isRight rhs' = rhs'
    | (elem lhs_type ["Int", "String", "Bool"] || elem rhs_type ["Int", "String", "Bool"]) && lhs_type /= rhs_type = Right $ Err line $ printf "comparison between %s and %s" (output_type' c lhs_type) (output_type' c rhs_type)
    | otherwise = Left (LessThan line lhs_node rhs_node "Bool", "Bool")
    where lhs' = type_check o m c p lhs
          rhs' = type_check o m c p rhs
          Left (lhs_node, lhs_type) = lhs'
          Left (rhs_node, rhs_type) = rhs'

type_check o m c p (LessEqual line lhs rhs _)
    | isRight lhs' = lhs'
    | isRight rhs' = rhs'
    | (elem lhs_type ["Int", "String", "Bool"] || elem rhs_type ["Int", "String", "Bool"]) && lhs_type /= rhs_type = Right $ Err line $ printf "comparison between %s and %s" (output_type' c lhs_type) (output_type' c rhs_type)
    | otherwise = Left (LessEqual line lhs_node rhs_node "Bool", "Bool")
    where lhs' = type_check o m c p lhs
          rhs' = type_check o m c p rhs
          Left (lhs_node, lhs_type) = lhs'
          Left (rhs_node, rhs_type) = rhs'

type_check o m c p (Equal line lhs rhs _)
    | isRight lhs' = lhs'
    | isRight rhs' = rhs'
    | (elem lhs_type ["Int", "String", "Bool"] || elem rhs_type ["Int", "String", "Bool"]) && lhs_type /= rhs_type = Right $ Err line $ printf "comparison between %s and %s" (output_type' c lhs_type) (output_type' c rhs_type)
    | otherwise = Left (Equal line lhs_node rhs_node "Bool", "Bool")
    where lhs' = type_check o m c p lhs
          rhs' = type_check o m c p rhs
          Left (lhs_node, lhs_type) = lhs'
          Left (rhs_node, rhs_type) = rhs'

type_check o m c p (Not line x _)
    | isRight x' = x'
    | x_type == "Bool" = Left (Not line x_node "Bool", "Bool")
    | otherwise = Right $ Err line $ printf "not applied to type %s instead of Bool" (output_type' c x_type)
    where x' = type_check o m c p x
          Left (x_node, x_type) = x'

type_check o m c p (Negate line x _)
    | isRight x' = x'
    | x_type == "Int" = Left (Negate line x_node "Int", "Int")
    | otherwise = Right $ Err line $ printf "not applied to type %s instead of Int" (output_type' c x_type)
    where x' = type_check o m c p x
          Left (x_node, x_type) = x'

type_check o m c p (Identifier line ident _)
    | isRight ident' = ident'
    | otherwise = Left (Identifier line ident_node ident_type, ident_type)
    where ident' = type_check o m c p ident
          Left (ident_node, ident_type) = ident'

type_check o m c p (IntL line value _) = Left (IntL line value "Int", "Int")
type_check o m c p (StringL line value _) = Left (StringL line value "String", "String")
type_check o m c p (BoolL line value _) = Left (BoolL line value "Bool", "Bool")

type_check o m c p (Let line bindings body _) =
    let bindings' = foldl process_let_binding (Left (o, [])) bindings
        Left (o', bindings_node) = bindings'
        body' = type_check o' m c p body
        Left (body_node, body_type) = body'
        
        process_let_binding :: Either (Map String String, [Node]) Err -> Node -> Either (Map String String, [Node]) Err
        process_let_binding acc@(Right _) _ = acc
        process_let_binding (Left (o, bindings)) binding@(LetBinding f1@(Id var_name var_line) f2@(Id declared_type type_line) initial)
            | var_name == "self" = Right $ Err var_line $ printf "binding self in a let is not allowed"
            | not type_is_defined = Right $ Err type_line $ printf "unknown type %s" declared_type
            | Maybe.isNothing initial = Left (Map.insert var_name declared_type o, bindings ++ [binding])
            | isRight initial' = Right $ getRight initial'
            | not (conform p c initial_type declared_type) = Right $ Err line $ printf "initializer type %s does not conform to type %s" (output_type' c initial_type) (output_type' c declared_type)
            | otherwise = Left (Map.insert var_name declared_type o, bindings ++ [LetBinding f1 f2 (Just initial_node)])
            where type_is_defined = (declared_type == "SELF_TYPE" || Map.member declared_type m)
                  initial' = type_check o m c p $ Maybe.fromJust initial
                  Left (initial_node, initial_type) = initial'
    in if isRight bindings'
          then Right $ getRight bindings'
          else if isRight body'
             then Right $ getRight body'
             else Left (Let line bindings_node body_node body_type, body_type)

type_check o m c p (Case line expr elements _)
    | isRight expr' = expr'
    | isRight elements' = Right $ getRight elements'
    | otherwise = Left (Case line expr_node nodes case_type, case_type)
    where expr' = type_check o m c p expr
          Left (expr_node, _) = expr'
          elements' = foldl process_element (Left ([], [], [])) elements
          Left (nodes, expr_types, _) = elements'
          case_type = lub_list p c expr_types

          process_element :: Either ([Node], [String], [String]) Err -> Node -> Either ([Node], [String], [String]) Err
          process_element acc@(Right _) _ = acc
          process_element (Left (nodes, expr_types, bound_types)) (CaseElement var@(Id var_name var_line) type_id@(Id type_name type_line) expr)
              | var_name == "self" = Right $ Err var_line "binding self in a case expression is not allowed"
              | type_name == "SELF_TYPE" = Right $ Err type_line "using SELF_TYPE as a case branch type is not allowed"
              | not $ Map.member type_name m = Right $ Err var_line $ printf "unknown type %s" type_name
              | elem type_name bound_types = Right $ Err var_line $ printf "case branch type %s is bound twice" type_name
              | isRight expr' = Right $ getRight expr'
              | otherwise = Left (nodes ++ [CaseElement var type_id expr_node], expr_types ++ [expr_type], bound_types ++ [type_name])
              where o' = Map.insert var_name type_name o
                    expr' = type_check o' m c p expr
                    Left (expr_node, expr_type) = expr'

annotate_ast :: Map String [Attr] -> Map String [Imp] -> Map String String -> [Node] -> Either [Node] Err
annotate_ast class_map m p ast = foldl annotate_class (Left []) ast
    where annotate_class :: Either [Node] Err -> Node -> Either [Node] Err
          annotate_class acc@(Right _) _ = acc
          annotate_class (Left class_list) (Class f1@(Id c _) f2 feature_list) =
              let annotate_feature :: Either [Node] Err -> Node -> Either [Node] Err
                  annotate_feature acc@(Right _) _ = acc
                  annotate_feature (Left feature_list) feature@(Attribute _ _ Nothing) = Left $ feature_list ++ [feature]
                  annotate_feature (Left feature_list) (Attribute f1@(Id _ attr_line) f2@(Id declared_type _) (Just initial)) =
                      let initial' = type_check o m c p initial
                      in if isRight initial'
                            then Right $ getRight initial'
                            else let Left (initial_node, initial_type) = initial'
                                 in if not $ conform p c initial_type declared_type
                                       then Right $ Err attr_line $ printf "%s does not conform to %s in initialized attribute" (output_type' c initial_type) (output_type' c declared_type)
                                       else Left $ feature_list ++ [Attribute f1 f2 (Just initial_node)]
                  annotate_feature (Left feature_list) (Method f1@(Id method_name method_line) formals f3@(Id return_type _) body) =
                      let o' = bind o formals
                          body' = type_check o' m c p body
                      in if isRight body'
                            then Right $ getRight body'
                            else let Left (body_node, body_type) = body'
                                 in if not $conform p c body_type return_type
                                       then Right $ Err method_line $ printf "%s does not conform to %s in method %s" (output_type' c body_type) (output_type' c return_type) method_name
                                       else Left $ feature_list ++ [Method f1 formals f3 body_node]

                  o = create_type_environment $ Maybe.fromJust $ Map.lookup c class_map
                  feature_list' = foldl annotate_feature (Left []) feature_list
              in if isRight feature_list'
                    then Right $ getRight feature_list'
                    else Left $ class_list ++ [Class f1 f2 (getLeft feature_list')]
                                 
output_nodes :: [Node] -> [String]
output_nodes nodes = foldl (\acc node -> acc ++ output_node node) [] nodes

output_node_list :: [Node] -> [String]
output_node_list nodes = show (length nodes) : output_nodes nodes

output_type' :: String -> String -> String
output_type' class_name "SELF_TYPE" = printf "SELF_TYPE(%s)" class_name
output_type' class_name t = t

output_node :: Node -> [String]
output_node (Class name Nothing features) = output_node name ++ ["no_inherits"] ++ output_node_list features
output_node (Class name (Just super) features) = output_node name ++ ["inherits"] ++ output_node super ++ output_node_list features
output_node (Attribute name declared_type Nothing) = "attribute_no_init" : output_nodes [name, declared_type]
output_node (Attribute name declared_type (Just initial)) = "attribute_init" : output_nodes [name, declared_type, initial]
output_node (Method name formals return_type body) = "method" : output_node name ++ output_node_list formals ++ output_nodes [return_type, body]
output_node (Formal name declared_type) = output_nodes [name, declared_type]
output_node (Id var line) = [show line, var]
output_node (Assign line lhs rhs anno) = show line : anno : "assign" : output_node lhs ++ output_node rhs
output_node (DynamicDispatch line e method args anno) = show line : anno : "dynamic_dispatch" : output_nodes [e, method] ++ output_node_list args
output_node (StaticDispatch line e type_id method args anno) = show line : anno : "static_dispatch" : output_nodes [e, type_id, method] ++ output_node_list args
output_node (SelfDispatch line method args anno) = show line : anno : "self_dispatch" : output_node method ++ output_node_list args
output_node (If line predicate then_expr else_expr anno) = show line : anno : "if" : output_nodes [predicate, then_expr, else_expr]
output_node (While line predicate body anno) = show line : anno : "while" : output_nodes [predicate, body]
output_node (Block line body anno) = show line : anno : "block" : output_node_list body
output_node (New line type_id anno) = show line : anno : "new" : output_node type_id
output_node (Isvoid line x anno) = show line : anno : "isvoid" : output_node x
output_node (Plus line x y anno) = show line : anno : "plus" : output_nodes [x, y]
output_node (Minus line x y anno) = show line : anno : "minus" : output_nodes [x, y]
output_node (Times line x y anno) = show line : anno : "times" : output_nodes [x, y]
output_node (Divide line x y anno) = show line : anno : "divide" : output_nodes [x, y]
output_node (LessThan line x y anno) = show line : anno : "lt" : output_nodes [x, y]
output_node (LessEqual line x y anno) = show line : anno : "le" : output_nodes [x, y]
output_node (Equal line x y anno) = show line : anno : "eq" : output_nodes [x, y]
output_node (Not line x anno) = show line : anno : "not" : output_node x
output_node (Negate line x anno) = show line : anno : "negate" : output_node x
output_node (IntL line x anno) = [show line, anno, "integer" , show x]
output_node (StringL line x anno) = [show line, anno, "string", x]
output_node (BoolL line x anno) = [show line, anno, if x then "true" else "false"]
output_node (Identifier line x anno) = show line : anno : "identifier" : output_node x
output_node (Let line bindings body anno) = show line : anno : "let" : output_node_list bindings ++ output_node body
output_node (Case line e case_elements anno) = show line : anno : "case" : output_node e ++ output_node_list case_elements
output_node (LetBinding var type_id Nothing) = "let_binding_no_init" : output_nodes [var, type_id]
output_node (LetBinding var type_id (Just initial)) = "let_binding_init" : output_nodes [var, type_id, initial]
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
    let class_ids' = create_class_ids ast
    if isRight class_ids'
       then report_error $ getRight class_ids'
       else return ()
    let Left class_ids = class_ids'

    -- Constructs the parent map
    let parent_map' = create_parent_map class_ids ast
    if isRight parent_map'
       then report_error $ getRight parent_map'
       else return()
    let Left parent_map = parent_map'
    
    -- Check if parent map contains cycles
    if has_cycle class_ids parent_map
       then report_error $ Err 0 "inheritance cycle"
       else return()

    -- Constructs the class map
    let class_map' = create_class_map ast class_ids parent_map
    if isRight class_map'
       then report_error $ getRight class_map'
       else return()
    let Left class_map = class_map'
        
    -- Constructs the implementation map
    let imp_map' = create_imp_map ast class_ids parent_map
    if isRight imp_map'
       then report_error $ getRight imp_map'
       else return ()
    let Left imp_map = imp_map'
    
    -- Check main() method in Main class
    let no_main_error = check_main imp_map
    if Maybe.isJust no_main_error
       then report_error $ Maybe.fromJust no_main_error
       else return()
    
    -- Type-check the AST
    let annotated_ast' = annotate_ast class_map imp_map parent_map ast
    if isRight annotated_ast'
       then report_error $ getRight annotated_ast'
       else return()
    let Left annotated_ast = annotated_ast'
        
    -- Re-generate class_map and imp_map
    let annotated_class_map = getLeft $ create_class_map annotated_ast class_ids parent_map
    let annotated_imp_map = getLeft $ create_imp_map annotated_ast class_ids parent_map

    -- Emit file
    outfp <- openFile output_filename WriteMode
    let class_map_output = output_class_map annotated_class_map
    let imp_map_output = output_imp_map annotated_imp_map
    let parent_map_output = output_parent_map parent_map
    let ast_output = output_node_list annotated_ast
    mapM (\line -> hPutStrLn outfp line) class_map_output
    mapM (\line -> hPutStrLn outfp line) imp_map_output
    mapM (\line -> hPutStrLn outfp line) parent_map_output
    mapM (\line -> hPutStrLn outfp line) ast_output
    
    hClose infp
    hClose outfp
