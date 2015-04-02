import sys
import copy


class Store:
    def __init__(self):
        self.next_loc = 0
        self.store = {}

    def new(self):
        s = copy.deepcopy(self)
        s.next_loc += 1
        return s.next_loc - 1, s

    def put(self, val, loc):
        s = copy.deepcopy(self)
        s.store[loc] = val
        return s

    def get(self, loc):
        return self.store[loc]


class Environment:
    def __init__(self):
        self.next_loc = 0
        self.environment = {}

    def associate(self, var, loc):
        self.environment[var] = loc

    def get(self, var):
        return self.environment[var]


def match(actual, expected):
    if actual != expected:
        sys.exit('Mismatch')


def next_int(infile):
    return int(infile.readline().strip('\n'))


def next_line(infile):
    return infile.readline().strip('\n')


def build_class_map(infile):
    match(next_line(infile), 'class_map')
    class_num = next_int(infile)
    class_map = {}

    for i in range(class_num):
        class_name = next_line(infile)
        attribute_num = next_int(infile)
        attributes = []

        for j in range(attribute_num):
            initializer_type = next_line(infile)
            attribute_name = next_line(infile)
            type_name = next_line(infile)

            if initializer_type == 'no_initializer':
                attributes.append([attribute_name, type_name, None])
            elif initializer_type == 'initializer':
                initializer = build_expr(infile)
                attributes.append([attribute_name, type_name, initializer])
            else:
                sys.exit('Mismatch')

        class_map[class_name] = attributes

    return class_map

def build_imp_map(infile):
    match(next_line(infile), 'implementation_map')
    class_num = next_int(infile)
    imp_map = {}

    for i in range(class_num):
        class_name = next_line(infile)
        method_num = next_int(infile)
        methods = {}

        for j in range(method_num):
            method_name = next_line(infile)
            formal_num = next_int(infile)
            formals = []

            for k in range(formal_num):
                formals.append(next_line(infile))

            origin = next_line(infile)
            body = build_expr(infile)
            methods[method_name] = [formals, origin, body]

        imp_map[class_name] = methods

    return imp_map


def build_parent_map(infile):
    match(next_line(infile), 'parent_map')
    relation_num = next_int(infile)
    parent_map = {}

    for i in range(relation_num):
        child_class = next_line(infile)
        parent_class = next_line(infile)
        parent_map[child_class] = parent_class

    return parent_map


def build_id(infile):
    line_num = next_line(infile)
    identifier = next_line(infile)
    return [line_num, identifier]


def build_list(func, infile):
    n = next_int(infile)
    elem_list = []

    for i in range(n):
        elem_list.append(func(infile))

    return elem_list


def build_expr(infile):
    line_num = next_int(infile)
    type_name = next_line(infile)
    expr_type = next_line(infile)

    if expr_type == 'internal':
        internal_call = next_line(infile)
        return [line_num, type_name, 'internal', internal_call]

    if expr_type == 'assign':
        lhs = build_id(infile)
        rhs = build_expr(infile)
        return [line_num, type_name, 'assign', lhs, rhs]

    if expr_type == 'dynamic_dispatch':
        receiver = build_expr(infile)
        method = build_id(infile)
        args = build_list(build_expr, infile)
        return [line_num, type_name, 'dynamic_dispatch', receiver, method, args]

    if expr_type == 'static_dispatch':
        receiver = build_expr(infile)
        static_type = build_id(infile)
        method = build_id(infile)
        args = build_list(build_expr, infile)
        return [line_num, type_name, 'static_dispatch', receiver, static_type, method, args]

    if expr_type == 'self_dispatch':
        method = build_id(infile)
        args = build_list(build_expr, infile)
        return [line_num, type_name, 'self_dispatch', method, args]

    if expr_type == 'if':
        predicate = build_expr(infile)
        then_expr = build_expr(infile)
        else_expr = build_expr(infile)
        return [line_num, type_name, 'if', predicate, then_expr, else_expr]

    if expr_type == 'while':
        predicate = build_expr(infile)
        body = build_expr(infile)
        return [line_num, type_name, 'if', predicate, predicate, body]

    if expr_type == 'block':
        body = build_list(build_expr, infile)
        return [line_num, type_name, 'block', body]

    if expr_type == 'new':
        identifier = build_id(infile)
        return [line_num, type_name, 'new', identifier]

    if expr_type in ['plus', 'minus', 'times', 'divide', 'lt', 'le', 'eq']:
        lhs = build_expr(infile)
        rhs = build_expr(infile)
        return [line_num, type_name, expr_type, lhs, rhs]

    if expr_type in ['isvoid', 'not', 'negate']:
        x = build_expr(infile)
        return [line_num, type_name, expr_type, x]

    if expr_type == 'integer':
        value = next_int(infile)
        return [line_num, type_name, 'integer', value]

    if expr_type == 'string':
        value = next_line(infile)
        return [line_num, type_name, 'string', value]

    if expr_type == 'identifier':
        identifier = build_id(infile)
        return [line_num, type_name, 'identifier', identifier]

    if expr_type in ['true', 'false']:
        return [line_num, type_name, expr_type]

    if expr_type == 'let':
        bindings = build_list(build_let_binding, infile)
        body = build_expr(infile)
        return [line_num, type_name, 'let', bindings, body]

    if expr_type == 'case':
        elements = build_list(build_case_element, infile)
        return [line_num, type_name, 'case', elements]

    sys.exit('Mismatch')


def build_let_binding(infile):
    binding_type = next_line(infile)
    var = build_id(infile)
    type_name = build_id(infile)

    if binding_type == 'let_binding_no_init':
        return [var, type_name, None]

    if binding_type == 'let_binding_init':
        initializer = build_expr(infile)
        return [var, type_name, initializer]

    sys.exit('Mismatch')


def build_case_element(infile):
    var = build_id(infile)
    type_name = build_id(infile)
    body = build_expr(infile)
    return [var, type_name, body]


def run(so, s, e, expr):
    if expr[2] == 'new':
        t = expr[3]
        t0 = so[0] if t == 'SELF_TYPE' else t



def __main__():
    input_filename = sys.argv[1]
    infile = open(input_filename, 'r')
    class_map = build_class_map(infile)
    imp_map = build_imp_map(infile)
    parent_map = build_parent_map(infile)

    init_receiver = [None, None, 'new', [None, 'Main']]
    init_method = [None, 'main']
    init_expr = [None, None, 'dynamic_dispatch', init_receiver, init_method, []]

    run(None, Store(), Environment(), init_expr)

    print(imp_map)

if __name__ == '__main__':
    __main__()