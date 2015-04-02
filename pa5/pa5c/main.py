import sys
import copy


class Store:
    def __init__(self):
        self.next_loc = 0
        self.store = {}

    def new(self):
        self.next_loc += 1
        return self.next_loc - 1

    def put(self, val, loc):
        s = copy.deepcopy(self)
        s.store[loc] = val
        return s

    def put_all(self, d):
        s = copy.deepcopy(self)
        for key, value in d.iteritems():
            s.store[key] = value
        return s

    def get(self, loc):
        return self.store[loc]


class Environment:
    def __init__(self):
        self.next_loc = 0
        self.environment = {}

    def associate(self, var, loc):
        e = copy.deepcopy(self)
        e.environment[var] = loc
        return e

    def associate_all(self, d):
        e = copy.deepcopy(self)
        for key, value in d.iteritems():
            e[key] = value
        return e

    def get(self, var):
        return self.environment[var]


def match(actual, expected):
    assert actual == expected


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
                assert False

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

    assert False


def build_let_binding(infile):
    binding_type = next_line(infile)
    var = build_id(infile)
    type_name = build_id(infile)

    if binding_type == 'let_binding_no_init':
        return [var, type_name, None]

    if binding_type == 'let_binding_init':
        initializer = build_expr(infile)
        return [var, type_name, initializer]

    assert False


def build_case_element(infile):
    var = build_id(infile)
    type_name = build_id(infile)
    body = build_expr(infile)
    return [var, type_name, body]


def default(type_name):
    if type_name == 'Int':
        return ['Int', 0]
    elif type_name == 'Bool':
        return ['Bool', False]
    elif type_name == 'String':
        return ['String', 0, '']
    else:
        return ['void']


def run(class_map, imp_map, so, s, e, expr):
    if expr[2] == 'assign':
        v1, s2 = run(class_map, imp_map, so, s, e, expr[4])
        l1 = e.get(expr[3][1])
        s3 = s2.put(v1, l1)
        return v1, s3

    if expr[2] == 'dynamic_dispatch':
        

    if expr[2] == 'block':
        s1 = copy.deepcopy(s)
        assert len(expr[3]) > 0

        v = None
        for stmt in expr[3]:
            v, s1 = run(class_map, imp_map, so, s1, e, stmt)
        return v, s1

    if expr[2] == 'new':
        t = expr[3][1]
        t0 = so[0] if t == 'SELF_TYPE' else t
        attributes = class_map[t0]

        s1 = copy.deepcopy(s)
        l = [s1.new() for i in attributes]
        v1 = [t0, dict(zip([attribute[0] for attribute in attributes], l))]
        s2 = s1.put_all(dict(zip(l, [default(attribute[1]) for attribute in attributes])))

        block_stmts = []
        for attribute in attributes:
            if attribute[2] is not None:
                block_stmts.append([None, None, 'assign', [None, attribute[0]], attribute[2]])

        if len(block_stmts) > 0:
            block_expr = [None, None, 'block', block_stmts]
            _, s3 = run(class_map, imp_map, v1, s2, v1[1], block_expr)
        else:
            s3 = s2
        return v1, s3

    if expr[2] == 'integer':
        return ['Int', expr[3]], s

    assert False


def __main__():
    input_filename = sys.argv[1]
    infile = open(input_filename, 'r')
    class_map = build_class_map(infile)
    imp_map = build_imp_map(infile)
    parent_map = build_parent_map(infile)

    init_receiver = [None, None, 'new', [None, 'Main']]
    init_method = [None, 'main']
    init_expr = [None, None, 'dynamic_dispatch', init_receiver, init_method, []]

    v, s = run(class_map, imp_map, None, Store(), Environment(), init_receiver)
    print(v, s.store)

if __name__ == '__main__':
    __main__()