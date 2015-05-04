import sys
import copy

sys.setrecursionlimit(10000)
class_map, imp_map, parent_map = None, None, None


class Store:
    def __init__(self):
        self.next_loc = 0
        self.store = {}

    def new(self):
        self.next_loc += 1
        return self.next_loc - 1

    def put(self, loc, val):
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
            e.environment[key] = value
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
        return [line_num, type_name, 'while', predicate, body]

    if expr_type == 'block':
        body = build_list(build_expr, infile)
        return [line_num, type_name, 'block', body]

    if expr_type == 'new':
        identifier = build_id(infile)
        return [line_num, type_name, 'new', identifier]

    if expr_type in ['plus', 'minus', 'times', 'divide', 'lt', 'le', 'eq']:
        x = build_expr(infile)
        y = build_expr(infile)
        return [line_num, type_name, expr_type, x, y]

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
        case_expr = build_expr(infile)
        elements = build_list(build_case_element, infile)
        return [line_num, type_name, 'case', case_expr, elements]

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
    if type_name == 'Bool':
        return ['Bool', False]
    if type_name == 'String':
        return ['String', 0, '']
    return ['void']


# TODO: Object.copy
# TODO: rename variables
def run_internal(so, s, e, method_call):
    if method_call == 'Object.abort':
        sys.exit('abort')

    if method_call == 'Object.type_name':
        return ['String', len(so[0]), so[0]], s

    if method_call == 'Object.copy':
        if so[0] in ['Int', 'Bool', 'String']:
            return so, s

        s1 = copy.deepcopy(s)
        e1 = Environment()
        for identifier, old_loc in so[1].iteritems():
            val = s1.get(old_loc)
            new_loc = s1.new()
            s1 = s1.put(new_loc, val)
            e1.associate(identifier, new_loc)

        return [so[0], e1], s1

    if method_call == 'IO.out_string':
        loc_x = e.get('x')
        val_x = s.get(loc_x)[2].replace('\\n', '\n').replace('\\t', '\t')
        sys.stdout.write(val_x)
        return so, s

    if method_call == 'IO.out_int':
        loc_x = e.get('x')
        val_x = s.get(loc_x)[1]
        sys.stdout.write(str(val_x))
        return so, s

    if method_call == 'IO.in_string':
        line = sys.stdin.readline().strip('\n')
        if '\0' in line:
            return ['String', 0, ''], s
        else:
            return ['String', len(line), line], s

    if method_call == 'IO.in_int':
        line = sys.stdin.readline().strip(' \n')
        val = check_int(line)
        if val is not None:
            return ['Int', val], s
        else:
            return ['Int', 0], s

    if method_call == 'String.length':
        return ['Int', so[1]], s

    if method_call == 'String.concat':
        par_s = s.get(e.get('s'))
        return ['String', so[1] + par_s[1], so[2] + par_s[2]], s

    if method_call == 'String.substr':
        val_i = s.get(e.get('i'))[1]
        val_l = s.get(e.get('l'))[1]
        if val_i + val_l > so[1]:
            sys.exit('ERROR: 0: Exception: String.substr out of range')
        return ['String', val_l, so[2][val_i:val_i+val_l]], s


def check_int(s):
    if len(s) == 0:
        return None

    i = 0
    if s[0] in ['+', '-']:
        i += 1

    numbers = [str(k) for k in range(10)]
    while i < len(s) and s[i] in numbers:
        i += 1

    try:
        val = int(s[0:i])
        if -2147483648 <= val <= 2147483647:
            return val
        else:
            return None
    except ValueError:
        return None


def is_primitive(v):
    if v[0] in ['Int', 'Bool', 'String']:
        return True
    return False


def convert_to_signed_32(t):
    x = t & 0xffffffff
    if x > 0x7fffffff:
        return x - 0x100000000
    else:
        return x


def compare(x, y, op):
    if op == 'lt':
        return x < y

    if op == 'le':
        return x <= y

    if op == 'eq':
        return x == y

    assert False


def conformed_to(t0, t1):
    if t0 == t1:
        return True

    while t0 in parent_map:
        t0 = parent_map[t0]
        if t0 == t1:
            return True

    return False


def run(so, s, e, expr):
    if expr[2] == 'internal':
        v, s1 = run_internal(so, s, e, expr[3])
        return v, s1

    if expr[2] == 'assign':
        v1, s2 = run(so, s, e, expr[4])
        l1 = e.get(expr[3][1])
        s3 = s2.put(l1, v1)
        return v1, s3

    if expr[2] in ['dynamic_dispatch', 'static_dispatch', 'self_dispatch']:
        if expr[2] == 'dynamic_dispatch':
            method_name = expr[4][1]
            actuals = expr[5]
        elif expr[2] == 'static_dispatch':
            method_name = expr[5][1]
            actuals = expr[6]
        else:
            method_name = expr[3][1]
            actuals = expr[4]

        s1 = copy.deepcopy(s)
        v = []
        for actual in actuals:
            vx, s1 = run(so, s1, e, actual)
            v.append(vx)

        if expr[2] == 'self_dispatch':
            v0, s2 = so, s1
        else:
            v0, s2 = run(so, s1, e, expr[3])

        if v0[0] == 'void':
            sys.exit('ERROR: %d: Exception: dispatch on void' % expr[0])

        if expr[2] == 'static_dispatch':
            class_name = expr[4][1]
        else:
            class_name = v0[0]

        imp = imp_map[class_name][method_name]
        formals, body = imp[0], imp[2]
        lx = [s2.new() for i in formals]
        s3 = s2.put_all(dict(zip(lx, v)))

        if is_primitive(v0):
            e1 = e
        else:
            e1 = e.associate_all(v0[1])
        e2 = e1.associate_all(dict(zip(formals, lx)))
        v1, s4 = run(v0, s3, e2, body)
        return v1, s4

    if expr[2] == 'if':
        predicate = expr[3]
        then_expr = expr[4]
        else_expr = expr[5]

        v0, s2 = run(so, s, e, predicate)
        if v0[1]:
            v2, s3 = run(so, s2, e, then_expr)
            return v2, s3
        else:
            v3, s3 = run(so, s2, e, else_expr)
            return v3, s3

    if expr[2] == 'while':
        s1 = copy.deepcopy(s)
        while True:
            v0, s1 = run(so, s1, e, expr[3])
            if not v0[1]:
                return ['void'], s1
            _, s1 = run(so, s1, e, expr[4])

    if expr[2] == 'block':
        s1 = copy.deepcopy(s)
        assert len(expr[3]) > 0

        v = None
        for stmt in expr[3]:
            v, s1 = run(so, s1, e, stmt)
        return v, s1

    if expr[2] == 'let':
        bindings = expr[3]
        body = expr[4]
        s1 = copy.deepcopy(s)
        e1 = copy.deepcopy(e)

        for binding in bindings:
            identifier = binding[0][1]
            type_name = binding[1][1]
            initializer = binding[2]

            if initializer is None:
                v1 = default(type_name)
            else:
                v1, s1 = run(so, s1, e1, initializer)

            l0 = s1.new()
            s1 = s1.put(l0, v1)
            e1 = e1.associate(identifier, l0)

        v2, s4 = run(so, s1, e1, body)
        return v2, s4

    if expr[2] == 'case':
        case_expr = expr[3]
        elements = expr[4]
        v0, s2 = run(so, s, e, case_expr)

        if v0[0] == 'void':
            sys.exit('ERROR: %d: Exception: case on void' % expr[0])

        dynamic_type = v0[0]

        selected = None
        for element in elements:
            ti = element[1][1]
            if conformed_to(dynamic_type, ti):
                if selected is None or conformed_to(ti, selected[1][1]):
                    selected = element

        if selected is None:
            sys.exit('ERROR: %d: Exception: case without matching branch: %s(...)' % (expr[0], dynamic_type))

        identifier = selected[0][1]
        body = selected[2]
        l0 = s2.new()
        s3 = s2.put(l0, v0)
        e1 = e.associate(identifier, l0)
        v1, s4 = run(so, s3, e1, body)
        return v1, s4


    if expr[2] == 'new':
        t = expr[3][1]
        if t == 'SELF_TYPE':
            t0 = so[0]
        else:
            t0 = t
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
            _, s3 = run(v1, s2, v1[1], block_expr)
        else:
            s3 = s2
        return v1, s3

    if expr[2] == 'isvoid':
        x, s2 = run(so, s, e, expr[3])

        if x[0] == 'void':
            return ['Bool', True], s2

        return ['Bool', False], s2

    if expr[2] in ['plus', 'minus', 'times', 'divide']:
        x, s2 = run(so, s, e, expr[3])
        y, s3 = run(so, s2, e, expr[4])

        if expr[2] == 'plus':
            return ['Int', convert_to_signed_32(x[1] + y[1])], s3

        if expr[2] == 'minus':
            return ['Int', convert_to_signed_32(x[1] - y[1])], s3

        if expr[2] == 'times':
            return ['Int', convert_to_signed_32(x[1] * y[1])], s3

        if expr[2] == 'divide':
            if y[1] == 0:
                sys.exit('ERROR: %d: Exception: division by zero' % expr[0])
            q = abs(x[1]) // abs(y[1])
            if x[1] * y[1] > 0:
                result = q
            else:
                result = -q
            return ['Int', convert_to_signed_32(result)], s3

    if expr[2] == 'negate':
        x, s2 = run(so, s, e, expr[3])
        if x[1] == -0x80000000:
            return ['Int', x[1]], s2
        else:
            return ['Int', -x[1]], s2

    if expr[2] in ['lt', 'le', 'eq']:
        x, s2 = run(so, s, e, expr[3])
        y, s3 = run(so, s2, e, expr[4])

        if x[0] == 'void' and y[0] == 'void':
            return ['Bool', True], s3

        if x[0] == 'void' or y[0] == 'void':
            return ['Bool', False], s3

        if x[0] == 'Int' and y[0] == 'Int':
            return ['Bool', compare(x[1], y[1], expr[2])], s3

        if x[0] == 'Bool' and y[0] == 'Bool':
            return ['Bool', compare(x[1], y[1], expr[2])], s3

        if x[0] == 'String' and y[0] == 'String':
            return ['Bool', compare(x[2], y[2], expr[2])], s3

        assert not is_primitive(x) and not is_primitive(y)

        if expr[2] in ['eq', 'le']:
            return ['Bool', x[1] == y[1]], s3
        else:
            return ['Bool', False], s3

    if expr[2] == 'not':
        x, s2 = run(so, s, e, expr[3])
        return ['Bool', not x[1]], s2

    if expr[2] == 'integer':
        return ['Int', expr[3]], s

    if expr[2] == 'string':
        return ['String', len(expr[3]), expr[3]], s

    if expr[2] == 'true':
        return ['Bool', True], s

    if expr[2] == 'false':
        return ['Bool', False], s

    if expr[2] == 'identifier':
        identifier = expr[3][1]
        if identifier == 'self':
            return so, s

        return s.get(e.get(identifier)), s

    #print(expr[3])
    assert False


def __main__():
    input_filename = sys.argv[1]
    infile = open(input_filename, 'r')
    global class_map
    global imp_map
    global parent_map
    class_map = build_class_map(infile)
    imp_map = build_imp_map(infile)
    parent_map = build_parent_map(infile)

    init_receiver = [None, None, 'new', [None, 'Main']]
    init_method = [None, 'main']
    init_expr = [None, None, 'dynamic_dispatch', init_receiver, init_method, []]

    v, s = run(None, Store(), Environment(), init_expr)
    #print(v, s)

if __name__ == '__main__':
    __main__()
