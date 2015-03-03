import sys


def __main__():
    content = sys.stdin.readlines()
    content = [line.strip("\n\r") for line in content]

    graph = {}
    indeg = {}
    order = []

    # initialize
    for line in content:
        if line not in indeg:
            indeg[line] = 0
            graph[line] = []

    # construct graph
    i = 0
    while i < len(content):
        graph[content[i+1]].append(content[i])
        indeg[content[i]] += 1
        i += 2

    # topological sort
    while True:
        next_node = None
        for node in indeg:
            if indeg[node] == 0 and (next_node is None or node < next_node):
                next_node = node

        if next_node is None:
            break

        order.append(next_node)
        indeg[next_node] = -1
        for node in graph[next_node]:
            indeg[node] -= 1

    if len(order) != len(indeg):
        sys.stdout.write("cycle\n")
    else:
        for node in order:
            sys.stdout.write(node+"\n")


if __name__ == "__main__":
    __main__()
