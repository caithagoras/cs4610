
var readline = require('readline');
var rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false
});

var line1 = undefined
var line2 = undefined

var graph = {}
var indeg = {}
var order = new Array()

rl.on('line', function(line) {
    if (line1 === undefined) {
        line1 = line.replace("\r","");
    }
    else {
        line2 = line.replace("\r","");
        if (!(line1 in indeg)) {
            indeg[line1] = 0;
            graph[line1] = new Array();
        }
        if (!(line2 in indeg)) {
            indeg[line2] = 0;
            graph[line2] = new Array();
        }

        indeg[line1] ++;
        graph[line2].push(line1);
        
        line1 = line2 = undefined;
    }
})

rl.on('close', function() {
    while (true) {
        var next_node = undefined;
        for (var node in indeg)
            if (indeg[node] == 0 && (next_node === undefined || node < next_node))
                next_node = node

        if (next_node === undefined)
            break;

        order.push(next_node);
        indeg[next_node] = -1;

        for (var inode in graph[next_node])
            indeg[graph[next_node][inode]] --;
    }

    if (order.length != Object.keys(indeg).length)
        process.stdout.write("cycle\n");
    else
        for (var i=0; i<order.length; i++)
            process.stdout.write(order[i] + "\n");
})
