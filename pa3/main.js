
// Require
var fs = require('fs');
var readline = require('readline');

// Process input/output filename
var input_filename = process.argv[2];
var output_filename = input_filename.substring(0, input_filename.length-4) + '-ast';

// Define input/output stream
var instream = fs.createReadStream(input_filename);
var outstream;
var rl = readline.createInterface({
    input: instream,
    output: process.stdout,
    terminal: false
});

// Define lexer
var token_count = 0
var parser = require("./parser").parser;

function Scanner() {
    this.setInput = function() {
    };
    this.lex = function() {
        var cl_lex_entry = cl_lex[token_count++] ;
        var location = Number(cl_lex_entry[0]);
        var token = cl_lex_entry[1];
        var lexeme = cl_lex_entry[2];
        this.yylloc.line = location;
        this.yytext = [location, lexeme];
        return token;
    };
}
parser.lexer = new Scanner();

// Define error handler of the parser
parser.yy.parseError = function(str, hash) {
    throw [hash.loc.line, hash.token];
}

// Define storage for input tokens
var lines = []
var cl_lex = []


// Read inputs
rl.on('line', function(line) {
    lines.push(line.replace("\r",""))
})

// Parsing
rl.on('close', function() {
    // Handle inputs
    var i = 0;
    while (i < lines.length) {
        if (lines[i+1] == "integer" || lines[i+1] == "identifier" || lines[i+1] == "type" || lines[i+1] == "string") {
            cl_lex.push([lines[i], lines[i+1].toUpperCase(), lines[i+2]]);
            i += 3;
        }
        else {
            cl_lex.push([lines[i], lines[i+1].toUpperCase()]);
            i += 2;
        }
    }
    cl_lex.push([cl_lex[cl_lex.length-1][0], 'EOF']);
    
    // Parse the input string
    try {
        var ast = parser.parse("");
    }
    catch (err) {
        process.stdout.write("ERROR: " + err[0].toString() + ": Parser: syntax error near " + err[1] + "\n");
        process.exit(0);
    }

    // Output parse tree if no syntax error is found
    outstream = fs.createWriteStream(output_filename);
    output_ast(ast);
})

// Functions for output the parse tree
function output_ast(ast) {
    output_class_list(ast);
}

function output_class_list(ast) {
    outstream.write(ast.length.toString() + "\n");
    for (var i=0; i<ast.length; i++)
        output_class(ast[i])
}

function output_class(ast) {
    output_id(ast[0]);
    if (ast[1] === undefined)
        outstream.write("no_inherits\n");
    else {
        outstream.write("inherits\n");
        output_id(ast[1]);
    }
    output_feature_list(ast[2]);
}

function output_id(ast) {
    outstream.write(ast[0].toString() + "\n" + ast[1] + "\n");
}

function output_feature_list(ast) {
    outstream.write(ast.length.toString() + "\n");
    for (var i=0; i<ast.length; i++)
        output_feature(ast[i])
}

function output_feature(ast) {
    outstream.write(ast[0] + "\n");
    switch (ast[0]) {
        case "method":
            output_id(ast[1]);
            output_formal_list(ast[2]);
            output_id(ast[3]);
            output_expr(ast[4]);
            break
        case "attribute_no_init":
            output_id(ast[1]);
            output_id(ast[2]);
            break;
        case "attribute_init":
            output_id(ast[1]);
            output_id(ast[2]);
            output_expr(ast[3]);
            break;
    }
}

function output_formal_list(ast) {
    outstream.write(ast.length.toString() + "\n");
    for (var i=0; i<ast.length; i++)
        output_formal(ast[i])
}

function output_formal(ast) {
    output_id(ast[0]);
    output_id(ast[1]);
}

function output_expr(ast){
    outstream.write(ast[0].toString() + "\n" + ast[1] + "\n");
    switch (ast[1]) {
        case "assign":
            output_id(ast[2]);
            output_expr(ast[3]);
            break;
        case "static_dispatch":
            output_expr(ast[2]);
            output_id(ast[3]);
            output_id(ast[4]);
            output_expr_list(ast[5]);
            break;
        case "dynamic_dispatch":
            output_expr(ast[2]);
            output_id(ast[3]);
            output_expr_list(ast[4]);
            break;
        case "self_dispatch":
            output_id(ast[2]);
            output_expr_list(ast[3]);
            break;
        case "if":
            output_expr(ast[2]);
            output_expr(ast[3]);
            output_expr(ast[4]);
            break;
        case "while":
            output_expr(ast[2]);
            output_expr(ast[3]);
            break;
        case "block":
            output_expr_list(ast[2]);
            break;
        case "new":
            output_id(ast[2]);
            break;
        case "isvoid":
            output_expr(ast[2]);
            break;
        case "plus":
        case "minus":
        case "times":
        case "divide":
        case "lt":
        case "le":
        case "eq":
            output_expr(ast[2]);
            output_expr(ast[3]);
            break;
        case "not":
        case "negate":
            output_expr(ast[2]);
            break;
        case "integer":
            outstream.write(ast[2].toString() + "\n");
            break;
        case "string":
            outstream.write(ast[2] + "\n");
            break;
        case "identifier":
            output_id(ast[2]);
            break;
        case "true":
        case "false":
            break;
        case "let":
            output_let_binding_list(ast[2]);
            output_expr(ast[3]);
            break;
        case "case":
            output_expr(ast[2]);
            output_case_element_list(ast[3]);
            break;
    }
}

function output_expr_list(ast) {
    outstream.write(ast.length.toString() + "\n");
    for (var i=0; i<ast.length; i++)
        output_expr(ast[i])
}

function output_let_binding_list(ast) {
    outstream.write(ast.length.toString() + "\n");
    for (var i=0; i<ast.length; i++)
        output_let_binding(ast[i]);
}

function output_let_binding(ast) {
    outstream.write(ast[0] + "\n");
    switch (ast[0]) {
        case "let_binding_no_init":
            output_id(ast[1]);
            output_id(ast[2]);
            break;
        case "let_binding_init":
            output_id(ast[1]);
            output_id(ast[2]);
            output_expr(ast[3]);
            break;
    }
}

function output_case_element_list(ast) {
    outstream.write(ast.length.toString() + "\n");
    for (var i=0; i<ast.length; i++)
        output_case_element(ast[i])
}

function output_case_element(ast) {
    output_id(ast[0]);
    output_id(ast[1]);
    output_expr(ast[2]);
}
