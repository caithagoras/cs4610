{
open Printf;;
open Char;;
}

let A = 'a' | 'A'
let B = 'b' | 'B'
let C = 'c' | 'C'
let D = 'd' | 'D'
let E = 'e' | 'E'
let F = 'f' | 'F'
let G = 'g' | 'G'
let H = 'h' | 'H'
let I = 'i' | 'I'
let J = 'j' | 'J'
let K = 'k' | 'K'
let L = 'l' | 'L'
let M = 'm' | 'M'
let N = 'n' | 'N'
let O = 'o' | 'O'
let P = 'p' | 'P'
let Q = 'q' | 'Q'
let R = 'r' | 'R'
let S = 's' | 'S'
let T = 't' | 'T'
let U = 'u' | 'U'
let V = 'v' | 'V'
let W = 'w' | 'W'
let X = 'x' | 'X'
let Y = 'y' | 'Y'
let Z = 'z' | 'Z'
let whitespaces = [' ' '\t' '\r' '\011' '\012']+
let digit = ['0' - '9']
let integer = digit+
let lower_case = ['a' - 'z']
let upper_case = ['A' - 'Z']
let alpha = lower_case | upper_case | digit | '_'
let identifier = lower_case (alpha)*
let type_regex = upper_case (alpha)*
let string_regex = '"' ([^ '\000' '\n' '"' '\\'] | ('\\' [^ '\000' '\n']))* '"'
let single_line_comment = "--" [^ '\n']* ('\n' | eof)

rule comment nline depth = parse
    eof                           { printf "ERROR: %d: Lexer: EOF in (* comment *)" nline; exit 0 }
    | "*)"                        { if depth = 1 then tokenlize nline lexbuf else comment nline (depth-1) lexbuf }
    | "(*"                        { comment nline (depth+1) lexbuf }
    | '\n'                        { comment (nline+1) depth lexbuf }
    | _                           { comment nline depth lexbuf }

and tokenlize nline = parse
    eof                           { [] }
    | "(*"                        { comment nline 1 lexbuf }
    | single_line_comment         { tokenlize (nline+1) lexbuf }
    | whitespaces                 { tokenlize nline lexbuf }
    | '\n'                        { tokenlize (nline+1) lexbuf }
    | "@"                         { string_of_int(nline) :: "at" :: tokenlize nline lexbuf }
    | C A S E                     { string_of_int(nline) :: "case" :: tokenlize nline lexbuf }
    | C L A S S                   { string_of_int(nline) :: "class" :: tokenlize nline lexbuf }
    | ":"                         { string_of_int(nline) :: "colon" :: tokenlize nline lexbuf }
    | ","                         { string_of_int(nline) :: "comma" :: tokenlize nline lexbuf }
    | "/"                         { string_of_int(nline) :: "divide" :: tokenlize nline lexbuf }
    | "."                         { string_of_int(nline) :: "dot" :: tokenlize nline lexbuf }
    | E L S E                     { string_of_int(nline) :: "else" :: tokenlize nline lexbuf }
    | "="                         { string_of_int(nline) :: "equals" :: tokenlize nline lexbuf }
    | E S A C                     { string_of_int(nline) :: "esac" :: tokenlize nline lexbuf }
    | 'f' A L S E                 { string_of_int(nline) :: "false" :: tokenlize nline lexbuf }
    | F I                         { string_of_int(nline) :: "fi" :: tokenlize nline lexbuf }
    | I F                         { string_of_int(nline) :: "if" :: tokenlize nline lexbuf }
    | I N                         { string_of_int(nline) :: "in" :: tokenlize nline lexbuf }
    | I N H E R I T S             { string_of_int(nline) :: "inherits" :: tokenlize nline lexbuf }
    | I S V O I D                 { string_of_int(nline) :: "isvoid" :: tokenlize nline lexbuf }
    | "<-"                        { string_of_int(nline) :: "larrow" :: tokenlize nline lexbuf }
    | "{"                         { string_of_int(nline) :: "lbrace" :: tokenlize nline lexbuf }
    | "<="                        { string_of_int(nline) :: "le" :: tokenlize nline lexbuf }
    | L E T                       { string_of_int(nline) :: "let" :: tokenlize nline lexbuf }
    | L O O P                     { string_of_int(nline) :: "loop" :: tokenlize nline lexbuf }
    | "("                         { string_of_int(nline) :: "lparen" :: tokenlize nline lexbuf }
    | "<"                         { string_of_int(nline) :: "lt" :: tokenlize nline lexbuf }
    | "-"                         { string_of_int(nline) :: "minus" :: tokenlize nline lexbuf }
    | N E W                       { string_of_int(nline) :: "new" :: tokenlize nline lexbuf }
    | N O T                       { string_of_int(nline) :: "not" :: tokenlize nline lexbuf }
    | O F                         { string_of_int(nline) :: "of" :: tokenlize nline lexbuf }
    | "+"                         { string_of_int(nline) :: "plus" :: tokenlize nline lexbuf }
    | P O O L                     { string_of_int(nline) :: "pool" :: tokenlize nline lexbuf }
    | "=>"                        { string_of_int(nline) :: "rarrow" :: tokenlize nline lexbuf }
    | "}"                         { string_of_int(nline) :: "rbrace" :: tokenlize nline lexbuf }
    | ")"                         { string_of_int(nline) :: "rparen" :: tokenlize nline lexbuf }
    | ";"                         { string_of_int(nline) :: "semi" :: tokenlize nline lexbuf }
    | T H E N                     { string_of_int(nline) :: "then" :: tokenlize nline lexbuf }
    | "~"                         { string_of_int(nline) :: "tilde" :: tokenlize nline lexbuf }
    | "*"                         { string_of_int(nline) :: "times" :: tokenlize nline lexbuf }
    | 't' R U E                   { string_of_int(nline) :: "true" :: tokenlize nline lexbuf }
    | W H I L E                   { string_of_int(nline) :: "while" :: tokenlize nline lexbuf }
    | integer as s                { try
                                        let n = int_of_string(s) in
                                            if n>=0 && n<=2147483647 then
                                                string_of_int(nline) :: "integer" :: string_of_int(n) :: tokenlize nline lexbuf
                                            else begin
                                                printf "ERROR: %d: Lexer: not a non-negative 32-bit signed integer: %s\n" nline s;
                                                exit 0
                                            end
                                    with _ -> begin
                                        printf "ERROR: %d: Lexer: not a non-negative 32-bit signed integer: %s\n" nline s;
                                        exit 0
                                    end
                                  }
    | identifier as s             { string_of_int(nline) :: "identifier" :: s :: tokenlize nline lexbuf }
    | type_regex as s             { string_of_int(nline) :: "type" :: s :: tokenlize nline lexbuf }
    | string_regex as s           { let str = String.sub s 1 ((String.length s)-2) in
                                    if String.length str <= 1024 then
                                        string_of_int(nline) :: "string" :: str :: tokenlize nline lexbuf
                                    else begin
                                        printf "ERROR: %d: Lexer: string constant is too long (%d > 1024)\n" nline (String.length str);
                                        exit 0
                                    end
                                  }
    | _ as s                      { printf "ERROR: %d: Lexer: invalid character: %c\n" nline s; exit 0 }

{
    let input_file = open_in (Sys.argv.(1)) in
    let input_channel = Lexing.from_channel input_file in
    let tokens = tokenlize 1 input_channel in
    let output_file = open_out (Sys.argv.(1) ^ "-lex") in
        List.iter (fun s -> fprintf output_file "%s\n" s) tokens
}
