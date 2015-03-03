Programming Assignment 2 - The Lexer
CS4610
Leiqing Cai

The lexer was generated using ocamllex, Ocaml's lexical analyzer generator.
main.mll is the lexer specification file, main.ml is the generated lexer.

Design Decisions:
    1. Key words
        Except for true and false, all key words are case-incensitive. In order to make the lexer specification clear, a regex that accepts both upper case and lower case letter is defined for each alphabet. For example: A = 'A' | 'a'
        In the way, each key word can be expressed by the concatenation of the corresponding regex. For example: "case" can be matched by "C A S E" (excluding the quotes).

    2. Integers: digit+
        Integers are just non-empty strings of digits. To handle the range restriction of integers, convert the matched string to integer. If an error is raised or the converted integer is not in the required range, an error message is displayed and the lexer exits.

    3. Indentifiers and Types
        Define the sets of all alpha-numeric characters and the underscore character as alpha.
        Indentifiers are lower-case letter followed by alpha*.
        Types are upper-case letter followed by alpha*.
    
    4. Strings: '"' ([^ '\000' '\n' '"' '\\'] | ('\\' [^ '\000' '\n']))* '"'
        a. Strings are always enclosed between double quotes.
        b. Within the quotes, two patterns might be repeatedly matched.
        c. The first pattern matches any single character, except for NUL, New Line, double quotation mark and backslash. This pattern consume all the normal characters excepts for backslash. (Note that a double quotation mark cannot appear by itself without being escaped).
        d. The second pattern matches a escape sequence, which is a black slash followed by a non-null and non-new-line character. (i.e. \" is matched by this pattern)
    
    5. Comments: Single line: "--" [^ '\n']* ('\n' | eof)
        The two different types of comment are handled differently.
        The single-line comment is matched by "--" followed by any character sequence (except for new line) and it has to end with a "\n" or an end-of-file.
        The multi-line comment is implemented using a new rule (entry point) called "comment". The rule has an integer parameter to keep track of the depth of the commment (level of comments nested).
        In the normal rule, when "(*" is encountered, comment is called with depth = 1.
        In the comment mode, when "(*" is encountered, comment is called with depth incremented by 1. When ")*" is encountered, comment is called with depth decrement by 1, except when the depth is already 1 (in this case, the normal rule is called). Anything other character will be consumed, with comment called with unchanged depth value.

Test Cases:
    good.cl include almost all accepting cases that the a correct lexer should handle.
        - Key words in mixed upper and lower cases.
        - Capitalized true and false are types, not key words.
        - Identifiers
        - Types
        - Integers within and at the boundary.
        - Symbols
        - Normal string literals, empty string, string with escaped sequence, string with escaped quotes, string length of 1024 (maximum length)
        - A short code snippets
        - Comments
    bad.cl
        - String of length (1025) greater than 1024 should be rejected.
