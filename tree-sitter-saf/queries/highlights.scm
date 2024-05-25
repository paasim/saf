(bool) @boolean
(int) @number
(string) @string

(unop) @operator
(binop) @operator
["fn"] @keyword

(identifier) @variable
(params (identifier) @variable.parameter)
(args (identifier) @variable.parameter)
(call (identifier) @function.call)


["[" "]" "(" ")"] @punctuation.bracket
[";" ","] @punctuation.delimiter
["=" "?" ":"] @operator
(comment) @comment
