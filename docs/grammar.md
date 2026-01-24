# stock grammar

| Operator        | Name            | Description                                   |
| --------------- | --------------- | --------------------------------------------- |
| `rule ::= body` | Production Rule | Defines `rule` as `body`                      |
| `a \| b`        | Alternation     | Represents a choice between `a` or `b`        |
| `( ... )`       | Grouping        | Groups expressions together                   |
| `[ ... ]`       | Option          | The expression is optional (0 or 1 times)     |
| `{ ... }`       | Repetition      | The expression occurs 0 or more times         |
| `"..."`         | Terminal        | A literal string that must appear in the code |
| `r"..."`        | Pattern         | A regex pattern that must be matched          |
| `(* *)`         | Comment         | A comment within the grammar definition       |

```ebnf
(* datatypes *)

digit      ::= r"[0-9]"
digit_list ::= digit { [ "_" ] digit }

char       ::= r"[a-zA-Z_]"
char_list  ::= char { char }

identifier ::= char_list
number     ::= digit_list [ "." digit_list ]

(* common *)

datatype ::= number | identifier
block    ::= "{" { statement } [ expression ] "}" 

(* expressions *)

expression                ::= additive_expression

equality_expression       ::= comparison_expression { ("==" | "!=") comparison_expression }
comparision_expression    ::= additive_expression { (">=" | "<=" | ">" | "<") additive_expression }

additive_expression       ::= multiplicative_expression { ("+" | "-") multiplicative_expression }
multiplicative_expression ::= unary_expression { ("*" | "/") unary_expression }
unary_expression          ::= ["-" | "!"] primary_expression

primary_expression        ::=
    | "(" expression ")"
    | datatype

(* statements *)

statement        ::=
    | let_statement
    | return_statement
    | block
statement_list   ::= statement { statement } [ expression ]

let_statement    ::= "let" identifier "=" expression ";"
return_statement ::= "return" expression ";"
```
