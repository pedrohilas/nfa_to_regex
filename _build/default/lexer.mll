{ open Parser }

rule token = parse
| ['a'-'z''0'-'9'] as c { CHAR c }
| '_' { EMPTY }
| '*' { STAR }
| '|' { CHOICE }
| '(' { LPAR }
| ')' { RPAR }
| eof { EOF }
