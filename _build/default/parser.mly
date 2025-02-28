%{ open Ast %}

%token <char> CHAR
%token EMPTY
%token STAR
%token CHOICE
%token LPAR
%token RPAR
%token EOF

%start main
%type <Ast.re> main

%%

main: r = regex EOF { r }

regex:
| r = disjunction { r }

disjunction:
| a = disjunction CHOICE b = concat { Escolha(a, b) }
| r = concat {r}

concat:
| a = concat b = repetition { Concatenacao(a, b) } 
| r = repetition {r}

repetition:
| r = repetition STAR { Estrela r }
| r = atom { r }

atom:
| LPAR r = regex RPAR { r }
| c = CHAR { Caracter c }
| EMPTY { Vazio }
