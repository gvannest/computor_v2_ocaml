(* ************** Declaration section ******************* *)

%token <int> CONST_INT
%token <float> CONST_FLOAT
%token <string> VAR
%token PLUS MINUS TIMES DIV POWER
%token LPAREN RPAREN
%token EOF

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left POWER             /* highest precedence */


%start <Solver.expr> parse_formula
%type <Solver.expr> formula_expr

%%

(* ************** rules section ******************* *)

parse_formula:
    | f = formula_expr EOF  { f }

formula_expr:
    | f = CONST_FLOAT                               { Literal_float f }
    | n = CONST_INT                               { Literal_int n }
    | LPAREN e = formula_expr RPAREN                { e }
    | e1 = formula_expr PLUS e2 = formula_expr      { Plus (e1, e2) }
    | e1 = formula_expr MINUS e2 = formula_expr     { Minus (e1, e2) }
    | e1 = formula_expr TIMES e2 = formula_expr     { Times (e1, e2) }
    | e1 = formula_expr DIV e2 = formula_expr       { Div (e1, e2) }
    | e1 = formula_expr POWER e2 = formula_expr     { Power (e1, e2) }