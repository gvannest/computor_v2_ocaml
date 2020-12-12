

(* ************** Declaration section ******************* *)


%token <float> CONST_FLOAT
%token <Cpx.ComplexWithFloats.t> COMPLEX
%token <string> VAR
%token PLUS MINUS TIMES DIV POWER
%token LPAREN RPAREN
%token EQUALS QUESTION_MARK
%token GIVE_VALUE
%token EOF

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left POWER             /* medium+ precedence */
%nonassoc UMINUS        /* highest precedence */


%start <External_types.parser_t> parse_input
%type <External_types.parser_t> equation_expr
%type <Solver.expr> formula_expr

%%

(* ************** rules section ******************* *)

parse_input:
    | e = equation_expr EOF     { e }

equation_expr:
    | x=VAR EQUALS f=formula_expr                           { Equation (x, f) }
    | f=formula_expr GIVE_VALUE                             { Expr f }
    | func=VAR LPAREN x=VAR RPAREN EQUALS f=formula_expr    { Function (func, x, f) }

formula_expr:
    | f = CONST_FLOAT                           { Literal_float f }
    | LPAREN e = formula_expr RPAREN            { e }
    | i = COMPLEX                               { Literal_complex i }
    | x = VAR                                   { Variable.get_variable x }
    | e1=formula_expr PLUS e2=formula_expr      { Plus (e1, e2) }
    | e1=formula_expr MINUS e2=formula_expr     { Minus (e1, e2) }
    | e1=formula_expr TIMES e2=formula_expr     { Times (e1, e2) }
    | e1=formula_expr DIV e2=formula_expr       { Div (e1, e2) }
    | e1=formula_expr POWER e2=formula_expr     { Power (e1, e2) }
    | MINUS e1=formula_expr %prec UMINUS        { Minus(Literal_float 0.0, e1) }

    
