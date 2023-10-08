(*
                         CS 51 Final Project
                           MiniML -- Parser
*)
                  
%{
  open Expr ;;
%}

%token EOF
%token OPEN CLOSE
%token LET DOT IN REC
%token NEG FLOATNEGATE
%token NOT
%token SINE COSINE TANGENT
%token NATURALLOG
%token PLUS FLOATPLUS MINUS FLOATMINUS
%token TIMES FLOATTIMES DIVIDE FLOATDIVIDE
%token POWER
%token EQUALS NOTEQUALS
%token LESSTHAN LESSTHANOREQUALS GREATERTHAN GREATERTHANOREQUALS
%token CONCAT
%token IF THEN ELSE 
%token FUNCTION
%token RAISE
%token <string> ID
%token <string> STRING
%token <int> INT 
%token <float> FLOAT
%token TRUE FALSE

%nonassoc IF
%left EQUALS NOTEQUALS
%left LESSTHAN LESSTHANOREQUALS GREATERTHAN GREATERTHANOREQUALS
%left PLUS FLOATPLUS MINUS FLOATMINUS
%left TIMES FLOATTIMES DIVIDE FLOATDIVIDE
%right POWER
%right CONCAT
%nonassoc NEG
%nonassoc FLOATNEGATE
%nonassoc NOT
%nonassoc SINE COSINE TANGENT
%nonassoc NATURALLOG

%start input
%type <Expr.expr> input

(* Grammar follows *)
%%
input:  exp EOF                 { $1 }

exp:    exp expnoapp            { App($1, $2) }
        | expnoapp              { $1 }

expnoapp: INT                   { Num $1 }
        | FLOAT                 { Float $1 }
        | STRING                { String $1 }
        | TRUE                  { Bool true }
        | FALSE                 { Bool false }
        | ID                    { Var $1 }
        | exp PLUS exp          { Binop(Plus, $1, $3) }
        | exp FLOATPLUS exp     { Binop(FloatPlus, $1, $3) }
        | exp MINUS exp         { Binop(Minus, $1, $3) }
        | exp FLOATMINUS exp    { Binop(FloatMinus, $1, $3) }
        | exp TIMES exp         { Binop(Times, $1, $3) }
        | exp FLOATTIMES exp    { Binop(FloatTimes, $1, $3) }
        | exp DIVIDE exp        { Binop(Divide, $1, $3) }
        | exp FLOATDIVIDE exp   { Binop(FloatDivide, $1, $3) }
        | exp POWER exp         { Binop(Power, $1, $3) }
        | exp EQUALS exp        { Binop(Equals, $1, $3) }
        | exp NOTEQUALS exp     { Binop(NotEquals, $1, $3) }
        | exp LESSTHAN exp      { Binop(LessThan, $1, $3) }
        | exp LESSTHANOREQUALS exp     { Binop(LessThanOrEqual, $1, $3) }
        | exp GREATERTHAN exp   { Binop(GreaterThan, $1, $3) }
        | exp GREATERTHANOREQUALS exp  { Binop(GreaterThanOrEqual, $1, $3) }
        | exp CONCAT exp        { Binop(Concat, $1, $3) }
        | NEG exp               { Unop(Negate, $2) }
        | FLOATNEGATE exp       { Unop(FloatNegate, $2) }
        | NOT exp               { Unop(Not, $2) }
        | SINE exp              { Unop(Sine, $2) }
        | COSINE exp            { Unop(Cosine, $2) }
        | TANGENT exp           { Unop(Tangent, $2) }
        | NATURALLOG exp        { Unop(NaturalLog, $2) }
        | IF exp THEN exp ELSE exp      { Conditional($2, $4, $6) }
        | LET ID EQUALS exp IN exp      { Let($2, $4, $6) }
        | LET REC ID EQUALS exp IN exp  { Letrec($3, $5, $7) }
        | FUNCTION ID DOT exp   { Fun($2, $4) } 
        | RAISE                 { Raise }
        | OPEN exp CLOSE        { $2 }
;

%%
