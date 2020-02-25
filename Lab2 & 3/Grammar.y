{ 
module Grammar where 
import Tokens 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token 
    let { TokenLet a } 
    in  { TokenIn a } 
    int { TokenInt a $$ } 
    var { TokenVar a $$ } 
    '=' { TokenEq a } 
    '+' { TokenPlus a } 
    '-' { TokenMinus a } 
    '*' { TokenTimes a } 
    '/' { TokenDiv a } 
    '^' {TokenExp a }
    '(' { TokenLParen a } 
    ')' { TokenRParen a } 

%right in 
%left '^'
%left '+' '-' 
%left '*' '/' 
%left NEG 
%% 
Exp : let var '=' Exp in Exp { Let $2 $4 $6 } 
    | Exp '+' Exp            { Plus $1 $3 } 
    | Exp '-' Exp            { Minus $1 $3 } 
    | Exp '*' Exp            { Times $1 $3 } 
    | Exp '/' Exp            { Div $1 $3 }
    | Exp '^' Exp            { Expon $1 $3}	
    | '(' Exp ')'            { $2 } 
    | '-' Exp %prec NEG      { Negate $2 } 
    | int                    { Int $1 } 
    | var                    { Var $1 } 
    
{ 
parseError :: [Token] -> a
parseError [] = error "Parse error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Exp = Let String Exp Exp 
         | Plus Exp Exp 
         | Minus Exp Exp 
         | Times Exp Exp 
         | Div Exp Exp 
         | Expon Exp Exp
         | Negate Exp
         | Int Int 
         | Var String 
         deriving Show 
} 