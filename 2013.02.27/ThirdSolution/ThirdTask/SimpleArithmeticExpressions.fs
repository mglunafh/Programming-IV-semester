module SimpleArithmeticExpressions

(* calculate a tree of arithmetic expression *)

type expr = 
    |Sum of expr * expr  
    |Subtr of expr * expr 
    |Mul of expr * expr 
    |Div of expr * expr
    |Minus of expr 
    |Value of int

let rec calculate expression = 
    match expression with 
    |Sum(left, right) -> calculate left + calculate right
    |Subtr(left, right) -> calculate left - calculate right
    |Mul(left, right) -> calculate left * calculate right
    |Div(left, right) -> calculate left / calculate right
    |Minus exp -> (0 - calculate exp) 
    |Value n -> n

// have a time to think about reading expressions from string.
