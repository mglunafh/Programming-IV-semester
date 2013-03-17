module TreeMap

type sign = 
    | Plus = '+' 
    | Minus = '-' 
    | Mul = '*' 
    | Div = '/' 

type expr = 
    | Op of sign * expr * expr
    | Var of string
    | Const of int

let rec rebuild expression = 
    let apply sgn n1 n2 = 
        match sgn with
        | Plus -> n1 + n2
        | Minus -> n1 - n2
        | Mul -> n1 * n2
        | Div -> n1 / n2
        
    match expression with
    | Op (sgn, Const n1, Const n2) -> Const (apply sgn n1 n2)
    | Op (Mul, left, Const n) -> Op (Mul, Const n, left)
    | Op (Mul, Const 0, _) -> Const 0
    | Op (Mul, Const 1, expr) -> rebuild expr
    | Const n -> Const n
    | Var name -> Var name
    | Op (sign, left, right) -> Op (sign, rebuild left, rebuild right)


    