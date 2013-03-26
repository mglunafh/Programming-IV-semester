module Differentiation

type sign = Plus | Minus | Mul | Div  

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
    | Op (Mul, _, Const 0) -> Const 0
    | Op (Mul, left, Const n) -> Op (Mul, Const n, rebuild left)
    | Op (Mul, Const 0, _) -> Const 0
    | Op (Mul, Const 1, expr) -> rebuild expr
    | Const n -> Const n
    | Var name -> Var name
    | Op (sgn, left, right) -> Op (sgn, rebuild left, rebuild right)

let derive expression = 
    let rec der exp = 
        match (rebuild exp) with
        | Var _ -> Const 1
        | Const _ -> Const 0
        | Op (sgn, left, right) when sgn = Plus || sgn = Minus -> Op (sgn, der left , der right)
        | Op (Mul, Const n, right) -> Op (Mul, Const n, der right)
        | Op (Mul, left, right) -> 
            Op (Plus, Op (Mul, der left, right), Op (Mul, left, der right))
        | Op (Div, left, right) -> 
            Op (Div, Op (Minus, Op (Mul, der left, right), Op (Mul, left, der right)) , Op (Mul, right, right))
    der expression |> rebuild