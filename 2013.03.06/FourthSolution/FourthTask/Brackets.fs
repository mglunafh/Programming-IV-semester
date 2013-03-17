module Brackets

let isCorrect (str : string) = 
    let rec f stack i = 
        if i < str.Length then   
            match (stack, str.[i]) with
            | ('(' :: tl, ')') -> f tl (i + 1)
            | ('[' :: tl, ']') -> f tl (i + 1)
            | ('{' :: tl, '}') -> f tl (i + 1)
            | (_, x) when x = ')' || x = ']' || x = '}' -> false
            | (_, x) when x = '(' || x = '[' || x = '{' -> f (x :: stack) (i + 1) 
            | _ -> f stack (i + 1)
        else
            if stack = [] then true else false
    f [] 0