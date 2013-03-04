(* calculate a tree of arithmetic expression *)

type expr = Sum of expr * expr |
	    Subtr of expr * expr |
	    Mul of expr * expr |
	    Div of expr * expr |
	    Minus of expr |
	    Value of int;;

let rec calculate expression = match expression with 
|Sum(left, right) -> calculate left + calculate right
|Subtr(left, right) -> calculate left - calculate right
|Mul(left, right) -> calculate left * calculate right
|Div(left, right) -> calculate left / calculate right
|Minus exp -> (0 - calculate exp) 
|Value n -> n;;

let expr' = 
Mul(
  Sum(
    Subtr(
      Value 1, 
      Sum (Value 4, Value 2)),
    Mul(
      Value 7, 
      Mul (Value 4, Value 2))),
  Mul(  
    Subtr(
      Value 8, 
      Sum (Value 3, Value 6)), 
    Subtr(
      Value 0,
      Sum (Value 2, Value 1))));;
                               

print_int (calculate expr');