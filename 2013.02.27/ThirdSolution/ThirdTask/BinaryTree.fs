module BinaryTree

type tree = Node of int * tree * tree | Leaf of int | None

let rec height tr = 
    match tr with
    |Node(_, right, left) -> 1 + max (height right) (height left) 
    |Leaf _ -> 1
    |None -> 0
