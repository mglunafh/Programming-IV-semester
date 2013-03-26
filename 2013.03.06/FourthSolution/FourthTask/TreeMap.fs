module Tree

type 'a tree  = Node of 'a * 'a tree * 'a tree | Leaf of 'a 

let rec map f tr = 
    match tr with 
    | Node (a, left, right) -> Node (f a, map f left, map f right)
    | Leaf a -> Leaf (f a)