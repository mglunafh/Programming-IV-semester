open List;;

type tree = Node of int * tree * tree | Leaf of int | None;;

let rec height tr = match tr with
|Node(_, right, left) -> 1 + max (height right) (height left) 
|Leaf _ -> 1
|None -> 0;;

let tr = Node(3, 
  Node(1, 
    Node(4, Leaf 2, None), 
    Node(7, Leaf 4, Leaf 2)), 
  Node(8, 
    Node(3, Leaf 6, None), 
    Node(0, Leaf 2, Leaf 1) ));;

print_int (height tr);
 