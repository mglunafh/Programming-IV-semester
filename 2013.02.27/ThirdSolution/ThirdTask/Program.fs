open Uniqueness
open EvenNumbers
open BinaryTree
open First
open SimpleArithmeticExpressions

let t = 15
printf "2)%b %b \n" (unique [1; 4; 8; 8]) (unique [1; 3; 4; 9])
printf "3)%d %d %d\n" (first list) (second list) (third list)

let binTree = 
    Node(3, 
        Node(1,  
            Node(4, Leaf 2, None), 
            Node(7, Leaf 4, Leaf 2)), 
        Node(8, 
            Node(3, Leaf 6, None), 
            Node(0, Leaf 2, Leaf 1)))
printf "4)da height of da tree is %d\n" (height binTree)

printf "1)позиция, на которой что-то там максимально - %d\n" (``пробел в названии`` [1; 4; 8; 8])

let expression = 
    Mul
        (Sum
            (Subtr
                (Value 1, 
                Sum (Value 4, Value 2)),
            Mul
                (Value 7, 
                Mul (Value 4, Value 2))),
        Mul
            (Subtr
                (Value 8, 
                Sum (Value 3, Value 6)), 
            Subtr
                (Value 0,
                Sum (Value 2, Value 1))))

printf "5)our expression equals to %d\n" (calculate expression);