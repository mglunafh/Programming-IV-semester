module Iterator

type 'a tree = Node of 'a * 'a tree * 'a tree | Leaf of 'a
    
type Iterator(tree) =
    let rec linearization tr = 
        match tr with 
        | Node(value, left, right) -> value :: (linearization left @ linearization right)
        | Leaf value -> [value]

    let mutable representation = (Seq.ofList << linearization) tree
    member it.Next = 
        let top = Seq.head representation
        representation <- Seq.skip 1 representation
        top
    member it.HasNext = not (Seq.isEmpty representation)   