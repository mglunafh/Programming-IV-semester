module Iterator
open System.Collections.Generic

type 'a tree = 
    |Node of 'a * 'a tree * 'a tree  
    |Leaf of 'a
    
type TreeIterator<'a>(tree) =
    let rec linearization tr = 
        match tr with 
        | Node(value, left, right) -> value :: (linearization left @ linearization right)
        | Leaf value -> [value]

    let mutable representation = linearization tree
    
    interface IEnumerator<'a> with
        member it.Current = List.head (representation : 'a list)            // what are these two lines 
        member it.get_Current() = List.head representation :> obj           // designed for?
        member it.MoveNext() =  
            representation <- List.tail representation
            false = List.isEmpty representation
            
        member it.Dispose() = () 
        member it.Reset() = 
            representation <- linearization tree

type 'a IterableTree(tree) =
    interface IEnumerable<'a> with
        member tr.GetEnumerator() = new TreeIterator<'a>(tree) :> System.Collections.IEnumerator
        member tr.GetEnumerator() = new TreeIterator<'a>(tree) :> IEnumerator<'a>


let datTree =  
    Node(1, 
        Node(4, 
            Node(8, 
                Leaf 1, 
                Node (5, Leaf 4, Leaf 2)),
            Node(3, 
                Leaf 7, 
                Node(64, Leaf 4, Leaf 2))),
        Node(16, 
            Node (42, 
                Leaf 8, 
                Node(5, Leaf 3, Leaf 6)), 
            Node(11, 
                Leaf 0,
                Node(5, Leaf 2, Leaf 1))))

let datIterableTree = new IterableTree<int>(datTree)
for nodes in datIterableTree do printf "%d " nodes                          // shoulda show "4 8 1 5 4 2 3 7 64 4 2 16 42 8 5 3 6 11 0 5 2 1 "