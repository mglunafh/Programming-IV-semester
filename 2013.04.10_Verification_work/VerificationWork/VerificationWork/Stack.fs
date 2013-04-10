
type Stack (initStack) = 
    let mutable mutStack = initStack
    
    member s.Push value =
        mutStack <- value :: mutStack
    
    member s.Pop = 
        match mutStack with 
        |top :: rest -> 
            mutStack <- rest
            top
        |[] -> failwith "attempt to call method 'Stack.Pop' from empty stack."
    
    member s.IsEmpty = (List.length mutStack = 0)
    new() = Stack []