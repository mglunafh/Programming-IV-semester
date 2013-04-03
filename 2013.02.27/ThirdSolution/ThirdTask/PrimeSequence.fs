module PrimeSequence

let isPrime n = 
    let rec f i = 
        if i * i > n then true 
        elif n % i = 0 then false
        else f (i + 1)
    f 2
    
let rec nextPrime n = if isPrime n then n else nextPrime (n + 1)

let primeSeq = 
    Seq.unfold (fun state -> let next = nextPrime state in Some (next, next + 1)) 2
