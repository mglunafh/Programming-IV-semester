type RoundingBuilder(n : int) = 
    member this.Bind(x : float, rest : float -> float) = rest (System.Math.Round(x, n))
    member this.Return(x : float) = System.Math.Round(x, n)

let rounding = fun n -> RoundingBuilder n
