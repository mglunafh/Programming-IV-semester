type RoundingBuilder(n : int) = 
    member this.Bind(x : float, rest : float -> float) = rest x
    member this.Return(x : float) = System.Math.Round(x, n)