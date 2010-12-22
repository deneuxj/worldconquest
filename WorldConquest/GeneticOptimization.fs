module GeneticOptimization

type Parameters<'TIndividual> =
    abstract member Evaluate: 'TIndividual -> float32
    abstract member Mix: 'TIndividual * 'TIndividual -> 'TIndividual
    abstract member Mutate: 'TIndividual -> 'TIndividual

let findBest (pars: Parameters<'T>) (goal : float32) (max_iters : int) (inds : 'T[]) =
    let num_good = inds.Length / 3
    let rnd = new System.Random()
    let getCouple() =
        let mutable ok = false
        let mutable res = (0,0)
        while (not ok) do
            res <- (rnd.Next(num_good), rnd.Next(num_good))
            ok <- fst res <> snd res
        res

    let inds = Array.copy inds

    let rec work iter =
        Array.sortInPlaceBy (fun x -> -pars.Evaluate(x)) inds

        let best = inds.[0]

        if iter >= max_iters || pars.Evaluate(best) >= goal then
            best
        else
            for i in num_good..inds.Length-1 do
                let A,B = getCouple()
                inds.[i] <- pars.Mix(inds.[A], inds.[B]) |> pars.Mutate
            work (iter+1)

    work 0

let mkEvenlySpaced (dist : 'T * 'T -> float32) (violatedConstraints : 'T[] -> int) (violation_cost) (mutate) =
    let rnd = new System.Random()

    let minDist dist arr i x =
        arr
        |> Array.mapi (fun j v -> if i = j then System.Single.PositiveInfinity else dist(v, arr.[i]))
        |> Seq.min

    { new Parameters<'T[]> with
        member this.Evaluate(x) =
            let bad = violatedConstraints x
            if bad > 0 then
                bad * violation_cost |> float32
            else
                x
                |> Array.mapi (minDist dist x)
                |> Seq.sum

        member this.Mix(x, y) =
            [|
                for i in 0..x.Length-1 do
                    yield if rnd.Next()%2 = 0 then x.[i] else y.[i]
            |]

        member this.Mutate(x) =
            x
            |> Array.map (fun v -> if rnd.Next()%5 = 0 then mutate v else v)
    }

