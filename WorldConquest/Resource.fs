module Resource

open GeneticOptimization
open Terrain

type Resource =
    | Oil
    | Wood
    | Iron

let mkSources n (terr : Terrain[,]) =
    let w = Array2D.length1 terr
    let h = Array2D.length2 terr

    let rnd = new System.Random()
    let nextCoord() =
        (rnd.Next(h), rnd.Next(w))

    let initial =
        Array.init 50 (fun _ ->
            Array.init n (fun _ -> nextCoord()))

    let mutate (x,y) =
        let mag = 10
        let clamp x = if x < 0 then x+w elif x >= w then x - w else x

        let x' = x + rnd.Next(mag) - mag/2
        let y' = y + rnd.Next(mag) - mag/2

        (clamp x', clamp y')

    let wrapped_dist ((x0, y0), (x1, y1)) =
        let sq x = x*x
        let f s t w =
            let d = abs(s-t)
            let d' = abs(w - d)
            min d d'
        let dx = f x0 x1 w
        let dy = f y0 y1 h 

        sq dx + sq dy
        |> float32

    let opt_params =
        let violation_cost = -100000
        mkEvenlySpaced
            wrapped_dist
            (fun coords ->
                coords
                |> Seq.filter (fun (x, y) -> terr.[x,y] <> Land)
                |> Seq.length)
            violation_cost
            mutate

    let locations = findBest opt_params 1000000.0f 1000 initial
    locations
