module MapCreation

open Terrain
open GeneticOptimization
open Regions
open Resource
open HexTiling

let mkStartLocations n (terr : Terrain[,]) =
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


// Placement of resources

let mkResources (terr : Terrain[,]) =
    let probs =
        [| 0.033; 0.033; 0.033; 0.033; 0.01 |]

    let rscs =
        [| Oil; Wood; Iron; Factory; Airfield |]

    let harbour_prob = 0.05

    let uppers, _ =
        Array.zip probs rscs
        |> Array.fold (fun (uppers, s) (p, r) ->
            let s' = s + p
            ((s', r) :: uppers, s')
            )
            ([], 0.0)

    let rec getResource x uppers =
        match uppers with
        | (x', r) :: _ when x <= x'-> Some r
        | _ :: rest -> getResource x rest
        | [] -> None

    let isCoastal c =
        (getSq terr c) = Land
        &&
        c
        |> neighboursOfSq
        |> List.exists (fun c' -> getSq terr c' <> Land)

    let rnd = new System.Random()

    let resources =
        [|
            let width = getWidth terr
            let height = getHeight terr
            for x in 0..width-1 do
                for y in 0..height-1 do
                    let c = SquareCoords(x, y)
                    let v = rnd.NextDouble()
                    match getSq terr c with
                    | Land when isCoastal c && v <= harbour_prob ->
                        yield (c, Harbour)
                    | Land ->
                        match getResource v uppers with
                        | Some r -> yield (c, r)
                        | None -> ()
                    | Sea -> ()
        |]
    
    resources