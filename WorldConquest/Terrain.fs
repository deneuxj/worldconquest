module Terrain

type Terrain =
    | Land
    | Sea

type InstallationCategory =
    | Town
    | Harbour
    | Airport

type Installation =
    {  owner : int option
       category : InstallationCategory  }

let makeTerrain (width : int) (height : int) (jagged_k) =
    let rnd = new System.Random()
    let nextPerturb(s) =
        (rnd.NextDouble() - 0.5
         |> float32)
        * s
        
    let size =
        Seq.initInfinite id
        |> Seq.scan (fun acc i -> 2*acc) 1
        |> Seq.filter (fun n -> n >= width && n >= height)
        |> Seq.head

    let area : float32[,] = Array2D.create size size (System.Single.NaN)
    area.[0, 0] <- 0.0f

    let mymod(i, j) =
        if i >= j then i - j
        elif i < 0 then i + j
        else i

    let getVal(i, j) =
        let i = mymod(i, size)
        let j = mymod(j, size)
        match area.[i, j] with
        | x when System.Single.IsNaN(x) -> sprintf "Read uninitialized tile %d %d" i j |> failwith
        | x -> x

    let growSquare (step : int) (pert_size : float32) =
        for i in step/2..step..size-1 do
            for j in step/2..step..size-1 do
                let i0 = i - step/2
                let i1 = i + step/2
                let j0 = j - step/2
                let j1 = j + step/2
                let mid_val =
                    (getVal(i0, j0)
                     + getVal(i0, j1)
                     + getVal(i1, j0)
                     + getVal(i1, j1)) / 4.0f
                    + nextPerturb(pert_size)
                area.[i, j] <- mid_val

    let growDiamond (step : int) (pert_size : float32) =
        let mutable flip = true
        for i in 0..step/2..size-1 do
            let offset = if flip then step/2 else 0
            flip <- not flip

            for j in offset..step..size-1 do
                let i0 = i - step/2
                let i1 = i + step/2
                let j0 = j - step/2
                let j1 = j + step/2
                let mid_val =
                    (getVal(i, j0)
                     + getVal(i0, j)
                     + getVal(i1, j)
                     + getVal(i, j1)) / 4.0f
                    + nextPerturb(pert_size)
                area.[i, j] <- mid_val

    let rec work step pert_size =
        if step > 1 then
            growSquare step pert_size
            growDiamond step pert_size
            work (step / 2) (pert_size * jagged_k)

    work size jagged_k

    area

let findLevel (land_part : float32) terr =
    let total = (Array2D.length1 terr) * (Array2D.length2 terr)

    let objective = land_part * (float32 total) |> int
    let min_dist = 0.01f

    let terr =
        [| for i in 0..(Array2D.length1 terr)-1 do
            for j in 0..(Array2D.length2 terr)-1 do
                yield terr.[i, j] |]

    let rec work(lower, higher) =
        let level = (lower + higher) / 2.0f
        if higher - lower <= min_dist then level
        else
            let landmass =
                terr
                |> Seq.filter (fun h -> h >= level)
                |> Seq.length
            if landmass < objective then
                work(lower, level)
            else
                work(level, higher)
    
    work(-1000.0f, 1000.0f)

let toTerrain (level) (terr) =
    terr
    |> Array2D.map (fun h -> if h >= level then Land else Sea)
