module DegreeRegions

open Terrain
open HexTiling

let markRegions (terr : Terrain[,]) =
    let width = Array2D.length1 terr
    let height = Array2D.length2 terr

    let nbrsOf (i, j) =
        (i, j)
        |> SquareCoords
        |> neighboursOfSq
        |> List.filter (fun (SquareCoords(i, j)) -> j >= 0 && j < height)
        |> List.map (fun (SquareCoords(i, j)) -> (if i < 0 then width + i elif i >= width then i - width else i), j)

    let land_ratios =
        terr
        |> Array2D.mapi (fun i j _ ->
            let nbrs = nbrsOf(i, j)
            let n_sea =
                nbrs
                |> Seq.filter (fun (i,j) -> terr.[i,j] = Sea)
                |> Seq.length
            let n_land =
                nbrs
                |> Seq.filter (fun (i, j) -> terr.[i,j] = Land)
                |> Seq.length
            (float32 n_sea) / (float32 n_land)
        )

    let regions = terr |> Array2D.map (fun _ -> -1)

    // TODO
    let paintFill c0 id =
        let mutable working = [ c0 ]
        while
            match working with
            | [] -> false
            | _ -> true
            do
            ()                
    regions