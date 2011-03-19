module Regions

open Terrain
open HexTiling

type RegionInfo =
    { id : int
      category : Terrain
      size : int }

let markRegions (terr : Terrain[,]) =
    let width = Array2D.length1 terr
    let height = Array2D.length2 terr

    let nbrsOf (SquareCoords(i, j) as c) =
        c
        |> neighboursOfSq
        |> List.filter (fun (SquareCoords(i, j)) -> j >= 0 && j < height)
        |> List.map (fun (SquareCoords(i, j)) -> SquareCoords((if i < 0 then width + i elif i >= width then i - width else i), j))

    let marked =
        terr
        |> Array2D.map (fun _ -> -1)

    let markedIsFree(x, y) = marked.[x, y] = -1
    let mark(x, y, id) = marked.[x, y] <- id

    let paintFill c0 id =
        let mutable num_tiles = 0
        let mutable working = [ c0 ]
        while not <| List.isEmpty working do
            let (SquareCoords(x, y) as c) = List.head working
            working <- List.tail working
            if markedIsFree(x, y) then
                num_tiles <- num_tiles + 1
                mark(x, y, id)
                let terrain_type = terr.[x, y]
                let same_ngbh =
                    c
                    |> nbrsOf
                    |> List.filter (fun (SquareCoords(x', y')) -> terr.[x', y'] = terrain_type && markedIsFree(x', y'))
                working <- working @ same_ngbh
        num_tiles

    let id = ref 0
    let area_infos =
        [|
            for y in 0..height-1 do
                for x in 0..width-1 do
                    if markedIsFree(x, y) then
                        let num_tiles = paintFill (SquareCoords(x, y)) !id
                        yield { id = !id; category = terr.[x, y]; size = num_tiles }
                        id := !id + 1
        |]

    marked, area_infos