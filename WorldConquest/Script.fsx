// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#load "GeneticOptimization.fs"
#load "HexTiling.fs"
#load "Terrain.fs"
#load "Resource.fs"

open Terrain
open Resource

let printTerr (terr : Terrain[,]) (locations : (int*int) seq) =
    let locs = Set.ofSeq locations
    let terr =
        terr
        |> Array2D.mapi (fun i j t ->
            if Set.contains (i, j) locs then
                "X"
            else
                match t with
                Land -> "*"
                | _ -> " ")

    for j in 0..(Array2D.length2 terr)-1 do
        for i in 0..(Array2D.length1 terr)-1 do
            printf "%s" terr.[i,j]
        printfn ""

let newTerrain() =
    let jagged_k = 0.5f
    let land_mass = 0.25f

    let heights = makeTerrain 64 64 jagged_k
    let sea_level = findLevel land_mass heights
    let terr = toTerrain sea_level heights

    let resources = mkSources 5 terr

    printTerr terr resources
