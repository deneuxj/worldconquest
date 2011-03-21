module GameState

open Units
open HexTiling

type Order =
    | Move of HexCoords
    | Bombard of HexCoords
    | Unload of HexCoords
    | Load of int // Id of the unit
    | DirectAttack of HexCoords
    | Conquer of HexCoords
    | DoNothing

let moveUnits (units : UnitInfo[]) (destinations : HexCoords option []) =
    let moveUnit (u : UnitInfo) (dest : HexCoords option) =
        match dest with
        | Some dest ->
            { u with coords = dest }
        | None -> u
    Array.map2 moveUnit units destinations