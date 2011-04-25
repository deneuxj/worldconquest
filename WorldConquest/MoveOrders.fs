module MoveOrders

open Units
open GameState
open HexTiling

type MoveOrder =
    | LateMove of int * HexCoords list
    | EarlyMove of int * HexCoords list

let extractMoveOrders (units : UnitInfo[]) (player : int) (orders : Order[]) =
    let getUnitOrder ((idx : UnitIndex, u : UnitInfo), order : Order) =
        match idx with
        | Root idx ->
            match order with
            | Order.Conquer path     
            | Order.DockAt path
            | Order.LandAt path
            | Order.Load (path, _)
            | Order.Move path
            | Order.Unload (path, _) -> [| LateMove(idx, path) |]
            
            | Order.Bomb (_, path)
            | Order.DirectAttack (_, path) -> [| EarlyMove(idx, path) |]

            | _ -> Array.empty
        | _ -> Array.empty
            
    let fetchUnitOrder = mkFetchOrderMap getUnitOrder orders
    playerUnitMap fetchUnitOrder (fun _ -> true) units
    |> Array.concat
