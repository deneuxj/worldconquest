﻿module MoveOrders

open Units
open GameState
open HexTiling
open Orders
open AttackOrders

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

let filterDeadMoveOrders (isDead : UnitIndex -> bool) (orders : MoveOrder[]) =
    orders
    |> Array.filter
        (function
         | LateMove (idx, _) -> not (isDead(Root idx))
         | EarlyMove _ -> true)

let applyMoves (orders : MoveOrder[]) (units : UnitInfo[]) =
    let orders =
        orders
        |> Seq.map
            (function
             | LateMove (id, path) -> (id, path)
             | EarlyMove (id, path) -> (id, path))
        |> dict

    [|
        for i in 0 .. units.Length - 1 do
            yield
                match orders.TryGetValue(i) with
                | false, _
                | true, [] -> units.[i]
                | true, (_ :: _ as path) -> { units.[i] with coords = path |> List.rev |> List.head }
    |]