module MoveOrders

open Units
open GameState
open HexTiling
open Orders
open AttackOrders

type MoveOrder =
    | LateMove of int * HexCoords list
    | EarlyMove of int * HexCoords list

let extractMoveOrders (units : UnitInfo[]) (player : int) (orders : Order[]) =
    let getUnitOrder (idx : UnitIndex, u : UnitInfo, order : Order) =
        
        match idx with
        | Root idx ->
            match order with
            | Order.Load { path = path; unit = unit }
            | Order.Move { path = path; unit = unit } ->
                if unit <> Root idx then failwith <| sprintf "UnitIndex inconsistency: %A <> Root %A" unit idx
            | _ -> ()

            match order with
            | Order.Conquer (target, path) ->
                [| LateMove(idx, (target :: (List.rev path)) |> List.rev) |]
            | Order.DockAt path
            | Order.LandAt path
            | Order.Load { path = path }
            | Order.Move { path = path }
            | Order.Unload (path, _) -> [| LateMove(idx, path) |]
            
            | Order.Bomb (_, path)
            | Order.DirectAttack (_, path) -> [| EarlyMove(idx, path) |]

            | _ -> Array.empty
        | _ -> Array.empty
            
    playerUnitZipMap getUnitOrder units orders
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
                | true, [] ->
                    match units.[i] with
                    | { specific = Fighter(landed, Fuel f) } as u -> { u with specific = Fighter(landed, Fuel (f - 1)) }
                    | { specific = Bomber(landed, Fuel f, units) } as u -> { u with specific = Bomber(landed, Fuel (f - 1), units) }
                    | u -> u
                | true, (_ :: _ as path) ->
                    let dest = path |> List.rev |> List.head
                    match units.[i] with
                    | { specific = Fighter(landed, Fuel f) } as u ->
                        { u with
                            coords = dest ;
                            specific = Fighter(landed, Fuel (f - 1)) }
                    | { specific = Bomber(landed, Fuel f, units) } as u ->
                        { u with
                            coords = dest ;
                            specific = Bomber(landed, Fuel (f - 1), units) }
                    | u ->                
                        { u with coords = dest }
    |]