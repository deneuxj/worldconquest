module EmbarkOrders

open GameState
open Orders
open Units
open HexTiling

type EmbarkOrder =
    { transporter : int  // Index of a root unit
      unit : UnitIndex } // The unit embarking

let extractEmbarkOrders (units : UnitInfo[]) (player : int) (orders : Order[]) =
    let getUnitOrder ((idx : UnitIndex, u : UnitInfo), order : Order) =
        match order with
        | Order.Load (path, root_unit) -> [| { transporter = root_unit; unit = idx } |]
        | _ -> Array.empty

    let fetchUnitOrder = mkFetchOrderMap getUnitOrder orders
    playerUnitMap fetchUnitOrder (fun _ -> true) units
    |> Array.concat

let filterDeadEmbarkOrders (isDead : PlayerId * UnitIndex -> bool) (player : int) (orders : EmbarkOrder[]) =
    let player = PlayerId player
    orders
    |> Array.filter (fun order -> not (isDead(player, Root order.transporter) || isDead(player, order.unit)))


type DisembarkOrder = Disembark of UnitIndex * HexCoords list

let extractDisembarkOrders (units : UnitInfo[]) (player : int) (orders : Order[]) =
    let early_orders =
        let getEarlyOrder ((idx : UnitIndex, u : UnitInfo), order : Order) =
            match idx with
            | Transported _
            | Transported2 _ ->
                match order with
                | Order.DirectAttack (_, path)
                | Order.Bomb (_, path) ->
                    [| Disembark (idx, path) |]
                | _ -> Array.empty
            | _ -> Array.empty

        let fetchUnitOrder = mkFetchOrderMap getEarlyOrder orders
        playerUnitMap fetchUnitOrder (fun _ -> true) units
        |> Array.concat

    let late_orders =
        let getLateOrder ((idx : UnitIndex, u : UnitInfo), order : Order) =
            match idx, order with
            | _, Order.Unload (_, target) -> [| Disembark (idx, [target]) |]
            | Transported _, Order.Move path
            | Transported2 _, Order.Move path -> [| Disembark (idx, path) |]
            | _ -> Array.empty

        let fetchUnitOrder = mkFetchOrderMap getLateOrder orders
        playerUnitMap fetchUnitOrder (fun _ -> true) units
        |> Array.concat

    (early_orders, late_orders)

let filterDeadDisembarkOrders (isDead : PlayerId * UnitIndex -> bool) (player : int) (orders : DisembarkOrder[]) =
    let player = PlayerId player
    orders
    |> Array.filter (fun (Disembark(idx, _)) -> not (isDead(player, idx)))