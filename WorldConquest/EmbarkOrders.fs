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
        let childOf (parent : UnitIndex, sub : int) =
            match parent with
            | Root idx -> Transported(idx, sub)
            | Transported (root, idx) -> Transported2(root, idx, sub)
            | Transported2 _ -> failwith "Transported2 cannot have children"

        let getLateOrder ((idx : UnitIndex, u : UnitInfo), order : Order) =
            match idx, order with
            | _, Order.Unload (_, target) ->
                match u.specific with
                | UnitTypes.Transport(_, transported) ->
                    [|
                        for sub in 0 .. transported.Length - 1 do
                            yield Disembark(childOf(idx, sub), [target])
                    |]
                | UnitTypes.Carrier(_, transported) ->
                    [|
                        for sub in 0 .. transported.Length - 1 do
                            yield Disembark(childOf(idx, sub), [target])
                    |]
                | UnitTypes.Bomber(_, _, BomberTransport.Infantry _) ->
                    [| Disembark(childOf(idx, 0), [target]) |]
                | UnitTypes.Bomber(_, _, BomberTransport.Bombs _)
                | UnitTypes.AntiAircraft
                | UnitTypes.Artillery
                | UnitTypes.Battleship _
                | UnitTypes.Destroyer _
                | UnitTypes.Fighter _
                | UnitTypes.Infantry
                | UnitTypes.Submarine _
                | UnitTypes.Tank -> failwith <| sprintf "Cannot unload all from %A" u
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