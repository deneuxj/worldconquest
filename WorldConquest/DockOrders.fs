module DockOrders

open GameState
open Orders
open Units

type DockOrder =
    | DockOrLand of UnitIndex

let extractDockOrders (units : UnitInfo[]) (orders : Order[]) =
    playerUnitZipMap
        (fun (idx, _, order) ->
            match order with
            | LandAt _ | DockAt _ -> [| DockOrLand idx |]
            | _ -> Array.empty)
        units
        orders

    |> Array.concat

let applyDockOrders (orders : DockOrder[]) (idx0_and_units : (UnitIndex option * UnitInfo)[]) =
    let orders =
        orders
        |> Seq.map (function DockOrLand idx -> (idx, ()))
        |> dict

    [|
        for (idx0, unit) in idx0_and_units do
            yield
                match idx0 with
                | Some idx0 as v ->
                    match orders.TryGetValue(idx0) with
                    | false, _ -> (v, unit)
                    | true, _ ->
                        v,
                        match unit with
                        | { specific = Transport(_, units) } as v -> { v with specific = Transport(Docked, units) }
                        | { specific = Destroyer(_) } as v -> { v with specific = Destroyer(Docked) }
                        | { specific = Submarine(_, stealth) } as v -> { v with specific = Submarine(Docked, stealth) }
                        | { specific = Battleship(_) } as v -> { v with specific = Battleship(Docked) }
                        | { specific = Carrier(_, units) } as v -> { v with specific = Carrier(Docked, units) }
                        | { specific = Fighter(_, Fuel f) } as v-> { v with specific = Fighter(Landed.Landed, Fuel (f - 1)) }
                        | { specific = Bomber(_, Fuel f, units) } as v -> { v with specific = Bomber(Landed.Landed, Fuel (f - 1), units) }
                        | _ -> failwith <| sprintf "Unit %A cannot dock" unit
                | None ->
                    (None, unit)
    |]