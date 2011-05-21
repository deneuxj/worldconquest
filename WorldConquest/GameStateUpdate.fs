module GameStateUpdate

open System.Collections.Generic

open GameState
open Orders
open MoveOrders
open EmbarkOrders
open AttackOrders
open DockOrders
open ConquerOrders
open Units

let growUnitTree (embark : EmbarkOrder[]) (disembark : DisembarkOrder[]) (units : UnitInfo[]) =
    let embark =
        embark
        |> Seq.groupBy (fun embark -> embark.transporter)
        |> dict
    
    [|
        for i in 0 .. units.Length - 1 do
            match embark.TryGetValue(i) with
            | true, orders ->
                let destination_unit = units.[i]
                match destination_unit.specific with
                | UnitTypes.Transport(docked, transported) ->
                    let extra_transported =                    
                        orders
                        |> Seq.map (fun embark -> getUnitByIndex units embark.unit)
                        |> Seq.map
                            (fun unit ->
                                match unit.specific with
                                | UnitTypes.Infantry -> TransportedUnit.Infantry (Health unit.health)
                                | UnitTypes.Tank -> TransportedUnit.Tank (Health unit.health)
                                | UnitTypes.Artillery -> TransportedUnit.Artillery (Health unit.health)
                                | UnitTypes.AntiAircraft -> TransportedUnit.AntiAircraft (Health unit.health)
                                | UnitTypes.Battleship _
                                | UnitTypes.Carrier _
                                | UnitTypes.Destroyer _
                                | UnitTypes.Submarine _
                                | UnitTypes.Bomber _
                                | UnitTypes.Fighter _
                                | UnitTypes.Transport _ -> failwith <| sprintf "Cannot load %A into a transport" unit)

                        |> Array.ofSeq
                    yield (Some(Root i),
                           { destination_unit with
                                specific = UnitTypes.Transport(docked, Array.concat [transported; extra_transported]) })

                | UnitTypes.Bomber(landed, fuel, transported) as bomber ->
                    match List.ofSeq orders with
                    | [] -> yield (Some(Root i), destination_unit)
                    | [{ unit = unit }] ->
                        match transported, getUnitByIndex units unit with
                        | BomberTransport.Bombs _, { health = health; specific = Infantry } ->
                            yield (Some(Root i),
                                   { destination_unit with
                                        specific = UnitTypes.Bomber(landed, fuel, BomberTransport.Infantry(Health health)) })
                        | BomberTransport.Bombs _, unit ->
                            failwith <| sprintf "Cannot load %A into a bomber" unit
                        | bomber, _ ->
                            failwith <| sprintf "Bomber %A already carries a unit" bomber
                    | _ :: _ ->
                        failwith "Cannot load more than one unit into a bomber"

                | UnitTypes.Carrier(docked, transported) as carrier ->
                    let extra_transported =
                        orders
                        |> Seq.map (fun embark -> getUnitByIndex units embark.unit)
                        |> Seq.map
                            (fun unit ->
                                match unit.specific with
                                | UnitTypes.Fighter _ -> CarriedAircraft.Fighter(Health unit.health)
                                | UnitTypes.Bomber(_, _, lifted) -> CarriedAircraft.Bomber(lifted, Health unit.health)
                                | UnitTypes.AntiAircraft
                                | UnitTypes.Artillery
                                | UnitTypes.Battleship _
                                | UnitTypes.Carrier _
                                | UnitTypes.Destroyer _
                                | UnitTypes.Infantry
                                | UnitTypes.Submarine _
                                | UnitTypes.Tank
                                | UnitTypes.Transport _ -> failwith <| sprintf "Cannot load %A onto a carrier" unit)
                        |> Array.ofSeq
                    yield (Some(Root i),
                           { destination_unit with
                                specific = UnitTypes.Carrier(docked, Array.concat [transported; extra_transported]) })

                | UnitTypes.Infantry
                | UnitTypes.Tank
                | UnitTypes.Artillery
                | UnitTypes.AntiAircraft
                | UnitTypes.Battleship _
                | UnitTypes.Destroyer _
                | UnitTypes.Submarine _
                | UnitTypes.Fighter _ -> failwith <| sprintf "Cannot load units into %A" units.[i]

            | false, _ -> yield (Some (Root i), units.[i])

        for Disembark(index, path) in disembark do
            let unit = getUnitByIndex units index
            yield
                (Some index,
                 match path with
                 | [] -> unit
                 | _ :: _ ->
                    let coords = path |> List.rev |> List.head
                    { unit with coords = coords })
    |]

let shrinkUnitTree (embark : EmbarkOrder[]) (disembark : DisembarkOrder[]) (idx_and_units : (UnitIndex option * UnitInfo)[]) =
    let to_remove =
        Array.concat
            [
                embark
                |> Array.map (fun embark -> embark.unit)
 
                disembark
                |> Array.map (fun (Disembark (unit, _ )) -> unit)
            ]

    let to_remove =
        to_remove
        |> Seq.map
            (function
             | Root idx -> (idx, None)
             | Transported (root, sub) -> (root, Some(sub, None))
             | Transported2 (root, sub, sub2) -> (root, Some(sub, Some sub2)))
        |> XNAUtils.SeqUtil.groupPairs
        |> Seq.map (fun (k, vs) -> 
            (k,
             vs
             |> Seq.choose id
             |> XNAUtils.SeqUtil.groupPairs
             |> List.ofSeq
             |> List.map (fun (k, vs) -> (k, vs |> Seq.choose id |> List.ofSeq))
            )
        )
        |> dict

    [|
        for root in 0 .. idx_and_units.Length - 1 do
            let idx0, unit = idx_and_units.[root]
            if not unit.IsDead then
                match to_remove.TryGetValue(root) with
                | false, _ -> yield (idx0, unit)
                | true, [] -> () // Root itself was removed
                | true, subs ->
                    let subs = dict subs
                    match unit.specific with
                    | UnitTypes.Transport(docked, transported) ->
                        yield (idx0,
                               { unit with specific =
                                            UnitTypes.Transport(docked, 
                                                [| for sub in 0 .. transported.Length - 1 do
                                                    if transported.[sub].Health > 0.0f then
                                                        match subs.TryGetValue(sub) with
                                                        | false, _ -> yield transported.[sub]
                                                        | true, [] -> ()
                                                        | true, vs -> failwith <| sprintf "There are bad UnitIndex in %A" vs |]) })
                    | UnitTypes.Bomber(landed, fuel, BomberTransport.Infantry (Health h)) ->
                        if h > 0.0f then
                            match subs.TryGetValue(0) with
                            | true, [] ->
                                yield (idx0, { unit with specific =
                                                            UnitTypes.Bomber(landed, fuel, BomberTransport.Bombs 0) })
                            | _ -> failwith <| sprintf "Bad unit indices %A for the content of a root bomber" unit
                    | UnitTypes.Bomber(_, _, BomberTransport.Bombs _) ->
                        failwith "Cannot remove content of bomber transporting bombs"
                    | UnitTypes.Carrier(docked, aircrafts) ->
                        let aircrafts' =
                            [| for sub in 0 .. aircrafts.Length - 1 do
                                if aircrafts.[sub].Health > 0.0f then
                                    match subs.TryGetValue(sub) with
                                    | false, _ -> yield aircrafts.[sub]
                                    | true, [] -> ()
                                    | true, [0] ->
                                        match aircrafts.[sub] with
                                        | CarriedAircraft.Bomber(BomberTransport.Infantry _, health) ->
                                            let (Health h) = health
                                            if h > 0.0f then
                                                yield CarriedAircraft.Bomber(BomberTransport.Bombs 0, health)
                                        | CarriedAircraft.Bomber(BomberTransport.Bombs _, _) ->
                                            failwith "Bad UnitIndex: Can't remove content from a bomber with bombs"
                                        | CarriedAircraft.Fighter _ ->
                                            failwith "Bad UnitIndex: Fighters can't transport anything"
                                    | true, idxs ->
                                        failwith <| sprintf "Bad list of level-2 unit indices %A" idxs
                            |]
                        yield (idx0, { unit with specific =
                                                    UnitTypes.Carrier(docked, aircrafts') })
                    | UnitTypes.Infantry
                    | UnitTypes.AntiAircraft
                    | UnitTypes.Artillery
                    | UnitTypes.Battleship _
                    | UnitTypes.Destroyer _
                    | UnitTypes.Fighter _
                    | UnitTypes.Submarine _
                    | UnitTypes.Tank -> failwith <| sprintf "Bad UnitIndex: %A can't transport units" unit
    |]

let update (gs : GameState) (orders : Order[][]) =
    let damages =
        seq {
            for player in 0 .. gs.player_units.Length - 1 do
                let player_id = PlayerId player
                let attacks = extractAttackOrders gs.player_units.[player] player orders.[player]
                yield! computeDamages gs player attacks |> accumulateDamage                
        }
        |> dict

    let getDamages u =
        match damages.TryGetValue(u) with
        | false, _ -> 0.0f
        | true, x -> x

    let gs = applyDamage gs getDamages

    let dead_units = getUnitDeaths gs damages

    let isDead u =
        dead_units.Contains u

    let player_units =
        [|
            for player in 0 .. gs.player_units.Length - 1 do
                let player_id = PlayerId player
                let units = gs.player_units.[player]
                let orders = orders.[player]
                let dock_orders = extractDockOrders units orders

                let isThisDead u = isDead(player_id, u)
                let move_orders =
                    extractMoveOrders units player orders
                    |> filterDeadMoveOrders isThisDead

                let early_disembark, late_disembark = extractDisembarkOrders units player orders
                let late_disembark = filterDeadDisembarkOrders isDead player late_disembark
                let disembark_orders = Array.concat [early_disembark ; late_disembark]

                let embark_orders =
                    extractEmbarkOrders units player orders
                    |> filterDeadEmbarkOrders isDead player
                                
                let units = applyMoves move_orders units

                yield
                    units
                    |> growUnitTree embark_orders disembark_orders
                    |> applyDockOrders dock_orders
                    |> shrinkUnitTree embark_orders disembark_orders
        |]
        |> Array.map Array.unzip

    let old_idx_maps =
        player_units
        |> Array.map (fst >> Array.choose id >> Array.mapi (fun i idx0 -> (idx0, i)) >> dict)
        
    let getRootFromOldIdx (PlayerId player) idx =
        match old_idx_maps.[player].TryGetValue(idx) with
        | false, _ -> None
        | true, i -> Some (Root i)

    let gs = { gs with player_units = Array.map snd player_units }

    let gs =
        seq { 0 .. gs.player_units.Length - 1 }
        |> Seq.fold (fun resources_of player ->
            let player_id = PlayerId player
            let conquests = extractConquests player orders.[player]            
            conquests
            |> Array.fold (fun resources_of (rsc, pos) -> captureResource pos rsc player_id resources_of)
                          resources_of
           )
           gs.resources_of
        |> redict gs

    gs, getRootFromOldIdx