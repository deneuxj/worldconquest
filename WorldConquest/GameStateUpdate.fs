module GameStateUpdate

open GameState
open Orders
open MoveOrders
open EmbarkOrders
open AttackOrders
open Units

let growUnitVector (embark : EmbarkOrder[]) (disembark : DisembarkOrder[]) (units : UnitInfo[]) =
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
                    yield { destination_unit with
                                specific = UnitTypes.Transport(docked, Array.concat [transported; extra_transported]) }

                | UnitTypes.Bomber(landed, fuel, transported) as bomber ->
                    match List.ofSeq orders with
                    | [] -> yield destination_unit
                    | [{ unit = unit }] ->
                        match transported, getUnitByIndex units unit with
                        | BomberTransport.Bombs _, { health = health; specific = Infantry } ->
                            yield { destination_unit with
                                        specific = UnitTypes.Bomber(landed, fuel, BomberTransport.Infantry(Health health)) }
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
                    yield { destination_unit with
                                specific = UnitTypes.Carrier(docked, Array.concat [transported; extra_transported]) }

                | UnitTypes.Infantry
                | UnitTypes.Tank
                | UnitTypes.Artillery
                | UnitTypes.AntiAircraft
                | UnitTypes.Battleship _
                | UnitTypes.Destroyer _
                | UnitTypes.Submarine _
                | UnitTypes.Fighter _ -> failwith <| sprintf "Cannot load units into %A" units.[i]

            | false, _ -> yield units.[i]

        for Disembark(index, path) in disembark do
            let unit = getUnitByIndex units index
            let coords = path |> List.rev |> List.head
            yield { unit with coords = coords }
    |]