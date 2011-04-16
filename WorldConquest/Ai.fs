module Ai

open GameState
open HexTiling
open Units


let getValidOrders (state : GameState) (player : int) =
    let width = state.terrain.GetLength(0)
    let getUnitOrders = getOrder state player
    [|
        let getUnitOrders u =
            let destinations =
                getAllWithin (getMovementRange u.specific)
                |> Array.filter (fun (HexCoords(_, y)) -> y >= 0 && y < width)
                |> Array.map (fromHex >> wrapX width >> toHex)
            destinations
            |> Array.map (getUnitOrders u)
            |> List.concat

        for u in state.player_units.[player] do
            yield! getUnitOrders u

            match u.specific with
            | Transport(_, transported) ->
                for t in transported do
                    let unit_type =
                        match t with
                        | TransportedUnit.Infantry _ -> Infantry
                        | TransportedUnit.Tank _ -> Tank
                        | TransportedUnit.Artillery _ -> Artillery
                        | TransportedUnit.AntiAircraft _ -> AntiAircraft
                    let u' =
                        {  coords = u.coords;
                           health = t.Health;
                           moves = getMovementRange unit_type;
                           specific = unit_type  }
                    yield! getUnitOrders u' |> List.filter (function Move _ -> true | _ -> false)
            | Bomber(_, _, BomberTransport.Infantry(Health h)) ->
                let u' =
                    {  coords = u.coords;
                       health = h;
                       moves = getMovementRange Infantry
                       specific = Infantry  }
                yield! getUnitOrders u'
            | Carrier(_, aircrafts) ->
                for plane in aircrafts do
                    let unit_type =
                        match plane with
                        | CarriedAircraft.Fighter _ -> Fighter(Landed, Fuel fighter_fuel_range)
                        | CarriedAircraft.Bomber (transported, _) -> Bomber(Landed, Fuel bomber_fuel_range, transported)
                    let u' =
                        {  coords = u.coords;
                           health = plane.Health;
                           moves = getMovementRange unit_type;
                           specific = unit_type  }
                    yield! getUnitOrders u'
            | _ -> ()
    |]

// Position evaluation functions

// Health
let evalHealth (gs : GameState) (player : int) =
    let infantry_value = 1.0f
    let tank_value = 2.0f
    let artillery_value = 3.0f
    let anti_aircraft_value = 3.0f
    let transport_value = 3.0f
    let destroyer_value = 5.0f
    let submarine_value = 5.0f
    let battleship_value = 10.0f
    let carrier_value = 25.0f
    let fighter_value = 5.0f
    let bomber_value = 5.0f
    let bomb_value = 0.5f

    let evalBomberValue (transported) =
        let transported_value =
            match transported with
            | BomberTransport.Bombs n -> (float32 n) * bomb_value
            | BomberTransport.Infantry (Health h) -> (1.0f + h) * infantry_value
        bomber_value + transported_value

    gs.player_units.[player]
    |> Array.sumBy (fun u ->
        (1.0f + u.health)
        *
        match u.specific with
        | Infantry -> infantry_value
        | Tank -> tank_value
        | Artillery -> artillery_value
        | AntiAircraft -> anti_aircraft_value
        | Transport (_, transported) ->
            let transported_value =
                transported
                |> List.sumBy
                    (function
                     | TransportedUnit.AntiAircraft (Health h) -> (1.0f + h) * anti_aircraft_value
                     | TransportedUnit.Artillery (Health h) -> (1.0f + h) * artillery_value
                     | TransportedUnit.Infantry (Health h) -> (1.0f + h) * infantry_value
                     | TransportedUnit.Tank (Health h) -> (1.0f + h) * tank_value)
            transport_value + transported_value
        | Destroyer _ -> destroyer_value
        | Submarine _ -> submarine_value
        | Battleship _ -> battleship_value
        | Carrier (_, transported) ->
            let transported_value =
                transported
                |> List.sumBy
                    (function
                     | CarriedAircraft.Fighter (Health h) -> (1.0f + h) * fighter_value
                     | CarriedAircraft.Bomber (transported, Health h) -> (1.0f + h) * evalBomberValue(transported))
            carrier_value + transported_value
        | Fighter _ -> fighter_value
        | Bomber (_, _, transported) -> evalBomberValue(transported)
        )

// Resources
let evalResources (gs : GameState) (player : int) =
    let counts = 
        gs.getResourcesOf (PlayerId player)
        |> Seq.groupBy (fun (_, rsc) -> rsc)
        |> Seq.map (fun (_, items) -> Seq.length items)
        |> Array.ofSeq

    let min = Array.min counts
    let max =
        counts
        |> Array.maxBy(fun n -> if n > min + 1 then -1 else n)

    max
    |> float32

// Being in range of enemy units

// Enemy units in range