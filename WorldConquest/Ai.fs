module Ai

open GameState
open HexTiling
open Units


let getValidOrders (state : GameState) (player : int) =
    let width = state.terrain.GetLength(0)
    let getUnitOrders = getOrder state player
    [|
        let getUnitOrders u = [|
            let destinations =
                getAllWithin (getMovementRange u.specific)
                |> Array.filter (fun (HexCoords(_, y)) -> y >= 0 && y < width)
                |> Array.map (fromHex >> wrapX width >> toHex)
            yield! destinations |> Array.map (getUnitOrders u)
        |]

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
                    yield! getUnitOrders u'
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