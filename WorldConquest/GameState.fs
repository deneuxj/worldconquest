module GameState

open System.Collections.Generic

open Units
open HexTiling
open Resource
open Terrain

type PlayerId = PlayerId of int

type GameState =
    {  terrain : Terrain[,]
       resource_at : IDictionary<HexCoords, (Resource * PlayerId option)>
       resources_of : (HexCoords * Resource) list []
       player_units : UnitInfo[][]  }
with
    member x.getResourceAt pos =
        match x.resource_at.TryGetValue(pos) with
        | true, res -> Some res
        | false, _ -> None

type UnitIndex =
    | Root of int
    | Transported of int * int // A unit inside another unit
    | Transported2 of int * int * int // A unit inside a unit which is itself inside another unit.


let getUnitByIndex (units : UnitInfo[]) (idx : UnitIndex) =
        match idx with
        | Root idx -> units.[idx]
        | Transported (root, idx) ->
            let transport = units.[root]
            match transport.specific with
            | Transport(_, units) ->
                match units.[idx] with
                | TransportedUnit.AntiAircraft (Health health) ->
                    { coords = transport.coords ;
                      health = health ;
                      moves = anti_aircraft_range ;
                      specific = AntiAircraft }
                | TransportedUnit.Artillery (Health health) ->
                    { coords = transport.coords ;
                      health = health ;
                      moves = artillery_range ;
                      specific = Artillery }
                | TransportedUnit.Infantry (Health health) ->
                    { coords = transport.coords ;
                      health = health ;
                      moves = infantry_range ;
                      specific = Infantry }
                | TransportedUnit.Tank (Health health) ->
                    { coords = transport.coords ;
                      health = health ;
                      moves = tank_range ;
                      specific = Tank }
            | Carrier (_, units) ->
                match units.[idx] with
                | CarriedAircraft.Bomber (lifted, Health health) ->
                    { coords = transport.coords ;
                      health = health ;
                      moves = bomber_range ;
                      specific = Bomber(Airborne, Fuel bomber_fuel_range, lifted) }
                | CarriedAircraft.Fighter (Health health) ->
                    { coords = transport.coords ;
                      health = health ;
                      moves = fighter_range ;
                      specific = Fighter(Airborne, Fuel fighter_fuel_range) }
            | _ -> failwith <| sprintf "Incorrect unit index: root unit is %A, should be a carrier or a transport" transport.specific
        | Transported2 (root, bomber, idx) ->
            let carrier = units.[root]
            match carrier.specific with
            | Carrier (_, aircrafts) ->
                match aircrafts.[bomber] with
                | CarriedAircraft.Bomber (lifted, _) ->
                    if idx = 0 then
                        match lifted with
                        | BomberTransport.Infantry (Health health) ->
                            { coords = carrier.coords ;
                              health = health ;
                              moves = infantry_range ;
                              specific = Infantry }
                        | BomberTransport.Bombs _ -> failwith "Incorrect unit index: the bomber on the carrier is transporting bombs"
                    else
                        failwith "Incorrect unit index: Bombers can only transport one infantry each"
                | CarriedAircraft.Fighter _ -> failwith "Incorrect unit index: the fighter (which is on a carrier) cannot transport units"
            | _ -> failwith <| sprintf "Incorrect unit index: the transporter is %A, expected a carrier" carrier
