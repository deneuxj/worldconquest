// Learn more about F# at http://fsharp.net

module Units

open HexTiling
 
type Health = Health of float32

type Fuel = Fuel of int

type BomberTransport =
    | Bombs of int
    | Infantry of Health

type CarriedAircraft =
    | Fighter of Health
    | Bomber of BomberTransport * Health

type TransportedUnit =
    | Infantry of Health
    | Tank of Health
    | Artillery of Health
    | AntiAircraft of Health

type Docked = Docked | NotDocked

type Stealthy = Stealthy | NotStealthy

type Landed = Landed | Airborne

type UnitTypes =
    | Infantry
    | Tank
    | Artillery
    | AntiAircraft
    | Transport of Docked * TransportedUnit list
    | Destroyer of Docked
    | Submarine of Docked * Stealthy
    | Battleship of Docked
    | Carrier of Docked * CarriedAircraft list
    | Fighter of Landed * Fuel
    | Bomber of Landed * Fuel * BomberTransport

let getBombardRange = function
    | Infantry
    | Tank
    | Transport _
    | Destroyer _
    | Submarine _
    | Carrier _
    | Fighter _
    | Bomber _ -> 0
    | Artillery -> 4
    | AntiAircraft -> 4
    | Battleship _ -> 4

let getMovementRange = function
    | Infantry -> 1
    | Tank -> 2
    | Transport _ -> 2
    | Destroyer _ -> 4
    | Submarine _ -> 3
    | Carrier _ -> 3
    | Fighter _ -> 9
    | Bomber _ -> 6
    | Artillery -> 1
    | AntiAircraft -> 1
    | Battleship _ -> 3

let (|LandUnit|SeaUnit|AirUnit|Docked|Landed|) u =
    match u with
    | Infantry | Tank | Artillery | AntiAircraft -> LandUnit
    | Transport(Docked, _)
    | Destroyer(Docked)
    | Submarine(Docked, _)
    | Battleship(Docked)
    | Carrier(Docked, _) -> Docked
    | Fighter(Landed, _)
    | Bomber(Landed, _, _) -> Landed
    | Transport _ | Destroyer _ | Submarine _ | Battleship _ | Carrier _ -> SeaUnit
    | Fighter _ | Bomber _ -> AirUnit


type UnitInfo =
    {  coords : HexCoords
       health : float32
       moves : int
       specific : UnitTypes  }
with
    member x.IsDead =
        x.health <= 0.0f
        ||
        match x.specific with
        | Fighter(_, Fuel fuel) | Bomber (_, Fuel fuel, _) -> fuel < 0
        | Infantry _
        | Tank _
        | Artillery _
        | AntiAircraft _
        | Transport _
        | Destroyer _
        | Submarine _
        | Battleship _
        | Carrier _ -> false

let canDirectAttack (attacker : UnitInfo, defender : UnitInfo) =
    match attacker.specific with
    | LandUnit ->
        match defender.specific with
        | Docked | Landed | LandUnit -> true
        | AirUnit | SeaUnit -> false
    | Transport _ | Docked | Landed -> false
    | SeaUnit ->
        match defender.specific with
        | SeaUnit -> true
        | LandUnit | AirUnit | Docked | Landed -> false
    | AirUnit -> true

let canBombard (attacker : UnitInfo, target : UnitInfo) =
    match attacker.specific with
    | Artillery _ ->
        match target.specific with
        | Submarine _
        | AirUnit -> false
        | LandUnit
        | SeaUnit
        | Docked
        | Landed -> true
    | AntiAircraft _ ->
        match target.specific with
        | AirUnit -> true
        | LandUnit
        | SeaUnit
        | Docked
        | Landed -> false
    | Battleship _ ->
        match target.specific with
        | Submarine _
        | AirUnit -> false
        | LandUnit
        | SeaUnit
        | Docked
        | Landed -> true
    | LandUnit
    | SeaUnit
    | AirUnit
    | Docked
    | Landed -> false