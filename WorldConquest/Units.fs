// Learn more about F# at http://fsharp.net

module Units

open HexTiling

type Health = Health of float32

type Fuel = Fuel of int

type BomberTransport =
    | Bombs of int
    | Infantries of int

type CarriedAircraft =
    | Fighter of Health
    | Bomber of BomberTransport * Health

type LiftedInfantry = { health : float32 }

type Docked = Docked | NotDocked

type Stealthy = Stealthy | NotStealthy

type Landed = Landed | Airborne

type UnitTypes =
    | Infantry
    | Tank
    | Artillery
    | AntiAircraft
    | Transport of Docked * LiftedInfantry list
    | Destroyer of Docked
    | Submarine of Docked * Stealthy
    | Battleship of Docked
    | Carrier of Docked * CarriedAircraft list
    | Fighter of Landed * Fuel
    | Bomber of Landed * Fuel * BomberTransport

let (|LandUnit|SeaUnit|AirUnit|DockedOrLanded|) u =
    match u with
    | Infantry | Tank | Artillery | AntiAircraft -> LandUnit
    | Transport(Docked, _)
    | Destroyer(Docked)
    | Submarine(Docked, _)
    | Battleship(Docked)
    | Carrier(Docked, _)
    | Fighter(Landed, _)
    | Bomber(Landed, _, _) -> DockedOrLanded
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
        | DockedOrLanded | LandUnit -> true
        | AirUnit | SeaUnit -> false
    | Transport _ | DockedOrLanded -> false
    | SeaUnit ->
        match defender.specific with
        | SeaUnit -> true
        | LandUnit | AirUnit | DockedOrLanded -> false
    | AirUnit -> true

let canBombard (attacker : UnitInfo, target : UnitInfo) =
    match attacker.specific with
    | Artillery _ ->
        match target.specific with
        | Submarine _
        | AirUnit -> false
        | LandUnit
        | SeaUnit
        | DockedOrLanded -> true
    | AntiAircraft _ ->
        match target.specific with
        | AirUnit -> true
        | LandUnit
        | SeaUnit
        | DockedOrLanded -> false
    | Battleship _ ->
        match target.specific with
        | Submarine _
        | AirUnit -> false
        | LandUnit
        | SeaUnit
        | DockedOrLanded -> true
    | LandUnit
    | SeaUnit
    | AirUnit
    | DockedOrLanded -> false