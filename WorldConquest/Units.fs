// Learn more about F# at http://fsharp.net

module Units

type BomberTransport =
    | Bombs of int
    | Infantries of int

type CarriedAircraft =
    | Fighter of float32 // health
    | Bomber of BomberTransport * float32  // health

type LiftedInfantry = { health : float32 }

type UnitTypes =
    | Infantry
    | Tank
    | Artillery
    | AntiAircraft
    | Transport of bool * LiftedInfantry list // docked
    | Destroyer of bool // docked
    | Submarine of bool * bool // docked, stealthy
    | Battleship of bool // docked
    | Carrier of bool * CarriedAircraft list // docked
    | Fighter of bool * int // landed, fuel
    | Bomber of bool * int * BomberTransport // landed, fuel

let (|LandUnit|SeaUnit|AirUnit|DockedOrLanded|) u =
    match u with
    | Infantry | Tank | Artillery | AntiAircraft -> LandUnit
    | Transport(true, _)
    | Destroyer(true)
    | Submarine(true, _)
    | Battleship(true)
    | Carrier(true, _)
    | Fighter(true, _)
    | Bomber(true, _, _) -> DockedOrLanded
    | Transport _ | Destroyer _ | Submarine _ | Battleship _ | Carrier _ -> SeaUnit
    | Fighter _ | Bomber _ -> AirUnit

type Order =
    | Sleep
    | Goto of int * int // x, y

type UnitInfo =
    {  x : int
       y : int
       health : float32
       moves : int
       order : Order option
       specific : UnitTypes  }
with
    member x.IsDead =
        x.health <= 0.0f
        ||
        match x.specific with
        | Fighter(_, fuel) | Bomber (_, fuel, _) -> fuel < 0
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
        | DockedOrLanded -> false
        | _ -> true
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