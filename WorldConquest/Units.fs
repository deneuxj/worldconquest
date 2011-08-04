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
with
    member x.Health =
        match x with
        | Fighter (Health h) | Bomber (_, Health h) -> h

type TransportedUnit =
    | Infantry of Health
    | Tank of Health
    | Artillery of Health
    | AntiAircraft of Health
with
    member x.Health =
        match x with
        | Infantry (Health h) | Tank (Health h) | Artillery (Health h) | AntiAircraft (Health h) -> h

type Docked = Docked | NotDocked

type Stealthy = Stealthy | NotStealthy

type Landed = Landed | Airborne

type UnitTypes =
    | Infantry
    | Tank
    | Artillery
    | AntiAircraft
    | Transport of Docked * TransportedUnit[]
    | Destroyer of Docked
    | Submarine of Docked * Stealthy
    | Battleship of Docked
    | Carrier of Docked * CarriedAircraft[]
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
    | Bomber _ -> -1
    | Artillery -> 4
    | AntiAircraft -> 4
    | Battleship _ -> 4

// Movement ranges
let infantry_range = 1
let tank_range = 2
let transport_range = 2
let destroyer_range = 4
let submarine_range = 3
let carrier_range = 3
let fighter_range = 9
let bomber_range = 6
let artillery_range = 1
let anti_aircraft_range = 1
let battleship_range = 3

let getMovementRange = function
    | Infantry -> infantry_range
    | Tank -> tank_range
    | Transport _ -> transport_range
    | Destroyer _ -> destroyer_range
    | Submarine _ -> submarine_range
    | Carrier _ -> carrier_range
    | Fighter _ -> fighter_range
    | Bomber _ -> bomber_range
    | Artillery -> artillery_range
    | AntiAircraft -> anti_aircraft_range
    | Battleship _ -> battleship_range

let getHealth = function
    | Infantry -> 1.0f
    | Tank -> 2.0f
    | Transport _ -> 1.0f
    | Destroyer _ -> 5.0f
    | Submarine _ -> 2.0f
    | Carrier _ -> 3.0f
    | Fighter _ -> 2.0f
    | Bomber _ -> 1.0f
    | Artillery -> 1.0f
    | AntiAircraft -> 1.0f
    | Battleship _ -> 7.0f

// Production costs
let infantry_cost = 2
let tank_cost = 5
let transport_cost = 10
let destroyer_cost = 15
let submarine_cost = 15
let carrier_cost = 50
let fighter_cost = 5
let bomber_cost = 7
let artillery_cost = 5
let anti_aircraft_cost = 5
let battleship_cost = 30

// Health points. Basic damage per hit is 3.0f
let base_damage = 3.0f

let infantry_hp = base_damage
let tank_hp = 2.0f * base_damage
let transport_hp = base_damage
let destroyer_hp = 3.0f * base_damage
let submarine_hp = 2.0f * base_damage
let carrier_hp = 5.0f * base_damage
let fighter_hp = 2.0f * base_damage
let bomber_hp = base_damage
let artillery_hp = base_damage
let anti_aircraft_hp = base_damage
let battleship_hp = 7.0f * base_damage

let getHealthPoints = function
    | Infantry -> infantry_hp
    | Tank -> tank_hp
    | Transport _ -> transport_hp
    | Destroyer _ -> destroyer_hp
    | Submarine _ -> submarine_hp
    | Carrier _ -> carrier_hp
    | Fighter _ -> fighter_hp
    | Bomber _ -> bomber_hp
    | Artillery -> artillery_hp
    | AntiAircraft -> anti_aircraft_hp
    | Battleship _ -> battleship_hp

// Number of turns aircrafts can be airborne.
let fighter_fuel_range = 2
let bomber_fuel_range = 3

// Number of bombs a bomber can transport
let bomber_max_bombs = 2

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
    | Transport (NotDocked, _) | Destroyer (NotDocked) | Submarine (NotDocked, _) | Battleship (NotDocked) | Carrier (NotDocked, _) -> SeaUnit
    | Fighter (Airborne, _) | Bomber (Airborne, _, _) -> AirUnit

let (|BomberWithBombs|_|) u =
    match u with
    | Bomber(_, _, Bombs n) when n > 0 -> Some n
    | _ -> None

type UnitInfo =
    {  coords : HexCoords
       health : float32
       specific : UnitTypes  }
with
    member x.IsDead =
        x.health <= 0.0f
        ||
        match x.specific with
        | Fighter(Airborne, Fuel fuel) | Bomber (Airborne, Fuel fuel, _) -> fuel < 0
        | Fighter(Landed.Landed, _) | Bomber (Landed.Landed, _, _)
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

let canBomb (attacker : UnitInfo, target : UnitInfo) =
    match attacker.specific with
    | Bomber(Airborne, _, Bombs n) when n > 0 ->
        match target.specific with
        | Submarine _
        | AirUnit -> false
        | LandUnit
        | SeaUnit
        | Docked
        | Landed -> true
    | _ -> false
