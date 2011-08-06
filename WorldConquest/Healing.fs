module Healing

open Orders
open Units

let healUnit (u : UnitInfo) order =
    match order with
    | DoNothing ->
        match u.specific with
        | Tank
        | AntiAircraft
        | Transport(Docked.Docked, _)
        | Fighter(Landed.Landed, _)
        | Bomber(Landed.Landed, _, _)
        | Destroyer(Docked.Docked)
        | Battleship(Docked.Docked)
        | Submarine(Docked.Docked, _)
        | Artillery -> { u with health = min 1.0f (u.health + 1.0f / (Units.getHealthPoints u.specific)) }

        | Infantry -> let bonus = 1.5f in { u with health = min 1.0f (u.health + bonus / (Units.getHealthPoints u.specific)) }

        | _ -> u
    | _ -> u