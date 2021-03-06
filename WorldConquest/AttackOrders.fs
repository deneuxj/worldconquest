﻿module AttackOrders

open System.Collections.Generic

open GameState
open Units
open HexTiling
open Orders

type AttackType =
    | Remote  // The attacker is safe
    | Melee   // One of the defenders (the most dangerous one) will strike back


type AttackOrder =
    {  player : PlayerId
       unit : UnitIndex
       coords : HexCoords
       attack : AttackType  }

let extractAttackOrders (units : UnitInfo[]) (player : int) (orders : Order[]) =
    let getUnitOrder (idx : UnitIndex, u : UnitInfo, order : Order) =
        let coords_and_attack =
            if
                match idx with
                | UnitIndex.Root _ -> true
                | UnitIndex.Transported (idx, _) ->
                    match u.specific, units.[idx].specific with
                    | AirUnit, Carrier _ -> true
                    | _ -> false
                | _ -> false
            then
                match order with
                | Order.Bomb (coords, _)
                | Order.Bombard coords -> Some (coords, Remote)
                | Order.DirectAttack (coords, _) -> Some (coords, Melee)
                | _ -> None
            else
                None

        match coords_and_attack with
        | Some (coords, attack) ->
            [|
                {  player = PlayerId player;
                    unit = idx;
                    coords = coords;
                    attack = attack  }
            |]
        | None -> Array.empty

    playerUnitZipMap getUnitOrder units orders
    |> Array.concat


let computeDamages (gs : GameState) (player : int) (orders : AttackOrder[]) =

    let enemyUnits =
        [|
            for i in 0..gs.player_units.Length - 1 do
                let victim = PlayerId i
                if i <> player then
                    for i2 in 0..gs.player_units.[i].Length - 1 do
                        let u = gs.player_units.[i].[i2]
                        yield victim, i2, u.coords
        |]
        |> Seq.groupBy (fun (victim, i, coords) -> coords)
        |> dict

    let damages_to_victims =
        [
            for attack in orders do
                match enemyUnits.TryGetValue(attack.coords) with
                | true, units ->                
                    let attacker = getUnitByIndex gs.player_units.[player] attack.unit
                    for (PlayerId victim_player) as vp_id, victim_idx, _ in units do
                        let victim_unit = gs.player_units.[victim_player].[victim_idx]
                        let damage =
                            match attacker.specific, victim_unit.specific, attack.attack with
                            | Infantry, Infantry, Melee
                            | Tank, Tank, Melee
                            | Tank, Destroyer(Docked.Docked), Melee
                            | Tank, Battleship(Docked.Docked), Melee
                            | Artillery, Artillery, Melee
                            | Artillery, AntiAircraft, Melee
                            | Artillery, Battleship _, Remote
                            | AntiAircraft, Artillery, Melee
                            | AntiAircraft, AntiAircraft, Melee
                            | Destroyer _, Destroyer _, Melee
                            | Destroyer _, Battleship _, Melee
                            | Destroyer _, Carrier _, Melee
                            | Submarine _, Submarine _, Melee
                            | Battleship _, Battleship _, _
                            | Carrier _, Carrier _, Melee
                            | Fighter _, AntiAircraft, Melee
                            | Fighter _, Destroyer _, Melee
                            | Fighter _, Submarine _, Melee
                            | Fighter _, Battleship _, Melee
                            | Fighter _, Carrier _, Melee
                            | Fighter _, Fighter(Airborne, _), Melee
                            | Bomber _, Bomber(Airborne, _, _), Melee -> base_damage

                            | BomberWithBombs _, Transport _, Remote -> base_damage + 1.0f
                            | BomberWithBombs _, AirUnit, Remote -> 0.0f
                            | BomberWithBombs _, _, Remote -> base_damage

                            | Infantry, Tank, Melee
                            | Infantry, Submarine(Docked.Docked, _), Melee
                            | Infantry, Carrier(Docked.Docked, _), Melee
                            | Infantry, Fighter(Landed.Landed, _), Melee
                            | Infantry, Bomber(Landed.Landed, _, _), Melee
                            | Tank, Infantry, Melee
                            | Tank, Submarine(Docked.Docked, _), Melee
                            | Tank, Carrier(Docked.Docked, _), Melee
                            | Tank, Fighter(Landed.Landed, _), Melee
                            | Tank, Bomber(Landed.Landed, _, _), Melee
                            | Artillery, Transport(Docked.Docked, _), Melee
                            | Artillery, Carrier(Docked.Docked, _), _
                            | Artillery, Fighter(Landed.Landed, _), Melee
                            | Artillery, Bomber(Landed.Landed, _, _), Melee
                            | Artillery, Tank, Remote
                            | Artillery, Destroyer _, Remote
                            | Artillery, Carrier _, Remote
                            | AntiAircraft, Transport(Docked.Docked, _), Melee
                            | AntiAircraft, Submarine(Docked.Docked, _), Melee
                            | AntiAircraft, Carrier(Docked.Docked, _), Melee
                            | AntiAircraft, Fighter(Landed.Landed, _), Melee
                            | AntiAircraft, Bomber(Landed.Landed, _, _), Melee
                            | Destroyer _, Fighter(Airborne, _), Melee
                            | Destroyer _, Bomber(Airborne, _, _), Melee
                            | Submarine(_, _), Battleship _, Melee
                            | Submarine(_, _), Carrier _, Melee
                            | Battleship _, Submarine _, Melee
                            | Battleship _, Carrier _, Melee
                            | Battleship _, Tank, Remote
                            | Battleship _, Destroyer _, Remote
                            | Battleship _, Carrier _, Remote
                            | Carrier _, Transport _, Melee
                            | Fighter _, LandUnit, Melee
                            | Fighter _, Bomber(Airborne, _, _), Melee
                            | Bomber _, Destroyer _, Melee
                            | Bomber _, Battleship _, Melee
                            | Bomber _, Carrier _, Melee
                            | Bomber _, Submarine _, Melee -> base_damage + 1.0f

                            | Infantry, Artillery, Melee
                            | Infantry, AntiAircraft, Melee
                            | Infantry, Transport(Docked.Docked, _), Melee
                            | Tank, Artillery, Melee
                            | Tank, AntiAircraft, Melee
                            | Tank, Transport(Docked.Docked, _), Melee
                            | Artillery, LandUnit, Remote
                            | Artillery, Transport _ , Remote
                            | Artillery, Landed, Remote
                            | AntiAircraft, AirUnit, Remote
                            | _, Transport _, _
                            | Destroyer _, Submarine _, Melee
                            | Battleship _, Destroyer _, Melee
                            | Battleship _, _, Remote
                            | AirUnit, Landed, Melee
                            | Submarine _, Transport _, Melee -> base_damage + 2.0f

                            | Artillery, Infantry, Melee
                            | Artillery, Tank, Melee
                            | Artillery, Destroyer(Docked.Docked), Melee
                            | Artillery, Battleship(Docked.Docked), Melee
                            | AntiAircraft, Infantry, Melee
                            | AntiAircraft, Tank, Melee
                            | AntiAircraft, Destroyer(Docked.Docked), Melee
                            | AntiAircraft, Battleship(Docked.Docked), Melee
                            | Submarine(_, NotStealthy), Destroyer _, Melee
                            | Carrier _, Destroyer _, Melee
                            | Carrier _, Submarine _, Melee
                            | Bomber _, AntiAircraft, Melee
                            | Bomber _, Fighter(Airborne, _), Melee
                            | Infantry, Destroyer(Docked.Docked), Melee
                            | Infantry, Battleship(Docked.Docked), Melee -> base_damage - 1.0f

                            | Carrier _, Battleship _, Melee -> base_damage - 2.0f

                            | _ -> 0.0f

                        if damage > 0.0f then
                            let health_points = getHealthPoints victim_unit.specific
                            yield (vp_id, victim_idx, damage / health_points)

                | false, _ -> ()
        ]

    let damages_to_attackers =
        [
            for attack in orders do
                let relative_damage =
                    [
                        match enemyUnits.TryGetValue(attack.coords) with
                        | true, units ->                
                            let attacker = getUnitByIndex gs.player_units.[player] attack.unit
                            let health_points = getHealthPoints attacker.specific
                            for (PlayerId victim_player) as vp_id, victim_idx, _ in units do
                                let victim_unit = gs.player_units.[victim_player].[victim_idx]
                                yield
                                    match attacker.specific, victim_unit.specific, attack.attack with
                                    // The victim cannot fire back
                                    | Transport _, _, _
                                    | SeaUnit, LandUnit, Melee
                                    | Docked, LandUnit, Melee
                                    | LandUnit, SeaUnit, Melee
                                    | AntiAircraft, LandUnit, Remote
                                    | AntiAircraft, Landed, Remote
                                    | AntiAircraft, SeaUnit, Remote
                                    | AntiAircraft, Docked, Remote
                                    | _, _, Remote
                                    | _, Transport _, _
                                    | _, Landed, _ -> 0.0f
                            
                                    // The victim unit inflicts extra damage
                                    | Tank, Destroyer(Docked.Docked), Melee
                                    | Tank, Battleship(Docked.Docked), Melee
                                    | Artillery, Destroyer(Docked.Docked), Melee
                                    | Artillery, Battleship(Docked.Docked), Melee
                                    | AntiAircraft, Destroyer(Docked.Docked), Melee
                                    | AntiAircraft, Battleship(Docked.Docked), Melee
                                    | Submarine(_, Stealthy.NotStealthy), Destroyer _, Melee
                                    | Carrier _, SeaUnit, Melee
                                    | Carrier _, Docked, Melee
                                    | Fighter _, AntiAircraft, Melee
                                    | Bomber _, AntiAircraft, Melee
                                    | Bomber _, Fighter(Landed.Airborne, _), Melee -> (base_damage + 1.0f) / health_points

                                    // Default case: normal damage
                                    | _, _, _ -> base_damage / health_points

                        | false, _ -> yield 0.0f
                    ]
                    |> List.max

                if relative_damage > 0.0f then
                    yield (PlayerId player, attack.unit, relative_damage)
        ]

    damages_to_victims, damages_to_attackers


let accumulateDamage (damages_to_victims, damages_to_attackers) =
    let damages_to_victims =
        damages_to_victims
        |> List.map (fun (id : PlayerId, idx, damage) -> (id, Root idx, damage))

    Seq.concat [damages_to_victims; damages_to_attackers]
    |> Seq.groupBy (fun (id, idx, _) -> (id, idx))
    |> Seq.map (fun ((id, idx), damages) -> ((id, idx), Seq.sumBy (fun (_, _, damage : float32) -> damage) damages))


let getUnitDeaths (gs : GameState) (accumulated_damages : IDictionary<PlayerId * UnitIndex, float32>) =
    accumulated_damages
    |> Seq.map (fun kvp -> (kvp.Key, kvp.Value))
    |> Seq.filter (fun ((PlayerId id, idx), damage) -> (getUnitByIndex (gs.player_units.[id]) idx).health <= damage)
    |> Seq.map fst
    |> Set.ofSeq


let applyDamage (gs : GameState) (getDamage : (PlayerId * UnitIndex) -> float32) =
    { gs with
        player_units =
            gs.player_units
            |> Array.mapi (fun player units ->
                let player = PlayerId player
                units
                |> Array.mapi (fun root unit ->
                        let unit =
                            match getDamage(player, Root root) with
                            | 0.0f -> unit
                            | x -> { unit with health = unit.health - x }
                        match unit.specific with
                        | UnitTypes.AntiAircraft
                        | UnitTypes.Artillery
                        | UnitTypes.Battleship _
                        | UnitTypes.Destroyer _
                        | UnitTypes.Fighter _
                        | UnitTypes.Infantry
                        | UnitTypes.Submarine _
                        | UnitTypes.Bomber(_, _, BomberTransport.Bombs _)
                        | UnitTypes.Tank -> unit

                        | UnitTypes.Transport(docked, transported) ->
                            { unit with
                                specific =
                                UnitTypes.Transport(docked,
                                    transported
                                    |> Array.mapi (fun sub t ->
                                        let idx = Transported(root, sub)
                                        let x = getDamage(player, idx)
                                        if x = 0.0f then
                                            t
                                        else
                                            match t with
                                            | TransportedUnit.AntiAircraft (Health h) ->
                                                TransportedUnit.AntiAircraft (Health (h - x))
                                            | TransportedUnit.Artillery (Health h) ->
                                                TransportedUnit.AntiAircraft (Health (h - x))
                                            | TransportedUnit.Infantry (Health h) ->
                                                TransportedUnit.Infantry (Health (h - x))
                                            | TransportedUnit.Tank (Health h) ->
                                                TransportedUnit.Tank (Health (h - x))
                                    )
                                )
                            }

                        | UnitTypes.Bomber(landed, fuel, BomberTransport.Infantry(Health h)) ->
                            match getDamage(player, Transported(root, 0)) with
                            | 0.0f -> unit
                            | x -> { unit with
                                        specific =
                                        UnitTypes.Bomber(landed, fuel, BomberTransport.Infantry(Health(h - x))) }

                        | UnitTypes.Carrier(docked, aircrafts) ->
                            { unit with
                                specific =
                                UnitTypes.Carrier(docked,
                                    aircrafts
                                    |> Array.mapi (fun sub t ->
                                        let idx = Transported(root, sub)
                                        let x = getDamage(player, idx)
                                        match t with
                                        | CarriedAircraft.Fighter (Health h) ->
                                            CarriedAircraft.Fighter(Health(h - x))
                                        | CarriedAircraft.Bomber (BomberTransport.Bombs _ as bombs, Health h) ->
                                            CarriedAircraft.Bomber(bombs, Health(h - x))
                                        | CarriedAircraft.Bomber (BomberTransport.Infantry(Health h2), Health h) ->
                                            let x2 = getDamage(player, Transported2(root, sub, 0))
                                            CarriedAircraft.Bomber(BomberTransport.Infantry(Health(h2 - x2)), Health(h - x))
                                    )
                                )
                            }
                    )
            )
    }